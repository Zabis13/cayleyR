#!/usr/bin/env Rscript
# A Q-model for the 3x3x3: one number per move, not one number per state.
#
# Why this rather than another distance estimator. The depth nets stop being
# useful well before they stop being accurate. cube3_dp_m64_h4_b3_d20_n60000_e40_s42
# reaches MAE 2.50, but its mean prediction climbs 0.94 -> 5.60 over depths 1..6
# and then flattens: from depth 10 to 20 it moves 0.83 in total. The `step`
# column of train_cube3_depth.R is where that shows up, and once step nears zero
# a descent has nothing to choose between neighbours. Measured on 20 states at
# depth 10: greedy descent solved 10%, beam width 20 solved 35%.
#
# A descent never needs the absolute distance. It needs the ORDER of the
# neighbours -- which move is better -- and that ordering survives at depths
# where the absolute number is already noise. So the head is widened from 1 to
# 18 and the network is asked the question the search actually poses.
#
# The labelling is the middle-of-the-walk idea from cayleypy-training-core
# (RandomWalkMiddleSampler; PyTorch, Apache 2.0). Only the approach is taken,
# no code. For a point p moves from solved, two of the eighteen moves have
# known labels: the inverse of the move that arrived there steps back, so it is
# worth p-1, and any other move steps away, worth p+1. The remaining sixteen
# are unknown and must not be trained.
#
# generate_walk() already hands back exactly what that needs. A walk of length p
# ends AT the midpoint -- there is nothing to reconstruct and no prefix to
# replay, so the R-loop this seemed to require does not exist:
#
#   previous = inverse of the last move of the word     label p-1
#   next     = any other move                           label p+1
#
# Verified on 200 walks: applying `previous` to the endpoint lands on the
# depth-(p-1) state 200/200 times.
#
# Run with:  Rscript inst/examples/train_cube3_q.R [name=value ...]
#
#   depth=20 states=60000 epochs=40 batch=128
#   d_model=64 heads=4 blocks=3 ff=256 backend=auto seed=42
#
# Predicting/descending with the saved model reads meta.rds for the scaling.

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

# ---------------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------------

defaults <- list(
  d_model = 64L,
  heads   = 4L,
  blocks  = 3L,
  ff      = 256L,
  backend = "auto",
  states  = 60000L,  # training walks (one training example each)
  val     = 4000L,   # held-out walks
  depth   = 20L,     # longest walk; lengths drawn uniformly from 2..depth
  epochs  = 40L,
  batch   = 128L,
  seed    = 42L,
  out     = "/mnt/Data2/DS_projects/333",
  name    = ""
)

opt <- defaults
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1L]]
  if (length(kv) != 2L) stop("argument must be name=value: ", a)
  key <- kv[[1L]]
  if (is.null(defaults[[key]])) {
    stop("unknown parameter: ", key, "\navailable: ",
         paste(names(defaults), collapse = ", "))
  }
  opt[[key]] <- if (is.character(defaults[[key]])) kv[[2L]]
                else if (is.integer(defaults[[key]])) as.integer(kv[[2L]])
                else as.numeric(kv[[2L]])
  if (!is.character(opt[[key]]) && is.na(opt[[key]]))
    stop("not a number: ", a)
}

if (opt$depth < 2L) stop("depth must be at least 2: a midpoint needs a step back")

set.seed(opt$seed)

cat("parameters:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

g   <- cube_group(3)
lay <- cube_piece_layout(g)
mv  <- g$moves
n_a <- length(mv)

cat("group    :", g$name, "--", g$n, "positions,", n_a, "moves\n")
cat("pieces   :", lay$n_piece, "of width", lay$width, "\n")
cat("head     :", n_a, "outputs, one per move\n\n")

# The alphabet's inverse map, as indices. group_inverse_seq of a single move
# names the move that undoes it; doing this once here keeps it out of the
# per-walk work below.
inv_idx <- vapply(seq_len(n_a), function(a) {
  nm <- group_inverse_seq(g, mv[a])[1L]
  w  <- which(mv == nm)
  if (length(w) != 1L) stop("move ", mv[a], " has no inverse in the alphabet")
  w
}, integer(1))

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

# One walk, one training example. exact = FALSE draws the length uniformly, so
# the midpoints spread over the depths instead of piling up at one; walks of
# length 1 are redrawn because p-1 = 0 would label a move as reaching solved
# when the walk only says it returns to the previous point.
#
# no_undo = TRUE matters more here than it does for a depth net. It is what
# makes `next` honest: without it the drawn next move could be the inverse of
# the arriving move, which is `previous`, and the same move would carry both
# p-1 and p+1.
make_data <- function(n, note) {
  cat(sprintf("  %s ...", note)); flush(stdout())
  w <- generate_walk(g, n = n, n_moves = opt$depth,
                     exact = FALSE, no_undo = TRUE)

  # Redraw the length-1 walks rather than dropping them, so `n` means n.
  short <- which(w$depth < 2L)
  while (length(short)) {
    r <- generate_walk(g, n = length(short), n_moves = opt$depth,
                       exact = FALSE, no_undo = TRUE)
    w$states[short, ] <- r$states
    w$depth[short]    <- r$depth
    w$moves[short]    <- r$moves
    short <- short[r$depth < 2L]
  }

  p    <- as.integer(w$depth)
  last <- vapply(w$moves, function(word) {
    i <- which(mv == word[length(word)])
    if (length(i) != 1L) stop("move not in alphabet: ", word[length(word)])
    i
  }, integer(1))

  prev_a <- inv_idx[last]                       # steps back: label p - 1

  # `next` is drawn from the other seventeen. Sampling an offset in 1..n_a-1
  # and rotating past prev_a picks uniformly among them without rejection.
  off    <- sample.int(n_a - 1L, n, replace = TRUE)
  next_a <- ((prev_a - 1L + off) %% n_a) + 1L

  list(states = w$states, p = p, prev_a = prev_a, next_a = next_a)
}

cat("building data\n"); flush(stdout())
t0 <- proc.time()[["elapsed"]]
tr <- make_data(opt$states, "train")
va <- make_data(opt$val,    "val  ")
Xtr <- cayleyR:::adi_encode(tr$states, "transformer", lay)
Xva <- cayleyR:::adi_encode(va$states, "transformer", lay)
cat(sprintf("\r  done, %.0f s%s\n", proc.time()[["elapsed"]] - t0, strrep(" ", 20)))

# Scaled by numbers that follow from `depth` alone, exactly as the depth script
# does, so the predicting side can undo it knowing only that.
y_mu <- (1 + opt$depth) / 2
y_sd <- stats::sd(seq_len(opt$depth))
cat(sprintf("target scaling: mean %.1f, sd %.1f\n", y_mu, y_sd))

# The two labelled coordinates, and the mask that confines the loss to them.
#
# ggmlR takes the mask directly: ggml_fit(sample_weight = ) accepts an
# nrow(x) by ncol(y) matrix for mse, weighting each output element separately
# (ggml_opt_dataset_weights with ne0 = ne_label underneath). The loss stays
# sum(w * (pred - y)^2) / nelements, so zeroed outputs drop out of the
# numerator without renormalising. No second forward pass, no predict-as-target
# trick, no epoch-at-a-time loop -- all of which the plan allowed for on the
# assumption that a per-output mask did not exist.
#
# What the mask does NOT do is isolate the heads completely, and that decides
# the fill value below. Measured on a two-layer net: with a shared trunk the
# untrained coordinates still reach the shared weights through Adam, and the
# value parked on them shows up in the trained head --
#
#   inactive target    0 -> active head MAE 0.027
#   inactive target    3 -> active head MAE 0.080
#   inactive target  100 -> active head MAE 0.201
#
# On a linear net, where the heads share no hidden units, the same comparison
# is bit-identical, which places the effect in the trunk rather than in the
# mask. So the fill is 0 in scaled units -- the mean depth, the blandest value
# available -- and never a wild sentinel.
build_targets <- function(d) {
  n <- length(d$p)
  y <- matrix(0, n, n_a)                       # 0 scaled == mean depth
  w <- matrix(0, n, n_a)
  ip <- cbind(seq_len(n), d$prev_a)
  ix <- cbind(seq_len(n), d$next_a)
  y[ip] <- (d$p - 1L - y_mu) / y_sd
  y[ix] <- (d$p + 1L - y_mu) / y_sd
  w[ip] <- 1
  w[ix] <- 1
  list(y = y, w = w)
}

Ttr <- build_targets(tr)
Tva <- build_targets(va)

cat(sprintf("labels: %d of %d outputs trained per example (%.1f%%)\n\n",
            2L, n_a, 100 * 2 / n_a))

# ---------------------------------------------------------------------------
# The network
# ---------------------------------------------------------------------------

# The same transformer the depth script and ADI use, so the three stay
# comparable on architecture; the only change is the width of the last dense.
# silu rather than gelu inside the blocks: gelu has no backward rule in ggml.
build_net <- function() {
  depth_dim <- lay$n_piece * lay$width
  inp <- ggmlR::ggml_input(shape = c(lay$n_piece, depth_dim))
  h   <- ggmlR::ggml_layer_dense(inp, opt$d_model, time_distributed = TRUE)
  h   <- ggmlR::ggml_layer_positional_embedding(h)
  for (b in seq_len(opt$blocks)) {
    h <- ggmlR::ggml_layer_transformer_block(
      h, opt$d_model, n_heads = opt$heads, ff_dim = opt$ff,
      activation = "silu", norm = "rms", name = paste0("block", b))
  }
  h   <- ggmlR::ggml_layer_sequence_pooling(h, mode = "mean")
  out <- ggmlR::ggml_layer_dense(h, n_a)
  ggmlR::ggml_compile(ggmlR::ggml_model(inputs = inp, outputs = out),
                      optimizer = "adam", loss = "mse", metrics = NULL,
                      backend = opt$backend)
}

cat(sprintf("training %d epochs on %d walks\n\n", opt$epochs, opt$states))
flush(stdout())

# ggml_fit RETURNS the trained model rather than training in place.
t0    <- proc.time()[["elapsed"]]
model <- build_net()
model <- ggmlR::ggml_fit(model, Xtr, Ttr$y,
                         epochs = opt$epochs, batch_size = opt$batch,
                         sample_weight = Ttr$w, verbose = 1L)
train_secs <- proc.time()[["elapsed"]] - t0
cat(sprintf("\ntrained in %.0f s\n\n", train_secs))

# ---------------------------------------------------------------------------
# What it learnt
# ---------------------------------------------------------------------------

vp <- ggmlR::ggml_predict(model, Xva) * y_sd + y_mu
idx_prev <- cbind(seq_len(nrow(vp)), va$prev_a)
idx_next <- cbind(seq_len(nrow(vp)), va$next_a)
q_prev <- vp[idx_prev]
q_next <- vp[idx_next]

mae_prev <- mean(abs(q_prev - (va$p - 1L)))
mae_next <- mean(abs(q_next - (va$p + 1L)))

# The metrics that say whether a SEARCH can use this, which MAE does not.
# pair_accuracy asks the only question a descent asks at one step: of two moves
# whose true order is known, did the net put the better one lower. top1 is the
# harder form -- the helping move has to beat all seventeen others, sixteen of
# which were never labelled for this state.
pair_acc <- mean(q_prev < q_next)
top1_acc <- mean(max.col(-vp, ties.method = "first") == va$prev_a)

cat("== the Q-model ==========================================\n\n")
cat(sprintf("  MAE on previous  %.2f moves   (label p-1)\n", mae_prev))
cat(sprintf("  MAE on next      %.2f moves   (label p+1)\n", mae_next))
cat(sprintf("\n  pair_accuracy    %.1f%%   (previous ranked below next)\n",
            100 * pair_acc))
cat(sprintf("  top1_accuracy    %.1f%%   (previous is the argmin of all %d)\n",
            100 * top1_acc, n_a))
cat(sprintf("\n  chance is %.1f%% for pair, %.1f%% for top1.\n",
            50, 100 / n_a))

if (pair_acc < 0.55) {
  cat("\n  The ordering is barely better than a coin: a descent cannot steer\n")
  cat("  by this, whatever the MAE says.\n")
}

# Both metrics by depth. This is the table that replaces the `step` column:
# where pair_accuracy falls to chance the net has stopped being able to order
# the neighbours, and that -- not where the absolute number flattens -- is the
# horizon a search actually hits.
cat("\n  by depth (where the ordering survives):\n")
for (d in sort(unique(va$p))) {
  k <- va$p == d
  if (!any(k)) next
  cat(sprintf("    %3d moves  n=%4d   pair %5.1f%%   top1 %5.1f%%   q(prev) %6.2f\n",
              d, sum(k), 100 * mean(q_prev[k] < q_next[k]),
              100 * mean(max.col(-vp[k, , drop = FALSE],
                                 ties.method = "first") == va$prev_a[k]),
              mean(q_prev[k])))
}
cat("\n  Read pair_accuracy, not the MAE: a descent needs the order of the\n")
cat("  neighbours, and it keeps working at depths where the absolute\n")
cat("  distance has long since flattened out.\n")

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

model_name <- if (nzchar(opt$name)) opt$name else
  sprintf("cube3_q_m%d_h%d_b%d_d%d_n%d_e%d_s%d",
          opt$d_model, opt$heads, opt$blocks, opt$depth,
          opt$states, opt$epochs, opt$seed)

out <- file.path(path.expand(opt$out), model_name)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

ggmlR::ggml_save_model(model, file.path(out, "model.ggml"))
saveRDS(list(version = 1L, kind = "q", y_mu = y_mu, y_sd = y_sd,
             moves = mv, n_out = n_a,
             depth = opt$depth,
             d_model = opt$d_model, heads = opt$heads, blocks = opt$blocks,
             ff = opt$ff, states = opt$states, epochs = opt$epochs,
             seed = opt$seed,
             mae_prev = mae_prev, mae_next = mae_next,
             pair_accuracy = pair_acc, top1_accuracy = top1_acc,
             train_secs = train_secs),
        file.path(out, "meta.rds"))

cat("\nsaved to", out, "\n")
for (f in list.files(out)) {
  cat(sprintf("  %-14s %8.1f KB\n", f, file.size(file.path(out, f)) / 1024))
}
