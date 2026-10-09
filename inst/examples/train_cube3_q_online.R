#!/usr/bin/env Rscript
# A Q-model for the 3x3x3, trained on states that are never seen twice.
#
# train_cube3_q.R builds one fixed set of walks and passes over it `epochs`
# times. That is what a Keras-shaped fit does, and here it is the wrong shape.
# Two of the eighteen outputs carry a label, and that label is noisy: p-1 is an
# upper bound on the distance, overstating it by an amount that depends on the
# state. Averaging the overstatement away needs MANY DIFFERENT states, not the
# same states many times -- and re-reading a fixed set is how a network learns
# the noise instead.
#
# What that looked like: at depth=20 the mean prediction settled on ~11, at
# depth=10 on ~5.8. Both are the middle of their own range, which is what a net
# answers once it has stopped telling states apart. pair_accuracy fell to chance
# past depth ~8 in the first and past ~6 in the second -- narrowing the training
# depth made the middle depths WORSE, so the fixed sample, not the depth range,
# was the binding constraint.
#
# Raising `states` does not fix it and cannot: the encoded input is
# [states, 26, 78] doubles, so 600k walks is 9.7 GB and the run is killed before
# it trains. The memory holds the whole sample at once precisely because the
# sample is fixed.
#
# So the sample is not fixed here. Each step draws a fresh batch, trains on it
# once, and discards it -- the shape cayleypy-training-core uses (256 steps of
# 2048 per "epoch", 65536 epochs, no state repeated). Memory is then one batch
# rather than the whole run, and the states seen is limited by time instead of
# by RAM.
#
# The pivot comes from generate_walk(pivot = TRUE): the cut is drawn inside the
# walk, so `next_move` is the move the walk actually went on to make rather than
# a neighbour drawn at random, and `prev_move` is the inverse of the move that
# arrived. Those are the only two moves whose value is known at that state.
#
# Run with:  Rscript inst/examples/train_cube3_q_online.R [name=value ...]
#
#   depth=20 steps=20000 batch=512 report=250
#   d_model=64 heads=4 blocks=3 ff=256 backend=auto seed=42
#
# `steps` x `batch` is the number of states the run will see; nothing is
# repeated, so raising `steps` is how this run is made longer, not `epochs`.

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
  steps   = 20000L,  # fresh batches; states seen = steps * batch
  batch   = 512L,
  val     = 4000L,   # held-out walks, drawn once and kept fixed
  depth   = 20L,     # longest walk; the cut falls inside it
  k_min   = 2L,      # shallowest pivot; 1 is one move from solved
  report  = 250L,    # steps between progress lines
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

if (opt$depth < 2L) stop("depth must be at least 2: a pivot needs a step each side")

set.seed(opt$seed)

cat("parameters:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

g   <- cube_group(3)
lay <- cube_piece_layout(g)
mv  <- g$moves
n_a <- length(mv)
mv_index <- setNames(seq_along(mv), mv)

cat("group    :", g$name, "--", g$n, "positions,", n_a, "moves\n")
cat("pieces   :", lay$n_piece, "of width", lay$width, "\n")
cat("head     :", n_a, "outputs, one per move\n")
cat(sprintf("plan     : %d steps x %d = %s states, none repeated\n\n",
            opt$steps, opt$batch,
            format(as.numeric(opt$steps) * opt$batch, big.mark = ",",
                   scientific = FALSE)))

# Scaled by numbers that follow from `depth` alone, so the predicting side can
# undo it knowing only that -- no extra file travels beside the weights.
y_mu <- (1 + opt$depth) / 2
y_sd <- stats::sd(seq_len(opt$depth))
cat(sprintf("target scaling: mean %.1f, sd %.1f\n\n", y_mu, y_sd))

# ---------------------------------------------------------------------------
# A batch
# ---------------------------------------------------------------------------

# generate_walk(pivot = TRUE) returns the cut point, its label, and the two
# moves that meet there. Everything else about the batch is shape: the encoder
# turns states into the transformer's input, and the mask says which two of the
# eighteen outputs the loss may look at.
#
# The fill on the sixteen unlabelled outputs is 0 in scaled units -- the mean
# depth. The mask zeroes their gradient exactly (checked on a linear net: two
# runs whose masked targets differ by 200 give bit-identical trained outputs),
# but a shared trunk still carries their VALUE into the trained head, and a wild
# fill costs accuracy there:
#
#   inactive target    0 -> active head MAE 0.027
#   inactive target    3 -> active head MAE 0.080
#   inactive target  100 -> active head MAE 0.201
#
# so the blandest available value is the right one.
make_batch <- function(n) {
  w <- generate_walk(g, n = n, n_moves = opt$depth,
                     exact = FALSE, no_undo = TRUE, pivot = TRUE,
                     k_min = opt$k_min)
  p      <- as.integer(w$depth)
  prev_a <- unname(mv_index[w$prev_move])
  next_a <- unname(mv_index[w$next_move])

  y <- matrix(0, n, n_a)
  m <- matrix(0, n, n_a)
  ip <- cbind(seq_len(n), prev_a)
  ix <- cbind(seq_len(n), next_a)
  y[ip] <- (p - 1L - y_mu) / y_sd
  y[ix] <- (p + 1L - y_mu) / y_sd
  m[ip] <- 1
  m[ix] <- 1

  list(x = cayleyR:::adi_encode(w$states, "transformer", lay),
       y = y, mask = m, p = p, prev_a = prev_a, next_a = next_a)
}

# The validation set is drawn once and then left alone: the point of it is that
# the same states are scored at every report, so the numbers are comparable
# down the run. Only the training stream is fresh.
cat("drawing the held-out set\n"); flush(stdout())
va <- make_batch(opt$val)

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

# What a report line says. pair_accuracy is the question a descent actually
# asks -- of two moves whose order is known, did the net put the better one
# lower -- and top1 is its harder form, where the helping move has to beat all
# seventeen others. MAE is reported too but it is not what decides a search.
score <- function(model, d) {
  vp <- ggmlR::ggml_predict(model, d$x) * y_sd + y_mu
  rows <- seq_len(nrow(vp))
  q_prev <- vp[cbind(rows, d$prev_a)]
  q_next <- vp[cbind(rows, d$next_a)]
  list(vp = vp, q_prev = q_prev, q_next = q_next,
       mae_prev = mean(abs(q_prev - (d$p - 1L))),
       mae_next = mean(abs(q_next - (d$p + 1L))),
       pair = mean(q_prev < q_next),
       top1 = mean(max.col(-vp, ties.method = "first") == d$prev_a))
}

cat(sprintf("training %d steps of %d\n\n", opt$steps, opt$batch))
cat("    step        seen   pair    top1   MAE(prev)   gen_s   fit_s\n")
flush(stdout())

model <- build_net()

# One ggml_fit per batch, one epoch each. ggml_fit RETURNS the trained model
# rather than training in place, and it carries the optimizer state forward, so
# calling it in a loop continues one training run rather than restarting it.
t_start <- proc.time()[["elapsed"]]
gen_secs <- 0; fit_secs <- 0
history <- list()

for (step in seq_len(opt$steps)) {
  t0 <- proc.time()[["elapsed"]]
  b  <- make_batch(opt$batch)
  t1 <- proc.time()[["elapsed"]]
  model <- ggmlR::ggml_fit(model, b$x, b$y, epochs = 1L,
                           batch_size = opt$batch,
                           sample_weight = b$mask, verbose = 0L)
  t2 <- proc.time()[["elapsed"]]
  gen_secs <- gen_secs + (t1 - t0)
  fit_secs <- fit_secs + (t2 - t1)

  if (step %% opt$report == 0L || step == opt$steps) {
    s <- score(model, va)
    history[[length(history) + 1L]] <- data.frame(
      step = step, seen = as.numeric(step) * opt$batch,
      pair = s$pair, top1 = s$top1, mae_prev = s$mae_prev)
    cat(sprintf("  %6d  %10s  %5.1f%%  %5.1f%%   %7.2f   %6.0f  %6.0f\n",
                step,
                format(as.numeric(step) * opt$batch, big.mark = ",",
                       scientific = FALSE),
                100 * s$pair, 100 * s$top1, s$mae_prev, gen_secs, fit_secs))
    flush(stdout())
  }
}

train_secs <- proc.time()[["elapsed"]] - t_start
cat(sprintf("\ntrained in %.0f s  (generating %.0f s, fitting %.0f s)\n\n",
            train_secs, gen_secs, fit_secs))

# ---------------------------------------------------------------------------
# What it learnt
# ---------------------------------------------------------------------------

s <- score(model, va)

cat("== the Q-model ==========================================\n\n")
cat(sprintf("  states seen      %s (none twice)\n",
            format(as.numeric(opt$steps) * opt$batch, big.mark = ",",
                   scientific = FALSE)))
cat(sprintf("  MAE on previous  %.2f moves   (label p-1)\n", s$mae_prev))
cat(sprintf("  MAE on next      %.2f moves   (label p+1)\n", s$mae_next))
cat(sprintf("\n  pair_accuracy    %.1f%%   (previous ranked below next)\n",
            100 * s$pair))
cat(sprintf("  top1_accuracy    %.1f%%   (previous is the argmin of all %d)\n",
            100 * s$top1, n_a))
cat(sprintf("\n  chance is %.1f%% for pair, %.1f%% for top1.\n", 50, 100 / n_a))

# The table that replaces the `step` column of the depth script. Where
# pair_accuracy reaches chance the net can no longer order the neighbours, and
# that is the horizon a search hits -- not where the absolute number flattens.
cat("\n  by depth (where the ordering survives):\n")
for (d in sort(unique(va$p))) {
  k <- va$p == d
  if (!any(k)) next
  cat(sprintf("    %3d moves  n=%4d   pair %5.1f%%   top1 %5.1f%%   q(prev) %6.2f\n",
              d, sum(k), 100 * mean(s$q_prev[k] < s$q_next[k]),
              100 * mean(max.col(-s$vp[k, , drop = FALSE],
                                 ties.method = "first") == va$prev_a[k]),
              mean(s$q_prev[k])))
}
cat("\n  Read pair_accuracy, not the MAE, and read q(prev) for saturation:\n")
cat("  once it stops rising with depth the net has stopped telling those\n")
cat("  depths apart, whatever the MAE says.\n")

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

model_name <- if (nzchar(opt$name)) opt$name else
  sprintf("cube3_qon_m%d_h%d_b%d_d%d_s%dk_b%d_s%d",
          opt$d_model, opt$heads, opt$blocks, opt$depth,
          round(opt$steps / 1000), opt$batch, opt$seed)

out <- file.path(path.expand(opt$out), model_name)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

ggmlR::ggml_save_model(model, file.path(out, "model.ggml"))
saveRDS(list(version = 1L, kind = "q_online", y_mu = y_mu, y_sd = y_sd,
             moves = mv, n_out = n_a, depth = opt$depth,
             d_model = opt$d_model, heads = opt$heads, blocks = opt$blocks,
             ff = opt$ff, steps = opt$steps, batch = opt$batch,
             states_seen = as.numeric(opt$steps) * opt$batch,
             seed = opt$seed,
             mae_prev = s$mae_prev, mae_next = s$mae_next,
             pair_accuracy = s$pair, top1_accuracy = s$top1,
             history = do.call(rbind, history),
             train_secs = train_secs),
        file.path(out, "meta.rds"))

cat("\nsaved to", out, "\n")
for (f in list.files(out)) {
  cat(sprintf("  %-14s %8.1f KB\n", f, file.size(file.path(out, f)) / 1024))
}
