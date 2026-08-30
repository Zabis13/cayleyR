#!/usr/bin/env Rscript
# A distance estimator for the 3x3x3, trained on scramble length.
#
# The other way round from ADI. There the label for a state is built out of the
# network's own opinion of its children, anchored by the one fact that a solved
# child is worth nothing; here the label is simply how long the scramble was.
# Nothing is inferred, nothing propagates outward from the goal, and no frozen
# copy is needed -- it is ordinary regression on data that happens to be free.
#
# The trade is exact-but-slow against approximate-but-cheap:
#
#   ADI      labels are exact next to the goal and get their accuracy from
#            there outward, which is why short scrambles are indispensable to
#            it. Costs 18 forward passes per state, one per child.
#   this     labels are an upper bound everywhere -- a random word of d moves
#            can often be undone in fewer -- and the overstatement grows with
#            depth. Costs one forward pass per state, so roughly 18 times less.
#
# Which matters here: ADI stalled at depth 10 because its horizon caught up
# with the training depth, and pushing it further meant paying 18x for every
# state. This pays once and can be trained on scrambles of any length,
# including a fixed one -- see `fixed` below.
#
# The label noise is not hidden. The run reports how many states were drawn
# twice carrying different depths, which is the part of it that can actually be
# observed, and the `step` column of the depth table shows where the estimator
# stops telling consecutive depths apart. That column, not the MAE, is what
# decides whether a descent can steer by these numbers.
#
# The network is the same transformer ADI uses -- pieces as a sequence,
# attention over them -- so the two are comparable on architecture and differ
# only in where their labels come from.
#
# Predicting with the saved model is the other script, predict_cube3_depth.R.
#
# Run with:  Rscript inst/examples/train_cube3_depth.R [name=value ...]
#
#   depth=20 states=60000 epochs=40 fixed=0 batch=128
#   d_model=64 heads=4 blocks=3 ff=256 backend=auto seed=42
#
#   fixed=0  depths drawn uniformly from 1..depth (the default)
#   fixed=1  every scramble exactly `depth` moves
#
# The uniform draw is not an even sample of the group -- deep states vastly
# outnumber shallow ones -- but it keeps the shallow end represented, and the
# shallow end is where a descent finishes its walk. fixed=1 is there to measure
# what that choice is worth rather than to assume it.

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
  states  = 60000L,  # training cubes
  val     = 4000L,   # held-out cubes
  depth   = 20L,     # scramble length, or its ceiling when fixed=0
  fixed   = 0L,      # 1 = every scramble exactly `depth` moves
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

set.seed(opt$seed)

cat("parameters:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

g   <- cube_group(3)
lay <- cube_piece_layout(g)

cat("group    :", g$name, "--", g$n, "positions,", length(g$moves), "moves\n")
cat("pieces   :", lay$n_piece, "of width", lay$width, "\n")
cat("scrambles:", if (opt$fixed > 0L) sprintf("exactly %d moves", opt$depth)
                  else sprintf("1..%d moves, uniform", opt$depth), "\n\n")

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

# cube_adi_scramble() walks away from the solved state in C++ and reports the
# depth it used, which is exactly the label wanted here. It draws uniformly from
# 1..max_depth, so a fixed length is had by asking for that one depth and
# keeping only the rows that came back with it -- the walk itself is the same.
#
# It also refuses to undo its own last move. That trims the crudest way a label
# can overstate the distance (a move followed by its inverse) without touching
# longer returns, which are rarer and are what the collision count below
# measures.
make_data <- function(n, note) {
  if (opt$fixed > 0L) {
    states <- matrix(0L, n, g$n)
    got    <- 0L
    while (got < n) {
      sc   <- cayleyR:::cube_adi_scramble(g$ptr, (n - got) * 2L, opt$depth)
      keep <- which(sc$depth == opt$depth)
      if (length(keep) == 0L) next
      keep <- keep[seq_len(min(length(keep), n - got))]
      states[(got + 1L):(got + length(keep)), ] <- sc$states[keep, , drop = FALSE]
      got <- got + length(keep)
      cat(sprintf("\r  %s %5.0f%%", note, 100 * got / n)); flush(stdout())
    }
    y <- rep(opt$depth, n)
  } else {
    sc     <- cayleyR:::cube_adi_scramble(g$ptr, n, opt$depth)
    states <- sc$states
    y      <- as.integer(sc$depth)
    cat(sprintf("\r  %s  100%%", note))
  }
  cat("\r", strrep(" ", 30), "\r", sep = "")
  list(states = states, y = y)
}

cat("building data\n"); flush(stdout())
t0 <- proc.time()[["elapsed"]]
tr <- make_data(opt$states, "train")
va <- make_data(opt$val,    "val  ")
Xtr <- cayleyR:::adi_encode(tr$states, "transformer", lay)
Xva <- cayleyR:::adi_encode(va$states, "transformer", lay)
cat(sprintf("  done, %.0f s\n", proc.time()[["elapsed"]] - t0))

# How often the same cube turned up under two different labels. That is the
# visible part of the overstatement: a word of d moves that lands where a word
# of d' < d also lands. It cannot see the case where every draw of a state
# overstates it equally, which is why it is a floor on the noise, not a measure.
keys <- apply(tr$states, 1L, paste, collapse = ",")
dup  <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
coll <- if (any(dup))
  sum(tapply(tr$y[dup], keys[dup], function(v) length(unique(v)) > 1L)) else 0L
cat(sprintf("label noise: %d states carry more than one depth (of %d)\n",
            coll, opt$states))

# The target is scaled by numbers that follow from `depth` alone, not from the
# sample, so the predicting script can undo the scaling knowing only that -- no
# extra file has to travel beside the weights. A fixed depth has no spread at
# all, so it is left unscaled.
if (opt$fixed > 0L) {
  y_mu <- 0; y_sd <- 1
} else {
  y_mu <- (1 + opt$depth) / 2
  y_sd <- stats::sd(seq_len(opt$depth))
}
cat(sprintf("target scaling: mean %.1f, sd %.1f\n\n", y_mu, y_sd))

if (opt$fixed > 0L) {
  cat("NOTE: with a fixed depth every label is identical, so there is nothing\n")
  cat("      to regress -- the model can only learn the constant. This mode is\n")
  cat("      for measuring that, not for producing a usable estimator.\n\n")
}

# ---------------------------------------------------------------------------
# The network
# ---------------------------------------------------------------------------

# The same transformer cube_adi_model() builds for arch = "transformer": a
# time-distributed dense projecting each piece into d_model, a learned
# positional embedding because attention is order-blind and the slots are not
# interchangeable, the encoder blocks, then a mean over the pieces rather than a
# flatten -- no summary token exists and every piece counts equally.
#
# silu rather than gelu inside the blocks: gelu has no backward rule in ggml, so
# the graph builds and training then aborts.
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
  out <- ggmlR::ggml_layer_dense(h, 1L)
  ggmlR::ggml_compile(ggmlR::ggml_model(inputs = inp, outputs = out),
                      optimizer = "adam", loss = "mse", metrics = NULL,
                      backend = opt$backend)
}

cat(sprintf("training %d epochs on %d cubes\n\n", opt$epochs, opt$states))
flush(stdout())

# ggml_fit RETURNS the trained model rather than training in place; dropping the
# return value leaves an untrained network that scores like the mean.
t0    <- proc.time()[["elapsed"]]
model <- build_net()
model <- ggmlR::ggml_fit(model, Xtr, matrix((tr$y - y_mu) / y_sd, ncol = 1L),
                         epochs = opt$epochs, batch_size = opt$batch,
                         verbose = 1L)
train_secs <- proc.time()[["elapsed"]] - t0
cat(sprintf("\ntrained in %.0f s\n\n", train_secs))

# ---------------------------------------------------------------------------
# What it learnt
# ---------------------------------------------------------------------------

vp   <- as.numeric(ggmlR::ggml_predict(model, Xva)) * y_sd + y_mu
mae  <- mean(abs(vp - va$y))
base <- mean(abs(va$y - mean(va$y)))

cat("== the estimator ========================================\n\n")
cat(sprintf("  MAE       %.2f moves   (baseline %.2f)\n", mae, base))
cat(sprintf("  within 1  %.0f%%   within 3  %.0f%%\n",
            100 * mean(abs(vp - va$y) <= 1), 100 * mean(abs(vp - va$y) <= 3)))

if (stats::sd(vp) < 0.05) {
  cat("\n  The predictions barely vary: the model has not converged.\n")
} else if (mae >= base * 0.95) {
  cat("\n  No better than answering the mean.\n")
} else {
  cat(sprintf("\n  Better than the mean by %.0f%%.\n", 100 * (1 - mae / base)))
}

# The table a descent actually consumes. Accuracy is not the point: what matters
# is whether the prediction RISES from one depth to the next. Where consecutive
# depths score the same the descent has nothing to choose between them and
# stalls, however accurate the numbers are in absolute terms.
if (opt$fixed == 0L) {
  cat("\n  by depth (what a descent steers by):\n")
  prev <- NA_real_
  for (d in sort(unique(va$y))) {
    k <- va$y == d
    if (!any(k)) next
    m    <- mean(vp[k])
    step <- if (is.na(prev)) "" else sprintf("  step %+.2f", m - prev)
    cat(sprintf("    %3d moves  n=%4d   predicted %6.2f +- %.2f%s\n",
                d, sum(k), m, stats::sd(vp[k]), step))
    prev <- m
  }
  cat("\n  Read the `step` column: once it nears zero the estimator has stopped\n")
  cat("  telling those depths apart, and a descent starting beyond that point\n")
  cat("  is walking blind until it gets back under it.\n")
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

# One .ggml file plus a small RDS holding what the predicting script needs to
# read the output: the scaling, and the layout the encoder was built from.
model_name <- if (nzchar(opt$name)) opt$name else
  sprintf("cube3_dp_m%d_h%d_b%d_d%d%s_n%d_e%d_s%d",
          opt$d_model, opt$heads, opt$blocks, opt$depth,
          if (opt$fixed > 0L) "f" else "",
          opt$states, opt$epochs, opt$seed)

out <- file.path(path.expand(opt$out), model_name)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

ggmlR::ggml_save_model(model, file.path(out, "model.ggml"))
saveRDS(list(version = 1L, y_mu = y_mu, y_sd = y_sd,
             depth = opt$depth, fixed = opt$fixed,
             d_model = opt$d_model, heads = opt$heads, blocks = opt$blocks,
             ff = opt$ff, states = opt$states, epochs = opt$epochs,
             seed = opt$seed, mae = mae, baseline = mae / base,
             train_secs = train_secs),
        file.path(out, "meta.rds"))

cat("\nsaved to", out, "\n")
for (f in list.files(out)) {
  cat(sprintf("  %-14s %8.1f KB\n", f, file.size(file.path(out, f)) / 1024))
}

cat("\nnext:  Rscript inst/examples/predict_cube3_depth.R net=", out, "\n", sep = "")
