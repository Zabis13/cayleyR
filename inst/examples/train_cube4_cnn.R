#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Train the distance estimator, and nothing else.
#
# A 1D CNN that reads a 4x4x4 as its 56 pieces and answers how many moves the
# scramble was. It is one half of a solver: the other half, a beam search that
# uses these scores as its heuristic, lives in bench_model_vs_schedule.R and
# loads whatever this script saved.
#
# Split in two because the halves have different costs and different reasons to
# be re-run. Training is minutes and depends only on TRAIN_MAX and the shape of
# the net; the benchmark is re-run whenever the cubes, the beam width or the
# rival solver changes, and has no business retraining a model each time.
#
# ---- Why the pieces --------------------------------------------------------
#
# The 96 stickers are grouped into the 56 pieces they physically belong to --
# 8 corners of three stickers, 24 wings of two, 24 centres of one -- so the
# network is told from the start that stickers 0, 51 and 64 are one corner
# rather than having to discover it. That also makes each cube a sequence of 56
# positions with 21 channels, which is what a 1D convolution wants.
#
# The layout is transcribed from /mnt/Data2/DS_projects/444/archive
# (unified_training/models.py, _cube4_layout) rather than derived here: which
# three stickers make a corner is a fact about the cube, and copying a working
# table beats re-deriving one. The check below confirms every sticker lands in
# exactly one piece.
#
# ---- What is predicted, and what that costs --------------------------------
#
# The length of the scramble. A random word of d moves can sometimes be undone
# in fewer, so the label is an upper bound rather than the true distance, and
# the noise grows with depth: by 80 moves a fair share of the labels overstate
# what the position really needs. The run reports how many states were seen
# carrying more than one label, which is the part of that noise it can actually
# observe.
#
# 80 is the training range, not a ceiling on what the model may be asked about
# afterwards. The benchmark runs it on cubes far deeper than that; whether the
# answers there mean anything is what the `by depth` table below is for.
#
# Depths are drawn uniformly from 1..TRAIN_MAX. That is not the same as
# sampling the group evenly -- deep cubes vastly outnumber shallow ones -- but
# it is what keeps the shallow end represented at all, and the shallow end is
# where a beam finishes its walk.
#
# Usage:  Rscript train_cube4_cnn.R [train_max] [n_train] [epochs] [out]
#   e.g.  Rscript train_cube4_cnn.R 80 60000 40
#         Rscript train_cube4_cnn.R 40 40000 40 /tmp/shallow.ggml
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args      <- commandArgs(trailingOnly = TRUE)
TRAIN_MAX <- if (length(args) >= 1) as.integer(args[1]) else 80L
N_TRAIN   <- if (length(args) >= 2) as.integer(args[2]) else 60000L
EPOCHS    <- if (length(args) >= 3) as.integer(args[3]) else 40L
## Models go beside the 4x4x4 project they belong to, in a folder of their own
## so they do not mix with its archive or its Kaggle data. The name carries the
## settings that shape the weights, so a model trained for other depths cannot
## be picked up by mistake.
MODEL_DIR <- "/mnt/Data2/DS_projects/444/cayleyR"
OUT       <- if (length(args) >= 4) args[4] else
  file.path(MODEL_DIR,
            sprintf("cube4_cnn_d%d_n%d_e%d.ggml", TRAIN_MAX, N_TRAIN, EPOCHS))
OUT <- path.expand(OUT)

FILTERS <- 64L
KERNEL  <- 3L
N_VAL   <- 4000L

g  <- cube_group(4)
id <- group_identity(g)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
nm <- names(mv)

## ---- the piece layout ----------------------------------------------------
CORNERS <- rbind(
  c(0, 51, 64), c(3, 35, 48), c(12, 16, 67), c(15, 19, 32),
  c(28, 79, 80), c(31, 44, 83), c(47, 60, 95), c(63, 76, 92))
WINGS <- rbind(
  c(1, 50), c(2, 49), c(4, 65), c(7, 34), c(8, 66), c(11, 33),
  c(13, 17), c(14, 18), c(20, 71), c(23, 36), c(24, 75), c(27, 40),
  c(29, 81), c(30, 82), c(39, 52), c(43, 56), c(45, 87), c(46, 91),
  c(55, 68), c(59, 72), c(61, 94), c(62, 93), c(77, 88), c(78, 84))
CENTRES <- as.integer(vapply(0:5, function(f) f * 16L + c(5L, 6L, 9L, 10L),
                             integer(4)))

N_PIECE   <- 56L
SLOTS     <- 3L
SLOT_DIM  <- 6L
PIECE_DIM <- SLOTS * SLOT_DIM + 3L
PIECE_IX  <- rbind(CORNERS + 1L, cbind(WINGS + 1L, 0L),
                   cbind(matrix(CENTRES + 1L, ncol = 1L), 0L, 0L))
PIECE_TYPE <- c(rep(0L, 8L), rep(1L, 24L), rep(2L, 24L))
stopifnot(identical(sort(as.integer(PIECE_IX[PIECE_IX > 0L])), seq_len(96L)))

encode_pieces <- function(states) {
  n <- nrow(states)
  out <- array(0, dim = c(n, N_PIECE, PIECE_DIM))
  col <- (states - 1L) %/% 16L
  for (p in seq_len(N_PIECE)) {
    for (sl in seq_len(SLOTS)) {
      ix <- PIECE_IX[p, sl]
      if (ix == 0L) next
      cvec <- col[, ix]
      base <- (sl - 1L) * SLOT_DIM
      for (cc in 0:5) out[, p, base + cc + 1L] <- (cvec == cc) * 1.0
    }
    out[, p, SLOTS * SLOT_DIM + PIECE_TYPE[p] + 1L] <- 1.0
  }
  out
}

## ---- the net -------------------------------------------------------------
## Dropout sits after the flatten and not between the convolutions. In ggmlR a
## dropout feeding a conv_1d aborts training with
## GGML_ASSERT(ggml_is_padded_1d(a)): conv_1d permutes its input, so the
## gradient arrives at dropout as a non-contiguous view and ggml_scale refuses
## it. The forward pass builds and runs either way, which is what makes this
## easy to miss -- it only shows in ggml_fit.
build_cnn <- function() {
  ggml_model_sequential() |>
    ggml_layer_conv_1d(FILTERS, kernel_size = KERNEL, activation = "relu",
                       padding = "same", input_shape = c(N_PIECE, PIECE_DIM),
                       name = "conv1") |>
    ggml_layer_batch_norm(name = "bn1") |>
    ggml_layer_conv_1d(FILTERS, kernel_size = KERNEL, activation = "relu",
                       padding = "same", name = "conv2") |>
    ggml_layer_batch_norm(name = "bn2") |>
    ggml_layer_flatten() |>
    ggml_layer_dropout(0.1, stochastic = TRUE, name = "drop") |>
    ggml_layer_dense(128L, activation = "relu", name = "head") |>
    ggml_layer_dense(1L, name = "depth") |>
    ggml_compile(optimizer = "adam", loss = "mse", metrics = character(0))
}

## `note` gives the loop something to say while it runs: at TRAIN_MAX 80 each
## cube costs up to eighty permutation applies, so sixty thousand of them is a
## wait the reader should not have to sit through blind.
make_data <- function(n, seed, note = NULL) {
  set.seed(seed)
  states <- matrix(0L, n, 96L)
  y <- integer(n)
  every <- max(1L, n %/% 20L)
  for (i in seq_len(n)) {
    d <- sample.int(TRAIN_MAX, 1L)
    s <- id
    for (m in sample(nm, d, replace = TRUE)) s <- s[mv[[m]]]
    states[i, ] <- s
    y[i] <- d
    if (!is.null(note) && i %% every == 0L) {
      cat(sprintf("\r  %s %5.0f%%", note, 100 * i / n))
      flush(stdout())
    }
  }
  if (!is.null(note)) cat("\r", strrep(" ", 30), "\r", sep = "")
  list(states = states, y = y)
}

## ---- run -----------------------------------------------------------------
cat(sprintf("training a distance estimator | depths 1..%d\n", TRAIN_MAX))
cat(sprintf("%d cubes, %d epochs, %d pieces x %d channels, %d filters\n",
            N_TRAIN, EPOCHS, N_PIECE, PIECE_DIM, FILTERS))
cat(sprintf("out: %s\n\n", OUT))

cat("building data\n")
flush(stdout())
t0 <- proc.time()[["elapsed"]]
tr <- make_data(N_TRAIN, 1L, note = "train")
va <- make_data(N_VAL, 777L, note = "val  ")
cat("  encoding ... ")
flush(stdout())
Xtr <- encode_pieces(tr$states)
cat(sprintf("done, %.0f s total\n", proc.time()[["elapsed"]] - t0))

keys <- apply(tr$states, 1L, function(r) paste(r, collapse = ","))
dup <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
collisions <- if (any(dup))
  sum(tapply(tr$y[dup], keys[dup], function(v) length(unique(v)) > 1L)) else 0L
cat(sprintf("label noise: %d states carry more than one depth (of %d)\n",
            collisions, N_TRAIN))

## The scaling of the target is a function of TRAIN_MAX alone, since depths are
## drawn uniformly from 1..TRAIN_MAX. Recomputing it the same way in the
## benchmark is what lets a loaded model return numbers on the right scale
## without storing anything beside the weights.
y_mu <- (1 + TRAIN_MAX) / 2
y_sd <- stats::sd(seq_len(TRAIN_MAX))
cat(sprintf("target scaling: mean %.1f, sd %.1f (from TRAIN_MAX alone)\n\n",
            y_mu, y_sd))

cat(sprintf("training %d epochs, reporting every %d\n",
            EPOCHS, max(1L, EPOCHS %/% 10L)))
flush(stdout())
t0 <- proc.time()[["elapsed"]]
# ggml_fit RETURNS the trained model rather than training in place; dropping
# the return value leaves an untrained network that scores like the mean.
model <- build_cnn()

## Progress is printed from the history rather than by ggml_fit itself.
## verbose = 1 draws a per-batch progress bar with carriage returns, which is
## unreadable once the run is long enough to matter and useless in a log file;
## the history carries the same losses per epoch and can be shown as a table.
##
## Training is split into blocks so something appears while it runs. Checked
## rather than assumed: six epochs in one call reached train loss 0.170 on a toy
## fit, three blocks of two reached 0.104 from the same seed, so the blocks do
## continue from the weights returned rather than starting over. The optimiser
## state does restart each block -- for Adam the moment estimates warm up again
## -- which is a small price for being able to watch the loss move.
##
## Note that model$history is REPLACED by each call, not appended to: after
## three blocks of two it holds two entries, not six. Only the last value of a
## block is read below for that reason; a full curve would have to be collected
## here rather than taken from the model at the end.
##
## Losses are reported in MOVES, not in the scaled units the net trains on:
## mse on a target divided by y_sd comes back multiplied by y_sd^2, so the
## square root times y_sd is a number the reader can compare with the MAE below.
BLOCK <- max(1L, EPOCHS %/% 10L)
done <- 0L
cat(sprintf("\n  %6s %12s %12s %8s\n", "epochs", "train rmse", "val rmse", "secs"))
while (done < EPOCHS) {
  n_now <- min(BLOCK, EPOCHS - done)
  tb <- proc.time()[["elapsed"]]
  invisible(utils::capture.output(
    model <- ggml_fit(model, Xtr, matrix((tr$y - y_mu) / y_sd, ncol = 1L),
                      epochs = n_now, batch_size = 64L,
                      validation_split = 0.1, verbose = 0)))
  done <- done + n_now
  h <- model$history
  tl <- if (!is.null(h$train_loss)) sqrt(utils::tail(h$train_loss, 1)) * y_sd else NA
  vl <- if (!is.null(h$val_loss)) sqrt(utils::tail(h$val_loss, 1)) * y_sd else NA
  cat(sprintf("  %6d %12.2f %12.2f %8.0f\n", done, tl, vl,
              proc.time()[["elapsed"]] - tb))
  flush(stdout())
}
cat(sprintf("\ntrained in %.0f s\n\n", proc.time()[["elapsed"]] - t0))

## ---- what it learnt ------------------------------------------------------
vp <- as.numeric(ggml_predict(model, encode_pieces(va$states))) * y_sd + y_mu
mae <- mean(abs(vp - va$y))
base_mae <- mean(abs(va$y - y_mu))

cat("== the estimator ========================================\n\n")
cat(sprintf("  MAE       %.2f moves   (baseline %.2f)\n", mae, base_mae))
cat(sprintf("  within 1  %.0f%%   within 5  %.0f%%\n",
            100 * mean(abs(vp - va$y) <= 1), 100 * mean(abs(vp - va$y) <= 5)))

if (stats::sd(vp) < 0.05) {
  cat("\n  The predictions barely vary: the model has not converged.\n")
} else if (mae >= base_mae * 0.95) {
  cat("\n  No better than answering the mean.\n")
} else {
  cat(sprintf("\n  Better than the mean by %.0f%%.\n", 100 * (1 - mae / base_mae)))
}

## The table a beam actually consumes. What matters is not the accuracy but
## whether the prediction RISES with the depth: where consecutive depths score
## the same, the search has nothing to choose between them, and that is where
## it will stall no matter how wide the beam.
cat("\n  by depth (what the beam steers by):\n")
bins <- unique(round(seq(1, TRAIN_MAX, length.out = 12)))
prev <- NA_real_
for (d in bins) {
  k <- abs(va$y - d) <= max(1, TRAIN_MAX / 40)
  if (!any(k)) next
  m <- mean(vp[k])
  step <- if (is.na(prev)) "" else sprintf("  step %+.2f", m - prev)
  cat(sprintf("    %3d moves  n=%4d   predicted %6.2f +- %.2f%s\n",
              d, sum(k), m, stats::sd(vp[k]), step))
  prev <- m
}
cat("\n  Read the `step` column: once it goes to nearly zero the estimator has\n")
cat("  stopped telling those depths apart, and a beam starting beyond that\n")
cat("  point is walking blind until it gets back under it.\n")

## ---- save ----------------------------------------------------------------
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
ggml_save_model(model, OUT)
cat(sprintf("\nsaved: %s (%.1f MB)\n", OUT, file.size(OUT) / 2^20))
cat("run the benchmark with:\n")
cat(sprintf("  Rscript bench_model_vs_schedule.R 10 500 %d 256 %s\n",
            TRAIN_MAX, OUT))
