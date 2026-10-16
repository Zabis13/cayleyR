#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Does ggml_trainer() start from the weights of a loaded model?
#
# A model saved by train_cube3_q_online.R is loaded and scored three ways on
# the same held-out walks:
#
#   loaded    ggml_predict() on the loaded model
#   trainer   tr$predict() right after ggml_trainer(loaded): must match
#             "loaded" exactly if the trainer took the weights over
#   stepped   tr$predict() after `steps` steps at a tiny lr: must stay close,
#             not fall back to an untrained net (pair ~60%, top1 ~15%)
#
# The scaling (y_mu, y_sd) is read from meta.rds, not derived from a depth:
# a continued run has to keep the scale the weights were trained on.
#
# Usage:  Rscript inst/examples/check_trainer_init.R [name=value ...]
#   net=/mnt/Data2/DS_projects/333/qon_cosine8k   backend=auto
#   val=2000   steps=50   lr=1e-5
# ---------------------------------------------------------------------------

library(cayleyR)
stopifnot(requireNamespace("ggmlR", quietly = TRUE))

defaults <- list(net = "/mnt/Data2/DS_projects/333/qon_cosine8k",
                 backend = "auto", val = 2000L, steps = 50L, lr = 1e-5)
opt <- defaults
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1L]]
  if (length(kv) != 2L || is.null(defaults[[kv[[1L]]]]))
    stop("argument must be one of ", paste(names(defaults), collapse = ", "))
  opt[[kv[[1L]]]] <- if (is.character(defaults[[kv[[1L]]]])) kv[[2L]]
                     else if (is.integer(defaults[[kv[[1L]]]])) as.integer(kv[[2L]])
                     else as.numeric(kv[[2L]])
}

meta  <- readRDS(file.path(opt$net, "meta.rds"))
model <- ggmlR::ggml_load_model(file.path(opt$net, "model.ggml"),
                                backend = opt$backend)

g   <- cube_group(3)
lay <- cube_piece_layout(g)
mv  <- g$moves
n_a <- length(mv)
mv_index <- setNames(seq_along(mv), mv)
stopifnot(identical(as.character(meta$moves), as.character(mv)))

# The same batch shape the training script builds, scaled by meta.
make_batch <- function(n) {
  w <- generate_walk(g, n = n, n_moves = meta$depth, exact = FALSE,
                     no_undo = TRUE, pivot = TRUE, k_min = 2L,
                     no_cancel = isTRUE(meta$no_cancel))
  p      <- as.integer(w$depth)
  prev_a <- unname(mv_index[w$prev_move])
  next_a <- unname(mv_index[w$next_move])
  y <- matrix(0, n, n_a); m <- matrix(0, n, n_a)
  ip <- cbind(seq_len(n), prev_a); ix <- cbind(seq_len(n), next_a)
  y[ip] <- (p - 1L - meta$y_mu) / meta$y_sd
  y[ix] <- (p + 1L - meta$y_mu) / meta$y_sd
  m[ip] <- 1; m[ix] <- 1
  list(x = cayleyR:::adi_encode(w$states, "transformer", lay),
       y = y, mask = m, p = p, prev_a = prev_a, next_a = next_a)
}

score <- function(vp, d) {
  vp <- vp * meta$y_sd + meta$y_mu
  rows <- seq_len(nrow(vp))
  list(vp = vp,
       pair = mean(vp[cbind(rows, d$prev_a)] < vp[cbind(rows, d$next_a)]),
       top1 = mean(max.col(-vp, ties.method = "first") == d$prev_a),
       mae  = mean(abs(vp[cbind(rows, d$prev_a)] - (d$p - 1L))))
}
line <- function(nm, s) cat(sprintf("  %-8s  pair %5.1f%%  top1 %5.1f%%  MAE %.2f\n",
                                    nm, 100 * s$pair, 100 * s$top1, s$mae))

set.seed(1)
va <- make_batch(opt$val)

cat(sprintf("model: %s  (saved: pair %.1f%%, top1 %.1f%%)\n\n",
            basename(opt$net), 100 * meta$pair_accuracy, 100 * meta$top1_accuracy))

s0 <- score(ggmlR::ggml_predict(model, va$x, batch_size = meta$batch), va)
line("loaded", s0)

tr <- ggmlR::ggml_trainer(model, batch_size = meta$batch, loss_mask = n_a)
s1 <- score(tr$predict(va$x), va)
line("trainer", s1)
d01 <- max(abs(s0$vp - s1$vp))

tr$set_lr(opt$lr)
for (i in seq_len(opt$steps)) {
  b <- make_batch(meta$batch)
  tr$step(b$x, b$y, b$mask)
}
s2 <- score(tr$predict(va$x), va)
line("stepped", s2)

cat(sprintf("\n  max |loaded - trainer|   = %.2e\n", d01))
cat(sprintf("  max |trainer - stepped|  = %.2e   (0 means the steps changed nothing)\n",
            max(abs(s1$vp - s2$vp))))
cat(sprintf("  pair after %d steps      : %+.1f points\n\n", opt$steps,
            100 * (s2$pair - s1$pair)))

# Bit-identical predictions after training are a failure, not a neutral
# result: either the steps did nothing or predict does not see them.
ok_take <- d01 < 1e-3
ok_move <- max(abs(s1$vp - s2$vp)) > 0
ok_keep <- s2$pair > s1$pair - 0.03
cat(if (!ok_take)
      "VERDICT: FAIL -- trainer predictions differ from the loaded model, weights NOT taken over\n"
    else if (!ok_move)
      "VERDICT: FAIL -- predictions unchanged after the steps: training is not seen\n"
    else if (!ok_keep)
      "VERDICT: FAIL -- weights taken over, but training from them falls apart\n"
    else
      "VERDICT: OK -- starts from the loaded weights and trains on from them\n")
