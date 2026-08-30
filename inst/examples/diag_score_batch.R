#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Is scoring slow per state, or slow per call?
#
# diag_encoding_cost.R put 88% of a piece-encoded ADI iteration in one step:
# scoring the children with the frozen network, 9.4x slower than the same step
# under the sticker encoding, while the training step next to it was only 1.4x
# slower. Both run the same network on the same shape of input, so input width
# does not separate them. What does separate them is how the work is divided:
# fit hands 2000 states to one call, score hands 24000 states to ninety-odd
# calls of 256.
#
# That suggests the cost is per call rather than per state. The test is to vary
# the batch and watch what the total does:
#
#   per call   -> time falls roughly as the number of calls falls, and the
#                 gap between the encodings closes with it
#   per state  -> time stays flat, the batch is irrelevant, and the forward
#                 pass itself is what to look at
#
# Both encodings are measured at every size, because "piece is slow" and
# "many small calls are slow" predict the same thing for piece alone; only the
# sticker column says which.
#
# Note that batch_size is not free to choose at solve time: a compiled graph is
# built for one batch, and cube_adi_solve pads its 12 children out to fill it.
# A larger scoring batch means padding more, which is wasted work in exactly
# the case -- one state, its children -- that a solve does most often. So the
# useful answer here is a size for training, where the batch is genuinely full,
# and that is what the last column reports.
#
# Usage:  Rscript diag_score_batch.R [n_states] [reps] [sizes]
#   e.g.  Rscript diag_score_batch.R 2000 5
#         Rscript diag_score_batch.R 2000 5 "256,512,1024,2048,4096"
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args   <- commandArgs(trailingOnly = TRUE)
NSTATE <- if (length(args) >= 1) as.integer(args[1]) else 2000L
REPS   <- if (length(args) >= 2) as.integer(args[2]) else 5L
SIZES  <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else
  c(256L, 512L, 1024L, 2048L)

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g <- cube_group(3, moves = FACES)

## One batch of children, made once and scored by everything below: the states
## being identical across the sizes and the encodings is what makes the times
## comparable.
set.seed(5)
sc <- cayleyR:::cube_adi_scramble(g$ptr, NSTATE, 14L)
ch <- cayleyR:::cube_adi_children(g$ptr, sc$states)
kids <- ch$children
n_kid <- nrow(kids)

cat(sprintf("scoring %d children (%d states x %d moves), %d reps each\n\n",
            n_kid, NSTATE, length(FACES), REPS))

time_score <- function(encoding, size) {
  set.seed(99)
  net <- cube_adi_model(g, embed_dim = 32L, hidden = c(512L), n_blocks = 3L,
                        arch = "resnet", encoding = encoding)
  ## A compiled graph is built on first use at a given batch, so the first call
  ## at each size pays for pipelines the rest do not. Warmed, not timed.
  invisible(cayleyR:::adi_value_of(net$value, kids[seq_len(size), , drop = FALSE],
                                   size, net$arch, net$layout))
  t0 <- proc.time()[["elapsed"]]
  for (r in seq_len(REPS))
    invisible(cayleyR:::adi_value_of(net$value, kids, size, net$arch,
                                     net$layout))
  (proc.time()[["elapsed"]] - t0) / REPS
}

cat(sprintf("%8s %6s %10s %10s %8s\n",
            "batch", "calls", "sticker", "piece", "ratio"))
res <- list()
for (size in SIZES) {
  a <- time_score("sticker", size)
  b <- time_score("piece", size)
  calls <- ceiling(n_kid / size)
  cat(sprintf("%8d %6d %10.3f %10.3f %8.1fx\n", size, calls, a, b,
              b / max(a, 1e-6)))
  flush(stdout())
  res[[length(res) + 1L]] <- data.frame(size = size, calls = calls,
                                        sticker = a, piece = b)
}
res <- do.call(rbind, res)

cat("\n== what the shape says ===================================\n\n")
base <- res[res$size == min(res$size), ]
best <- res[which.min(res$piece), ]
cat(sprintf("  piece at batch %d: %.3f s, against %.3f s at batch %d (%.1fx faster)\n",
            best$size, best$piece, base$piece, base$size,
            base$piece / max(best$piece, 1e-6)))
cat(sprintf("  sticker over the same range: %.3f -> %.3f s (%.1fx)\n",
            base$sticker, res$sticker[which.min(res$piece)],
            base$sticker / max(res$sticker[which.min(res$piece)], 1e-6)))

## Per-call cost shows up as time tracking the call count; per-state cost shows
## up as a flat line. Reported as the correlation of time with calls so the
## answer does not depend on reading the column by eye.
if (nrow(res) >= 3L) {
  cat(sprintf("\n  time vs number of calls: piece r = %.2f, sticker r = %.2f\n",
              stats::cor(res$calls, res$piece),
              stats::cor(res$calls, res$sticker)))
  cat("  Near 1 means the calls are the cost, not the states in them.\n")
}

cat(sprintf("\n  For training, where the batch is full either way, the cheapest\n"))
cat(sprintf("  size measured here is %d.\n", best$size))
