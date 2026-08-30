#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Was the 2-d input the reason the piece encoding was slow?
#
# Three explanations have been measured and ruled out. Noise: the two steps
# that cannot depend on the encoding came back at 0.9x. Per-call overhead:
# eight times fewer calls bought piece 1.4x while buying sticker 2.4x, so the
# calls are what sticker pays for. Transfer: 24000 states are 110 MB, which
# copies in about 0.019 s against the 1.5 s the step takes -- one per cent.
#
# What was left is the one structural difference between the two paths. The
# sticker input is a dense tensor produced by an embedding inside the graph;
# the piece input was declared [pieces, bits] with a flatten over it. Same
# numbers, same order, same dense layer above -- different tensor shape.
#
# ---- Why this script builds its own networks -------------------------------
#
# "Before" and "after" are two builds of the package, and two builds cannot be
# compared inside one process. So neither side comes from cube_adi_model: both
# are built here, straight from ggmlR, differing only in the line under test.
# One run, one machine state, one set of weights.
#
# ---- What would count as an answer -----------------------------------------
#
# Not "faster". The cost was measured as a straight line -- a fixed part per
# call and a slope per state -- and the two encodings differed in SHAPE, not
# just in size: sticker spent half its time on the fixed part and 3.1 us per
# state, piece had no fixed part at all and 78.3 us per state.
#
# So the prediction is specific: if the 2-d input was the mechanism, the flat
# version should take on sticker's profile -- a slope in the same order of
# magnitude, the fixed part reappearing as the thing that dominates a small
# batch. If instead the slope merely halves and the profile stays the same
# shape, something was made cheaper but the mechanism was not the one removed,
# and that is a different finding. Both are reported below rather than a single
# ratio, because a single ratio cannot tell them apart.
#
# Usage:  Rscript diag_flat_vs_2d.R [reps]
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args  <- commandArgs(trailingOnly = TRUE)
REPS  <- if (length(args) >= 1) as.integer(args[1]) else 5L
## ggml_predict refuses a batch larger than the data, so the smallest run has
## to be at least BATCH. 1024 was the smallest size in an earlier version of
## this script and stopped it dead on the first row.
BATCH <- 1024L
SIZES <- c(1024L, 2048L, 4096L, 8192L)

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g   <- cube_group(3, moves = FACES)
lay <- cube_piece_layout(g)
P   <- lay$n_piece
W   <- lay$width
WIDE <- P * W          # bits per slot
FLAT <- P * WIDE       # the whole input

set.seed(5)
sc   <- cayleyR:::cube_adi_scramble(g$ptr, max(SIZES), 14L)
pool <- sc$states
enc  <- cayleyR:::adi_encode_pieces(pool, lay)   # flat, n x FLAT

## The same 1200 numbers per state, in the same order, viewed either way.
as_2d   <- array(enc, dim = c(nrow(enc), P, WIDE))
as_flat <- enc

## Two networks alike in everything but the input line. Seeded together so the
## weights match: the timing should not depend on them, and making them equal
## removes the question.
build <- function(kind) {
  set.seed(99)
  inp <- if (kind == "2d") ggml_input(shape = c(P, WIDE))
         else ggml_input(shape = FLAT)
  h <- if (kind == "2d") ggml_layer_flatten(inp) else inp
  h <- ggml_layer_dense(h, 512L, activation = "relu")
  for (b in 1:3) {
    r <- ggml_layer_dense(h, 512L, activation = "relu")
    r <- ggml_layer_dense(r, 512L, activation = NULL)
    h <- ggml_layer_add(list(h, r))
  }
  out <- ggml_layer_dense(h, 1L, activation = NULL)
  ggml_compile(ggml_model(inputs = inp, outputs = out), optimizer = "adam",
               loss = "mse", metrics = NULL, backend = "auto")
}

nets <- list(`2d` = build("2d"), flat = build("flat"))
data <- list(`2d` = as_2d, flat = as_flat)

## Each shape compiles its own graph on first use, and each batch size does
## too. Both are warmed here so no first-call cost lands in a measurement --
## the mistake that made the very first benchmark report 118 s for a 2 s solve.
for (k in names(nets)) {
  d <- data[[k]]
  sub <- if (length(dim(d)) == 3L) d[seq_len(BATCH), , , drop = FALSE]
         else d[seq_len(BATCH), , drop = FALSE]
  invisible(ggml_predict(nets[[k]], sub, batch_size = BATCH))
}

cat(sprintf("piece encoding, %d inputs per state | batch %d | %d reps\n\n",
            FLAT, BATCH, REPS))
cat(sprintf("%8s %12s %12s %10s %10s %8s\n",
            "states", "2d s", "flat s", "2d us/st", "fl us/st", "gain"))

rows <- list()
for (n in SIZES) {
  tt <- vapply(names(nets), function(k) {
    d <- data[[k]]
    sub <- if (length(dim(d)) == 3L) d[seq_len(n), , , drop = FALSE]
           else d[seq_len(n), , drop = FALSE]
    ## Repeated, not measured once: two earlier single measurements were
    ## misleading -- one lost to timer resolution, one to reading a
    ## correlation whose scale differed between the columns.
    t0 <- proc.time()[["elapsed"]]
    for (r in seq_len(REPS))
      invisible(ggml_predict(nets[[k]], sub, batch_size = BATCH))
    (proc.time()[["elapsed"]] - t0) / REPS
  }, numeric(1))
  cat(sprintf("%8d %12.3f %12.3f %10.1f %10.1f %7.1fx\n", n,
              tt[["2d"]], tt[["flat"]], 1e6 * tt[["2d"]] / n,
              1e6 * tt[["flat"]] / n, tt[["2d"]] / max(tt[["flat"]], 1e-9)))
  flush(stdout())
  rows[[length(rows) + 1L]] <- data.frame(n = n, two_d = tt[["2d"]],
                                          flat = tt[["flat"]])
}
res <- do.call(rbind, rows)

## ---- the profile, not just the total --------------------------------------
cat("\n== fixed cost and cost per state ==========================\n\n")
prof <- function(v) {
  f <- stats::lm(v ~ res$n)
  c(fixed = unname(stats::coef(f)[1L]), per = 1e6 * unname(stats::coef(f)[2L]))
}
p2 <- prof(res$two_d)
pf <- prof(res$flat)
cat(sprintf("%10s %14s %16s\n", "input", "fixed cost s", "us per state"))
cat(sprintf("%10s %14.3f %16.1f\n", "2d", p2[["fixed"]], p2[["per"]]))
cat(sprintf("%10s %14.3f %16.1f\n", "flat", pf[["fixed"]], pf[["per"]]))
cat(sprintf("\n  sticker, measured earlier: fixed 0.025 s, 3.1 us per state\n"))

cat("\n== reading it ============================================\n\n")
if (pf[["per"]] < p2[["per"]] / 5) {
  cat(sprintf("  The slope fell %.0fx, from %.1f to %.1f us per state.\n",
              p2[["per"]] / max(pf[["per"]], 1e-9), p2[["per"]], pf[["per"]]))
  if (pf[["fixed"]] > 0.3 * max(res$flat)) {
    cat("  And the fixed part now dominates, as it does for sticker: the flat\n")
    cat("  input has taken on the same profile, not merely a smaller version\n")
    cat("  of the old one. The 2-d input was the mechanism.\n")
  } else {
    cat("  The fixed part did not come to dominate the way sticker's does, so\n")
    cat("  the shape of the cost has not fully changed. Cheaper, and probably\n")
    cat("  the right change, but something else still scales with the states.\n")
  }
} else {
  cat(sprintf("  The slope went from %.1f to %.1f us per state -- not the\n",
              p2[["per"]], pf[["per"]]))
  cat("  order-of-magnitude fall the 2-d hypothesis predicts. Whatever costs\n")
  cat("  78 us a state, the tensor shape is not it, and the next place to look\n")
  cat("  is inside ggmlR at how a dense layer of this width is run.\n")
}
