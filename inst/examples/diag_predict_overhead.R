#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# What does the wrapper around ggml_predict cost?
#
# The number that started this: scoring 24000 states took the piece encoding
# 1.5 s through adi_value_of, at 78 us a state. Timing the same network's
# forward pass directly -- no wrapper -- gave 27 us a state for the very same
# 2-d input. Nothing about the encoding changed between those two measurements,
# so about two thirds of that 78 us was never in the network at all.
#
# adi_value_of does three things around the forward pass, and this times them
# apart:
#
#   encode   building the input from the states
#   pad      topping a short batch up to the compiled batch size
#   call     ggml_predict itself
#   unpack   as.numeric() and trimming the padding off the answer
#
# ---- One thing the earlier reasoning got wrong -----------------------------
#
# The padding branch only runs when there are FEWER states than the batch size.
# The 24000-state measurement had a batch of 256, so it never padded once, and
# an explanation resting on the cost of padding cannot be the explanation for
# that number. Both regimes are therefore measured here:
#
#   many   24000 states at batch 256 -- what a training iteration does, no
#          padding, ninety-odd calls
#   few    12 states at batch 256 -- what one step of A* does, padding 12 rows
#          out to 256, one call
#
# The second is where padding lives, and it is not a minor case: a solve runs
# it once per node expanded, thousands of times per cube.
#
# Both encodings are timed in both regimes, because the question is not what
# the wrapper costs but why it costs so much more for one encoding than the
# other.
#
# Usage:  Rscript diag_predict_overhead.R [reps]
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args  <- commandArgs(trailingOnly = TRUE)
REPS  <- if (length(args) >= 1) as.integer(args[1]) else 5L
BATCH <- 256L
MANY  <- 24000L
FEW   <- 12L

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g   <- cube_group(3, moves = FACES)
lay <- cube_piece_layout(g)

set.seed(5)
sc   <- cayleyR:::cube_adi_scramble(g$ptr, MANY, 14L)
pool <- sc$states

nets <- lapply(c("sticker", "piece"), function(e) {
  set.seed(99)
  cube_adi_model(g, embed_dim = 32L, hidden = c(512L), n_blocks = 3L,
                 arch = "resnet", encoding = e)
})
names(nets) <- c("sticker", "piece")

## Warm each network at this batch: the first call at a given size compiles the
## graph and its Vulkan pipelines, and that cost would otherwise be attributed
## to whichever encoding happened to run first.
for (e in names(nets)) {
  net <- nets[[e]]
  invisible(cayleyR:::adi_value_of(net$value, pool[seq_len(BATCH), , drop = FALSE],
                                   BATCH, net$arch, net$layout))
}

## The four steps, timed by doing them by hand in the same order adi_value_of
## does. `pad` is a no-op when there are more states than the batch, and is
## timed anyway so the table shows that rather than hiding it.
measure <- function(net, states, batch) {
  t <- c(encode = 0, pad = 0, call = 0, unpack = 0)
  for (r in seq_len(REPS)) {
    t0 <- proc.time()[["elapsed"]]
    x <- cayleyR:::adi_encode(states, net$arch, net$layout)
    t1 <- proc.time()[["elapsed"]]

    n <- dim(x)[1L]
    if (n < batch) {
      idx <- c(seq_len(n), rep(1L, batch - n))
      x <- if (length(dim(x)) == 3L) x[idx, , , drop = FALSE]
           else x[idx, , drop = FALSE]
    }
    t2 <- proc.time()[["elapsed"]]

    out <- ggml_predict(net$value, x, batch_size = batch)
    t3 <- proc.time()[["elapsed"]]

    out <- matrix(as.numeric(out), ncol = 1L)
    invisible(as.numeric(out[seq_len(n), , drop = FALSE]))
    t4 <- proc.time()[["elapsed"]]

    t <- t + c(t1 - t0, t2 - t1, t3 - t2, t4 - t3)
  }
  t / REPS
}

report <- function(label, n_states) {
  states <- pool[seq_len(n_states), , drop = FALSE]
  cat(sprintf("== %s: %d states, batch %d, %d call%s ==\n\n", label, n_states,
              BATCH, ceiling(n_states / BATCH),
              if (ceiling(n_states / BATCH) == 1L) "" else "s"))
  a <- measure(nets$sticker, states, BATCH)
  b <- measure(nets$piece,   states, BATCH)

  ## The whole call, measured as one, so the parts can be checked against it:
  ## if they do not add up, the breakdown is missing something.
  whole <- vapply(names(nets), function(e) {
    net <- nets[[e]]
    t0 <- proc.time()[["elapsed"]]
    for (r in seq_len(REPS))
      invisible(cayleyR:::adi_value_of(net$value, states, BATCH, net$arch,
                                       net$layout))
    (proc.time()[["elapsed"]] - t0) / REPS
  }, numeric(1))

  cat(sprintf("%10s %12s %12s %8s\n", "step", "sticker s", "piece s", "ratio"))
  for (k in names(a))
    cat(sprintf("%10s %12.4f %12.4f %8.1fx\n", k, a[[k]], b[[k]],
                b[[k]] / max(a[[k]], 1e-9)))
  cat(sprintf("%10s %12.4f %12.4f %8.1fx\n", "sum", sum(a), sum(b),
              sum(b) / max(sum(a), 1e-9)))
  cat(sprintf("%10s %12.4f %12.4f %8.1fx   <- adi_value_of end to end\n",
              "measured", whole[["sticker"]], whole[["piece"]],
              whole[["piece"]] / max(whole[["sticker"]], 1e-9)))

  gap <- whole - c(sum(a), sum(b))
  if (any(gap > 0.2 * whole))
    cat("\n  The parts do not account for the whole: something outside these\n  four steps is taking the difference.\n")
  cat("\n")
  invisible(list(parts = list(sticker = a, piece = b), whole = whole))
}

cat(sprintf("piece input %d values per state, sticker %d\n\n",
            lay$n_piece * lay$n_piece * lay$width, 54L))

m <- report("training regime, no padding", MANY)
f <- report("solve regime, padding 12 to 256", FEW)

cat("== what to conclude ======================================\n\n")
dom <- function(p) names(which.max(p))
cat(sprintf("  many states: the largest step is %s for sticker, %s for piece\n",
            dom(m$parts$sticker), dom(m$parts$piece)))
cat(sprintf("  few states:  the largest step is %s for sticker, %s for piece\n",
            dom(f$parts$sticker), dom(f$parts$piece)))
cat("\n  If `call` dominates both encodings and the ratio there matches the\n")
cat("  ratio of the whole, the wrapper is innocent and the cost is the\n")
cat("  forward pass. If a step around it dominates instead, that step is\n")
cat("  what to fix, and the network need not be touched at all.\n")

cat(sprintf("\n  For scale: a solve expands thousands of nodes, one `few` call\n"))
cat(sprintf("  each. At the rates above that is %.1f s per thousand nodes for\n",
            1000 * f$whole[["piece"]]))
cat(sprintf("  piece against %.1f s for sticker.\n",
            1000 * f$whole[["sticker"]]))
