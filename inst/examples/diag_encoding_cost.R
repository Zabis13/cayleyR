#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Where the piece encoding spends its time.
#
# bench_cube3_encoding.R showed the piece encoding guiding the search better
# and training about sixteen times slower. Moving the encoder into C++ took
# only a fifth off that, so the encoder was not the cost, and the remaining
# explanation -- "the wider input costs more" -- is a guess. This measures it
# instead.
#
# An ADI iteration is five steps, and they are timed separately here rather
# than as a total:
#
#   scramble   generating the batch                  (C++, encoding-blind)
#   children   expanding every move of every state   (C++, encoding-blind)
#   score      the frozen network over the children  (24k states forward)
#   encode     turning the batch into network input
#   fit        one pass of value and one of policy   (2k states, fwd + bwd)
#
# Two of them cannot depend on the encoding at all, and they are timed anyway:
# if they differ between the two runs, the difference is noise in the machine
# rather than anything about encodings, and that sets the scale for reading the
# three that can.
#
# The expected shape of the answer: `score` is twelve times as many states as
# `fit` and no backward pass, so on a batch this size it is usually the larger
# of the two. If the piece encoding is slower in `score` and `fit` in the same
# proportion, the cost is the input width and nothing is wrong. If it is slow
# in one and not the other, something in how ggmlR handles the 2-d input is,
# and that is worth chasing before any longer training run.
#
# Usage:  Rscript diag_encoding_cost.R [iters] [batch_states] [depth]
#   e.g.  Rscript diag_encoding_cost.R 20 2000 14
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args   <- commandArgs(trailingOnly = TRUE)
ITERS  <- if (length(args) >= 1) as.integer(args[1]) else 20L
NSTATE <- if (length(args) >= 2) as.integer(args[2]) else 2000L
DEPTH  <- if (length(args) >= 3) as.integer(args[3]) else 14L
BATCH  <- 256L

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g <- cube_group(3, moves = FACES)

cat(sprintf("timing an ADI iteration | %d iters x %d states, depth 1..%d\n\n",
            ITERS, NSTATE, DEPTH))

time_one <- function(encoding) {
  set.seed(99)
  net <- cube_adi_model(g, embed_dim = 32L, hidden = c(512L), n_blocks = 3L,
                        arch = "resnet", encoding = encoding)

  ## One iteration before the clock starts. The first call to a compiled graph
  ## builds Vulkan pipelines, which is a one-off cost of seconds and would
  ## otherwise land entirely on whichever encoding ran first.
  net <- cube_adi_train(net, iterations = 1L, batch_states = NSTATE,
                        max_depth = DEPTH, batch_size = BATCH, verbose = FALSE)

  frozen  <- net$value
  n_moves <- net$n_moves
  t <- c(scramble = 0, children = 0, score = 0, encode = 0, fit = 0)

  for (it in seq_len(ITERS)) {
    t0 <- proc.time()[["elapsed"]]
    sc <- cayleyR:::cube_adi_scramble(g$ptr, NSTATE, DEPTH)
    t1 <- proc.time()[["elapsed"]]; t["scramble"] <- t["scramble"] + t1 - t0

    ch <- cayleyR:::cube_adi_children(g$ptr, sc$states)
    t2 <- proc.time()[["elapsed"]]; t["children"] <- t["children"] + t2 - t1

    child_v <- cayleyR:::adi_value_of(frozen, ch$children, BATCH, net$arch,
                                      net$layout)
    t3 <- proc.time()[["elapsed"]]; t["score"] <- t["score"] + t3 - t2

    tg <- cayleyR:::cube_adi_targets(child_v, ch$solved, n_moves)
    x  <- cayleyR:::adi_encode(sc$states, net$arch, net$layout)
    y_value  <- matrix(tg$value, ncol = 1L)
    y_policy <- matrix(0, nrow = dim(x)[1L], ncol = n_moves)
    y_policy[cbind(seq_len(dim(x)[1L]), tg$policy)] <- 1
    t4 <- proc.time()[["elapsed"]]; t["encode"] <- t["encode"] + t4 - t3

    net$value  <- ggml_fit(net$value, x, y_value, epochs = 1L,
                           batch_size = BATCH, verbose = 0L)
    net$policy <- ggml_fit(net$policy, x, y_policy, epochs = 1L,
                           batch_size = BATCH, verbose = 0L)
    t5 <- proc.time()[["elapsed"]]; t["fit"] <- t["fit"] + t5 - t4

    cat(sprintf("\r  %s: %d/%d", encoding, it, ITERS)); flush(stdout())
  }
  cat("\r", strrep(" ", 40), "\r", sep = "")
  t / ITERS
}

a <- time_one("sticker")
b <- time_one("piece")

cat("== seconds per iteration =================================\n\n")
cat(sprintf("%10s %10s %10s %8s\n", "step", "sticker", "piece", "ratio"))
for (k in names(a))
  cat(sprintf("%10s %10.3f %10.3f %8.1fx\n", k, a[[k]], b[[k]],
              b[[k]] / max(a[[k]], 1e-6)))
cat(sprintf("%10s %10.3f %10.3f %8.1fx\n", "total", sum(a), sum(b),
            sum(b) / sum(a)))

cat("\n  Read `scramble` and `children` first: neither touches the network, so\n")
cat("  whatever ratio they show is the noise floor for the other three.\n")

## The input each network actually sees, so the numbers above have something to
## be divided by.
lay <- cube_piece_layout(g)
cat(sprintf("\n  input widths: sticker %d values (54 tokens x %d embedding),",
            54L * 32L, 32L))
cat(sprintf(" piece %d values (%d slots x %d)\n",
            lay$n_piece * lay$n_piece * lay$width, lay$n_piece,
            lay$n_piece * lay$width))
cat(sprintf("  states per step: score %d, fit %d\n",
            NSTATE * length(FACES), NSTATE))
