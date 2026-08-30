#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# How big should the network be?
#
# DeepCubeA's own answer -- 5000 wide, then 1000, then four residual blocks of
# 1000, about 13M weights -- was reached with ten billion training states on a
# rack of GPUs. One card and an overnight run is nearer a hundred million, a
# hundredfold less, and at that budget a bigger network is not automatically a
# better one: the weights have to be paid for out of the same data.
#
# So the three sizes are given the same WALL CLOCK, not the same number of
# iterations. Iterations are what the big one gets fewer of, and hiding that
# would be measuring nothing: the question is not which network is better per
# step, it is which is better per minute, because minutes are what is short.
#
# ---- What is compared ------------------------------------------------------
#
# Not the training loss. ADI's loss is computed against targets that move as
# the frozen network is refreshed, so it falls for reasons that have nothing to
# do with the network being right, and a network whose targets have collapsed
# to "everything is one move away" shows an excellent loss. It is reported
# below, but only as a diagnostic.
#
# What is compared is what the network is for: how a search does when steered
# by it. Same cubes for all three, same weight, same node cap. Nodes expanded
# is the sharper of the two numbers -- solve rates saturate at these depths,
# while nodes keep separating the heuristics well after that.
#
# ---- The value of a scrambled cube -----------------------------------------
#
# One more diagnostic, cheap and worth more than the loss: the mean predicted
# distance at each scramble depth. A useful heuristic RISES with depth. One
# that has collapsed answers about the same everywhere, and a search steered by
# it is doing breadth-first with extra steps. This is the table that says
# whether five minutes was enough for a given size to start working at all.
#
# Usage:  Rscript bench_cube3_sizes.R [minutes] [depth] [cubes]
#   e.g.  Rscript bench_cube3_sizes.R 5 20 8
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args    <- commandArgs(trailingOnly = TRUE)
MINUTES <- if (length(args) >= 1) as.numeric(args[1]) else 5
DEPTH   <- if (length(args) >= 2) as.integer(args[2]) else 20L
CUBES   <- if (length(args) >= 3) as.integer(args[3]) else 8L

## batch_size is what ggml_fit cuts the training data into, so batch_states has
## to be a multiple of it and not the other way round: 2000 states at a batch
## of 2048 makes no complete batch at all and ggml_fit stops on ndata > 0.
##
## 2048 came from diag_score_batch.R, where it was the cheapest size measured
## -- but what that script timed was scoring the CHILDREN, twelve per state, so
## it was really measuring a batch of 24000. Applying its answer to the 2000
## states of the fit step was reading the result off the wrong axis.
BATCH_STATES <- 8192L
BATCH_SIZE   <- 2048L
stopifnot(BATCH_STATES %% BATCH_SIZE == 0L)
WEIGHT       <- 0.6
ASTAR_BATCH  <- 200L
MAX_NODES    <- 60000L
TESTD        <- c(10L, 14L, 18L)

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g   <- cube_group(3, moves = FACES)
id  <- group_identity(g)
tbl <- cube_moves(3)

## Every residual block is hidden[1] wide -- a skip connection adds a tensor to
## its own input, so the two have to be the same shape -- and cube_adi_model's
## resnet reads nothing but hidden[1]. So a size here is one width and a count
## of blocks, and asking for c(5000, 1000) would not build DeepCubeA's tapering
## stack: it would build 5000-wide blocks, twenty times the weights intended.
## The wide-then-narrow shape would need a change to cube_adi_model, which is
## not something a benchmark should be quietly doing to the thing it measures.
CONFIGS <- list(
  small  = list(hidden = 512L,  blocks = 3L),
  medium = list(hidden = 1000L, blocks = 4L),
  large  = list(hidden = 2000L, blocks = 4L)
)

lay <- cube_piece_layout(g)

## Weights, counted the way the model actually builds them: the projection from
## the input, then two square layers per block, then the head.
size_of <- function(cfg) {
  w <- cfg$hidden[1L]
  n_in <- lay$n_piece * lay$n_piece * lay$width
  n_in * w + 2 * w * w * cfg$blocks + w
}

cat(sprintf("cube 3x3x3, %d face turns | piece encoding, %d inputs\n",
            length(FACES), lay$n_piece * lay$n_piece * lay$width))
cat(sprintf("%.0f minutes per size, scrambles 1..%d, batch %d\n\n",
            MINUTES, DEPTH, BATCH_SIZE))

## One set of cubes for every size. At eight cubes a depth the luck of the draw
## is not a small effect, and reusing them takes it out of the comparison.
set.seed(2024)
cubes <- lapply(TESTD, function(d)
  lapply(seq_len(CUBES), function(k) {
    s <- id
    for (m in sample(FACES, d, replace = TRUE)) s <- s[tbl[[m]]]
    s
  }))
names(cubes) <- as.character(TESTD)

## A fixed sample for the depth table, scrambled once and scored by each
## network at the end of its training.
set.seed(31)
probe_d <- rep(seq(2L, DEPTH, length.out = 6L), each = 60L)
probe_d <- as.integer(round(probe_d))
probe <- t(vapply(probe_d, function(d) {
  s <- id
  for (m in sample(FACES, d, replace = TRUE)) s <- s[tbl[[m]]]
  s
}, integer(54L)))

train_for <- function(name, cfg) {
  set.seed(99)
  net <- cube_adi_model(g, hidden = c(cfg$hidden), n_blocks = cfg$blocks,
                        arch = "resnet", encoding = "piece")
  cat(sprintf("-- %s: hidden %s, %d blocks, ~%.1fM weights --\n", name,
              paste(cfg$hidden, collapse = "+"), cfg$blocks,
              size_of(cfg) / 1e6))
  flush(stdout())

  deadline <- proc.time()[["elapsed"]] + MINUTES * 60
  iters <- 0L
  last_report <- proc.time()[["elapsed"]]
  repeat {
    ## Ten iterations at a time: enough that the clock is checked often, few
    ## enough that the check costs nothing. Training resumes from the weights
    ## returned, so the blocks are one run split up, not separate runs.
    net <- cube_adi_train(net, iterations = 10L, batch_states = BATCH_STATES,
                          max_depth = DEPTH, batch_size = BATCH_SIZE,
                          verbose = FALSE)
    iters <- iters + 10L
    now <- proc.time()[["elapsed"]]
    if (now - last_report > 30) {
      cat(sprintf("   %5d iters | %5.0f s left | value loss %.4f\n", iters,
                  deadline - now, utils::tail(net$history$value_loss, 1L)))
      flush(stdout())
      last_report <- now
    }
    if (now >= deadline) break
  }
  cat(sprintf("   %d iterations, %d states seen\n\n", iters,
              iters * BATCH_STATES))
  list(net = net, iters = iters)
}

evaluate <- function(net, name) {
  ## Does the estimate rise with depth? If it does not, nothing below matters.
  v <- cayleyR:::adi_value_of(net$value, probe, BATCH_SIZE, net$arch,
                              net$layout)
  cat(sprintf("   predicted distance by scramble depth:\n"))
  ds <- sort(unique(probe_d))
  means <- vapply(ds, function(d) mean(v[probe_d == d]), numeric(1))
  for (i in seq_along(ds))
    cat(sprintf("     %2d moves -> %6.2f%s\n", ds[i], means[i],
                if (i > 1L) sprintf("   step %+.2f", means[i] - means[i - 1L])
                else ""))
  if (all(diff(means) < 0.05))
    cat("     (flat: this network is not telling depths apart yet)\n")

  rows <- list()
  for (i in seq_along(TESTD)) {
    d <- TESTD[i]
    ok <- 0L; len <- 0; nodes <- 0; secs <- 0
    for (k in seq_len(CUBES)) {
      t0 <- proc.time()[["elapsed"]]
      r <- cube_adi_astar(net, cubes[[i]][[k]], weight = WEIGHT,
                          batch = ASTAR_BATCH, max_nodes = MAX_NODES,
                          batch_size = BATCH_SIZE)
      secs <- secs + proc.time()[["elapsed"]] - t0
      if (r$solved) { ok <- ok + 1L; len <- len + r$length; nodes <- nodes + r$nodes }
      cat(sprintf("\r   depth %2d: cube %d/%d", d, k, CUBES)); flush(stdout())
    }
    cat("\r", strrep(" ", 40), "\r", sep = "")
    cat(sprintf("   depth %2d: solved %d/%-2d | mean %5.1f moves | %6.0f nodes | %5.0f s\n",
                d, ok, CUBES, if (ok) len / ok else NA_real_,
                if (ok) nodes / ok else NA_real_, secs))
    flush(stdout())
    rows[[length(rows) + 1L]] <- data.frame(
      config = name, depth = d, solved = ok,
      mean_len = if (ok) len / ok else NA_real_,
      mean_nodes = if (ok) nodes / ok else NA_real_, secs = secs)
  }
  cat("\n")
  do.call(rbind, rows)
}

out <- list()
for (nm in names(CONFIGS)) {
  tr <- train_for(nm, CONFIGS[[nm]])
  res <- evaluate(tr$net, nm)
  res$iters <- tr$iters
  out[[nm]] <- res
}
res <- do.call(rbind, out)

cat("== side by side ==========================================\n\n")
cat(sprintf("%8s", "depth"))
for (nm in names(CONFIGS)) cat(sprintf(" | %-20s", nm))
cat("\n")
cat(sprintf("%8s", ""))
for (nm in names(CONFIGS)) cat(sprintf(" | %6s %6s %6s", "solved", "moves", "nodes"))
cat("\n")
for (d in TESTD) {
  cat(sprintf("%8d", d))
  for (nm in names(CONFIGS)) {
    r <- res[res$config == nm & res$depth == d, ]
    cat(sprintf(" | %4d/%-2d %6.1f %6.0f", r$solved, CUBES, r$mean_len,
                r$mean_nodes))
  }
  cat("\n")
}

cat("\n")
for (nm in names(CONFIGS)) {
  r <- res[res$config == nm, ]
  cat(sprintf("  %-7s %5d iterations, %2d/%d solved, %6.0f nodes on average\n",
              nm, r$iters[1L], sum(r$solved), length(TESTD) * CUBES,
              mean(r$mean_nodes, na.rm = TRUE)))
}

cat("\n  Five minutes is short enough that a size can lose here for being slow\n")
cat("  to start rather than for being the wrong size. Read the depth table\n")
cat("  first: a network whose predictions are still flat has not begun, and\n")
cat("  its search numbers say nothing about how it would end up.\n")
