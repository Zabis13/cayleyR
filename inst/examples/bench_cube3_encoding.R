#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Does the piece encoding earn its place?
#
# Two networks, same architecture, same training budget, same cubes, differing
# only in how a state reaches them:
#
#   sticker -- 54 positions as tokens through an embedding, what cube_adi has
#              always done
#   piece   -- DeepCubeA's input: one one-hot per piece slot saying which of
#              the 20 pieces is in it and how it is turned
#
# The claim being tested is that telling the network which stickers belong to
# one physical piece beats making it work that out. The claim is not obviously
# true -- the embedding could learn the grouping in a few iterations and the
# extra inputs could cost more than they give -- so it is measured.
#
# The moves are the six face turns only. That is DeepCubeA's puzzle: slices
# add nothing to the reachable states (M is R L' plus a rotation of the whole
# cube) and a third more branching to every search. It also fixes the centres,
# which is what lets the layout drop them and leaves exactly 20 pieces.
#
# What to read: solved counts first, then nodes. A better heuristic shows up
# as fewer nodes expanded for the same solve, more clearly than as a shorter
# solution -- the weight, not the network, is what decides length.
#
# ---- Why the shallow depths are gone ---------------------------------------
#
# An earlier run of this script tested depths 5 and 8 as well, and both
# encodings returned the same node counts to the digit -- 447 and 887 -- along
# with the same mean lengths. Two different networks do not agree that exactly
# by chance: at those depths the search reaches the goal before the heuristic
# has had a chance to steer it anywhere, so the rows measured the branching
# factor and not the network. They are dropped rather than kept as filler: a
# row that cannot distinguish the things being compared is not evidence that
# they are alike.
#
# Depths start at 10 for that reason. Where the floor should be is itself a
# property of the beam and the cube rather than something to fix once, so if a
# run comes back with two encodings matching exactly on its shallowest row,
# that row has fallen below the floor again and should go.
#
# Usage:  Rscript bench_cube3_encoding.R [iters] [depth] [cubes] [test_depths]
#   e.g.  Rscript bench_cube3_encoding.R 300 14 10
#         Rscript bench_cube3_encoding.R 800 20 20 "10,15,20"
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args   <- commandArgs(trailingOnly = TRUE)
ITERS  <- if (length(args) >= 1) as.integer(args[1]) else 300L
DEPTH  <- if (length(args) >= 2) as.integer(args[2]) else 14L
CUBES  <- if (length(args) >= 3) as.integer(args[3]) else 10L
## Below depth 10 the search finishes before the heuristic matters and both
## encodings return identical numbers; see the header.
TESTD  <- if (length(args) >= 4) as.integer(strsplit(args[4], ",")[[1]]) else
  sort(unique(c(10L, 12L, DEPTH)))

BATCH_STATES <- 2000L
BATCH_SIZE   <- 256L
WEIGHT       <- 0.6
ASTAR_BATCH  <- 200L
MAX_NODES    <- 60000L

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g   <- cube_group(3, moves = FACES)
id  <- group_identity(g)
tbl <- cube_moves(3)

cat(sprintf("cube 3x3x3, %d face turns | ADI %d iters x %d states, depths 1..%d\n",
            length(FACES), ITERS, BATCH_STATES, DEPTH))
cat(sprintf("A*: weight %.1f, batch %d, node cap %d | %d cubes per depth\n\n",
            WEIGHT, ASTAR_BATCH, MAX_NODES, CUBES))

## The same cubes go to both networks. Scrambling once and reusing removes the
## luck of the draw from the comparison, which at ten cubes a depth is not a
## small effect.
set.seed(2024)
cubes <- lapply(TESTD, function(d) {
  lapply(seq_len(CUBES), function(k) {
    s <- id
    for (m in sample(FACES, d, replace = TRUE)) s <- s[tbl[[m]]]
    s
  })
})
names(cubes) <- as.character(TESTD)

train_one <- function(encoding) {
  ## Seeded per encoding rather than once for the run: both networks then start
  ## from the same random draw, so what differs between them is the encoding
  ## and not the initial weights.
  set.seed(99)
  net <- cube_adi_model(g, embed_dim = 32L, hidden = c(512L), n_blocks = 3L,
                        arch = "resnet", encoding = encoding)
  cat(sprintf("-- %s encoding --\n", encoding))
  print(net)
  t0 <- proc.time()[["elapsed"]]
  every <- max(1L, ITERS %/% 10L)
  done <- 0L
  while (done < ITERS) {
    n_now <- min(every, ITERS - done)
    net <- cube_adi_train(net, iterations = n_now, batch_states = BATCH_STATES,
                          max_depth = DEPTH, batch_size = BATCH_SIZE,
                          verbose = FALSE)
    done <- done + n_now
    h <- net$history
    cat(sprintf("  %4d/%d iters | value loss %.4f | %.0f s\n", done, ITERS,
                utils::tail(h$value_loss, 1L),
                proc.time()[["elapsed"]] - t0))
    flush(stdout())
  }
  cat(sprintf("  trained in %.0f s\n\n", proc.time()[["elapsed"]] - t0))
  net
}

evaluate <- function(net, label) {
  cat(sprintf("-- %s: solving --\n", label))
  rows <- list()
  for (i in seq_along(TESTD)) {
    d <- TESTD[i]
    ok <- 0L; len <- 0; nodes <- 0; secs <- 0
    for (k in seq_len(CUBES)) {
      t0 <- proc.time()[["elapsed"]]
      r <- cube_adi_astar(net, cubes[[i]][[k]], weight = WEIGHT,
                          batch = ASTAR_BATCH, max_nodes = MAX_NODES,
                          batch_size = BATCH_SIZE)
      el <- proc.time()[["elapsed"]] - t0
      secs <- secs + el
      if (r$solved) { ok <- ok + 1L; len <- len + r$length; nodes <- nodes + r$nodes }
      cat(sprintf("\r  depth %2d: cube %2d/%d  %s        ", d, k, CUBES,
                  if (r$solved) sprintf("%2d moves, %5d nodes, %4.1f s",
                                        r$length, r$nodes, el)
                  else sprintf("unsolved at %d nodes", r$nodes)))
      flush(stdout())
    }
    cat("\r", strrep(" ", 60), "\r", sep = "")
    cat(sprintf("  depth %2d: solved %2d/%-2d | mean %5.1f moves | %6.0f nodes | %5.1f s\n",
                d, ok, CUBES, if (ok) len / ok else NA_real_,
                if (ok) nodes / ok else NA_real_, secs))
    flush(stdout())
    rows[[length(rows) + 1L]] <- data.frame(
      encoding = label, depth = d, solved = ok, cubes = CUBES,
      mean_len = if (ok) len / ok else NA_real_,
      mean_nodes = if (ok) nodes / ok else NA_real_, secs = secs)
  }
  cat("\n")
  do.call(rbind, rows)
}

res <- rbind(evaluate(train_one("sticker"), "sticker"),
             evaluate(train_one("piece"),   "piece"))

cat("== side by side ==========================================\n\n")
cat(sprintf("%6s | %-22s | %-22s\n", "", "sticker", "piece"))
cat(sprintf("%6s | %6s %6s %7s | %6s %6s %7s\n",
            "depth", "solved", "moves", "nodes", "solved", "moves", "nodes"))
for (d in TESTD) {
  a <- res[res$encoding == "sticker" & res$depth == d, ]
  b <- res[res$encoding == "piece"   & res$depth == d, ]
  cat(sprintf("%6d | %4d/%-2d %6.1f %7.0f | %4d/%-2d %6.1f %7.0f\n", d,
              a$solved, a$cubes, a$mean_len, a$mean_nodes,
              b$solved, b$cubes, b$mean_len, b$mean_nodes))
}

## A row where both encodings expand the same number of nodes is a row where
## the search never consulted them differently. Said here rather than left for
## the reader to notice, because it looks like agreement and is not.
flat <- vapply(TESTD, function(d) {
  a <- res[res$encoding == "sticker" & res$depth == d, ]
  b <- res[res$encoding == "piece"   & res$depth == d, ]
  isTRUE(a$solved == b$solved) && isTRUE(a$mean_nodes == b$mean_nodes)
}, logical(1))
if (any(flat))
  cat(sprintf("\n  note: depth %s gave identical counts for both encodings.\n  That row is below the depth where the heuristic steers; drop it.\n",
              paste(TESTD[flat], collapse = ", ")))

tot_a <- sum(res$solved[res$encoding == "sticker"])
tot_b <- sum(res$solved[res$encoding == "piece"])
cat(sprintf("\n  solved overall: sticker %d, piece %d (of %d)\n",
            tot_a, tot_b, length(TESTD) * CUBES))
cat("\n  Nodes is the column that reads the heuristic: at equal solve rates,\n")
cat("  the encoding that expands fewer nodes is the one guiding better.\n")
