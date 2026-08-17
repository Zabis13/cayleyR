#!/usr/bin/env Rscript
# Shorten a reduction solve by branching at random and re-solving.
#
# Pick a position on the current path, play one random move there, and solve the
# cube again from where that lands. If prefix + move + new solve is shorter than
# what we had, keep it. Repeat.
#
# There is no model in this. There was: the same search ran with the model
# choosing where to branch, scoring all 24 moves at every position and taking
# the ones that scored far better than the algorithm's own. It cut 28 moves off
# a 314-move solve in six minutes. Branching at random cuts 62 in under three
# seconds.
#
# The reason is visible in what was measured on the way. The model answers "how
# far is this state from solved", and the correlation between that answer and
# the moves actually saved came out at -0.24 -- the largest gain it ever reported
# produced the worst branch of the run. What decides a branch is not how near the
# state looks but how well cube_solve4 happens to handle it, and there is no
# reason those agree. Meanwhile a full solve costs 0.14 s against 21 ms for a
# single model call, so the exact answer is about six model calls' worth of time.
# Guessing is both slower and worse than measuring.
#
# Gains stop around fifty branches: three hundred found nothing that fifty had
# not. BUDGET is the one number worth turning.
#
# Then a depth-limited shortener sweeps the result, which takes out what
# branching structurally cannot -- see SHORTEN_DEPTH below.
#
# Solved means solved by COLOUR -- four indistinguishable centres per face mean a
# finished cube need not have its sticker numbers back in order.
#
# Run with:  Rscript inst/examples/demo_cube4_branch_random.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE <- 691L   # quarter turns walked away from solved
BUDGET   <- 20L    # branches tried; about the point where gains stop
N_CUBES  <- 10L
SEED     <- 2026L

# The branch search and the shortener take out different things. Branching
# changes which plan gets played and only ever accepts a whole solve that came
# out shorter; it cannot see a turn followed three moves later by its inverse.
# The shortener sees exactly that and nothing else, sweeping a window along the
# path and replacing any stretch a depth-limited search can do in fewer moves.
# Running one after the other takes out both.
#
# Depth 3 on a 24-move alphabet is 24^3 windows, which is quick; depth 4 and 5
# cost 24x and 576x that for progressively less.
SHORTEN_DEPTH <- 3L

# ---- run -------------------------------------------------------------------

set.seed(SEED)

g  <- cube_group(4)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
mn <- names(mv)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

shorten <- function(state, path, budget) {
  best <- path
  hits <- integer(0)          # where the accepted branches were, as a fraction
  for (i in seq_len(budget)) {
    p <- sample.int(length(best), 1L) - 1L        # branch after p moves
    s <- state
    for (m in best[seq_len(p)]) s <- s[mv[[m]]]
    mvname <- sample(mn, 1L)

    r <- try(cube_solve4(as.integer(s[mv[[mvname]]])), silent = TRUE)
    if (inherits(r, "try-error") || !isTRUE(r$found)) next

    total <- p + 1L + length(r$path)
    if (total < length(best)) {
      best <- c(best[seq_len(p)], mvname, r$path)
      hits <- c(hits, p)
    }
  }
  list(path = best, hits = hits)
}

cat(sprintf("scramble %d   budget %d   cubes %d\n\n", SCRAMBLE, BUDGET, N_CUBES))
cat(sprintf("%5s %9s %9s %9s %8s %8s %7s\n",
            "cube", "solve4", "branched", "shortened", "saved", "kept", "sec"))

rows <- list()
for (i in seq_len(N_CUBES)) {
  state <- generate_state(group = g, n_moves = SCRAMBLE)
  res   <- cube_solve4(state)
  if (!isTRUE(res$found)) { cat(sprintf("%5d  not solved\n", i)); next }

  t0  <- proc.time()[["elapsed"]]
  out <- shorten(state, res$path, BUDGET)
  sh  <- short_path_bfs(out$path, state, depth = SHORTEN_DEPTH, group = g)
  el  <- proc.time()[["elapsed"]] - t0

  final <- if (!is.null(sh$path)) sh$path else out$path
  ok <- cube_is_colour_solved(replay(state, final))
  n0 <- length(res$path); n1 <- length(out$path); n2 <- length(final)
  cat(sprintf("%5d %9d %9d %9d %8d %8d %7.1f%s\n", i, n0, n1, n2, n2 - n0,
              length(out$hits), el, if (ok) "" else "   NOT SOLVED"))
  flush.console()
  rows[[length(rows) + 1L]] <- data.frame(n0 = n0, n1 = n1, n2 = n2, sec = el,
                                          acc = length(out$hits), ok = ok)
}

if (length(rows)) {
  d <- do.call(rbind, rows)
  cat(sprintf("\nsolve4    : %.0f moves\n", mean(d$n0)))
  cat(sprintf("branching : %.0f  (%.1f%% off)\n", mean(d$n1),
              100 * (1 - mean(d$n1) / mean(d$n0))))
  cat(sprintf("+ bfs %d   : %.0f  (%.1f%% off in total)\n", SHORTEN_DEPTH,
              mean(d$n2), 100 * (1 - mean(d$n2) / mean(d$n0))))
  cat(sprintf("\n%.1fs per cube, %.1f branches kept, %d/%d verified\n",
              mean(d$sec), mean(d$acc), sum(d$ok), nrow(d)))
}
