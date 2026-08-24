#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# The scramble against the solve, one move at a time.
#
# A solved cube is walked away from by random moves, and every position along
# the way is solved from scratch. The walk knows the way back in as many moves
# as it took; the solver does not, and takes hundreds. What the run shows is
# where that gap opens, and what happens to a solution when the very same cube
# is made one move easier.
#
# The positions are printed from the far end inwards -- depth 5, then 4, then
# 3 -- so consecutive blocks are the same cube with one move of the scramble
# taken off. Two numbers say how the solver reacted:
#
#   one move less   how the length changed. An easier cube should want a
#                   shorter word; where it wants a longer one, the solver went
#                   down a different route entirely.
#   shared prefix   how many opening moves the two solutions have in common.
#                   A short prefix on nearly identical cubes means the
#                   reduction is not stable -- close inputs, unrelated outputs.
#
# Measured on a 4x4x4, seed 1: solutions track the walk move for move out to
# seven, and at eight the length jumps from 11 to 288. That is where the cube
# stops being near-solved and the full reduction takes over, and it is also
# where a parity stage first appears.
#
# The method is red+kociemba -- reduction to a 3x3x3, then the two-phase
# search on top. Over a hundred random 4x4x4 cubes it is 251 moves against
# CFOP's 315, shorter on every one of them.
#
# Usage:  Rscript diag_scramble_vs_solve.R [n] [depth] [seed] [trials]
#   e.g.  Rscript diag_scramble_vs_solve.R 4 5 1        one walk, in full
#         Rscript diag_scramble_vs_solve.R 4 12 1 20    twenty walks, summary
# ---------------------------------------------------------------------------

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
N      <- if (length(args) >= 1) as.integer(args[1]) else 4L
DEPTH  <- if (length(args) >= 2) as.integer(args[2]) else 5L
SEED   <- if (length(args) >= 3) as.integer(args[3]) else 1L
TRIALS <- if (length(args) >= 4) as.integer(args[4]) else 1L
VERBOSE <- TRIALS == 1L

g  <- cube_group(N)
id <- group_identity(g)
mv <- cube_moves(N); names(mv) <- cube_move_names(N)
nm <- names(mv)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

invert1 <- function(m)
  if (grepl("'", m, fixed = TRUE)) sub("'", "", m, fixed = TRUE) else paste0(m, "'")
invert <- function(w) rev(vapply(w, invert1, character(1), USE.NAMES = FALSE))

## ---- the solve, kept in its stages ---------------------------------------
solve_one <- function(state) {
  t0 <- proc.time()[["elapsed"]]
  r <- try(cube_solve4(state, method = "kociemba"), silent = TRUE)
  el <- proc.time()[["elapsed"]] - t0
  if (inherits(r, "try-error") || !isTRUE(r$found))
    return(list(ok = FALSE, secs = el))

  # cube_solve4 reports its stages as a data.frame of name, detail and n_moves.
  # The words themselves are not carried per stage -- that is cube_solve_centres
  # with its "moves" attribute -- so they are cut out of the whole path by the
  # lengths, which is the same thing as long as the stages are in order and
  # their lengths add up. Checked below rather than assumed.
  stages <- r$stages
  ends <- cumsum(stages$n_moves)
  starts <- c(1L, head(ends, -1L) + 1L)
  words <- Map(function(a, b) if (b >= a) r$path[a:b] else character(0),
               starts, ends)
  if (sum(stages$n_moves) != length(r$path))
    warning("stage lengths do not add up to the path: ",
            sum(stages$n_moves), " against ", length(r$path))
  list(ok = TRUE, path = r$path, secs = el, stages = stages, words = words)
}

## ---- one walk ------------------------------------------------------------
walk <- function(seed) {
  set.seed(seed)
  scramble <- sample(nm, DEPTH, replace = TRUE)

  states <- vector("list", DEPTH + 1L)
  states[[1]] <- id
  for (k in seq_len(DEPTH)) states[[k + 1L]] <- replay(states[[k]], scramble[k])

  rows <- vector("list", DEPTH)
  for (k in seq_len(DEPTH)) {
    st <- states[[k + 1L]]
    res <- solve_one(st)
    # the way the scramble itself came back: undo the k moves that made it
    back <- invert(scramble[seq_len(k)])
    rows[[k]] <- list(depth = k, scramble = scramble[seq_len(k)],
                      back = back, res = res)
  }
  list(scramble = scramble, rows = rows)
}

## ---- report --------------------------------------------------------------
if (VERBOSE) {
  w <- walk(SEED)
  cat(sprintf("scramble vs solve | n=%d, depth %d, seed %d\n", N, DEPTH, SEED))
  cat(sprintf("method: red+kociemba\n\n"))
  cat(sprintf("the walk: %s\n\n", paste(w$scramble, collapse = " ")))
  cat("Printed from the far end inwards: the same cube with one move of the\n")
  cat("scramble taken off each time, so the solutions can be read against one\n")
  cat("another. A method that is stable would answer a slightly easier cube\n")
  cat("with a slightly shorter word; where the length jumps instead, the two\n")
  cat("positions went down different routes.\n\n")

  prev <- NULL
  for (row in rev(w$rows)) {
    cat(sprintf("== %d move%s from solved =================================\n",
                row$depth, if (row$depth > 1) "s" else ""))
    cat(sprintf("  scrambled by : %s\n", paste(row$scramble, collapse = " ")))
    cat(sprintf("  undone by    : %s   (%d moves)\n",
                paste(row$back, collapse = " "), length(row$back)))

    r <- row$res
    if (!isTRUE(r$ok)) { cat("  solver       : FAILED\n\n"); prev <- NULL; next }

    cat(sprintf("  solver found : %d moves in %.2f s   -- %.0fx the walk\n",
                length(r$path), r$secs, length(r$path) / row$depth))

    # against the cube one move harder, which was printed just above
    if (!is.null(prev)) {
      d <- length(r$path) - prev$len
      cat(sprintf("  one move less: %+d moves than at depth %d   (%s)\n",
                  d, prev$depth,
                  if (abs(d) <= 5) "much the same route"
                  else if (d < 0) "a shorter route opened"
                  else "a longer route was taken"))
      cat(sprintf("  shared prefix: %d moves\n",
                  {
                    k <- 0L
                    while (k < min(length(r$path), length(prev$path)) &&
                           r$path[k + 1L] == prev$path[k + 1L]) k <- k + 1L
                    k
                  }))
    }

    cat("  by stage:\n")
    for (i in seq_len(nrow(r$stages))) {
      word <- r$words[[i]]
      cat(sprintf("    %-12s %-22s %4d moves\n", r$stages$name[i],
                  r$stages$detail[i], length(word)))
    }

    # the moves themselves, wrapped, with a marker between stages
    cat("  path:\n")
    for (i in seq_len(nrow(r$stages))) {
      word <- r$words[[i]]
      if (!length(word)) next
      cat(sprintf("    [%s]\n", r$stages$name[i]))
      for (j in seq(1, length(word), by = 16)) {
        chunk <- word[j:min(j + 15L, length(word))]
        cat("      ", paste(chunk, collapse = " "), "\n", sep = "")
      }
    }

    # a sanity check the report would be worthless without
    cat(sprintf("  verified     : %s\n\n",
                cube_is_colour_solved(replay(replay(id, row$scramble), r$path))))
    prev <- list(len = length(r$path), path = r$path, depth = row$depth)
    flush(stdout())
  }

} else {
  cat(sprintf("scramble vs solve | n=%d, depth %d, seeds %d..%d, %d walks\n",
              N, DEPTH, SEED, SEED + TRIALS - 1L, TRIALS))
  cat("method: red+kociemba\n\n")
  cat(sprintf("%6s %8s %9s %8s %8s %8s\n", "depth", "walk", "solver",
              "ratio", "reduce", "3x3x3"))

  acc <- matrix(NA_real_, nrow = DEPTH, ncol = 4,
                dimnames = list(NULL, c("solver", "reduce", "cube3", "secs")))
  counts <- integer(DEPTH)

  for (t in seq_len(TRIALS)) {
    w <- walk(SEED + t - 1L)
    for (row in w$rows) {
      r <- row$res
      if (!isTRUE(r$ok)) next
      k <- row$depth
      red <- sum(vapply(seq_len(nrow(r$stages)), function(i)
        if (r$stages$name[i] != "3x3x3") length(r$words[[i]]) else 0L,
        integer(1)))
      c3 <- length(r$path) - red
      counts[k] <- counts[k] + 1L
      acc[k, ] <- c(
        sum(c(acc[k, "solver"], length(r$path)), na.rm = TRUE),
        sum(c(acc[k, "reduce"], red), na.rm = TRUE),
        sum(c(acc[k, "cube3"], c3), na.rm = TRUE),
        sum(c(acc[k, "secs"], r$secs), na.rm = TRUE))
    }
  }

  for (k in seq_len(DEPTH)) {
    if (!counts[k]) next
    a <- acc[k, ] / counts[k]
    cat(sprintf("%6d %8d %9.0f %8.0fx %8.0f %8.0f\n", k, k, a["solver"],
                a["solver"] / k, a["reduce"], a["cube3"]))
  }
  cat("\n  walk    the moves it took to scramble, and so to undo\n")
  cat("  solver  what red+kociemba returns, knowing nothing of the walk\n")
  cat("  reduce  of that, the reduction to a 3x3x3\n")
  cat("  3x3x3   and the two-phase solve on top of it\n")
}
