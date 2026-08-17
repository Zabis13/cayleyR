#!/usr/bin/env Rscript
# Descend on the 4x4x4 by looking three moves ahead from one state.
#
# From any one state only about one move in twenty-four leads anywhere: measured
# twelve moves out, two of the twenty-four improved on the state at all and one
# improved by more than a move. That is why a width-1 search stalls -- it has to
# guess the one, and when it guesses wrong the next state may have no good move
# at all.
#
# Looking further ahead does not run into the wall you would expect. Two moves
# out, 44 of 576 states improve and 5 by more than a move; three moves out, 272
# of 9600 improve and 103 by more than a move. The good continuations grow about
# as fast as the tree does, so the fraction holds while the count multiplies --
# and the best of them lies steadily deeper: 1.27 better at one move, 2.65 at
# two, 3.85 at three.
#
# So a step here is not a move, it is the best endpoint of a three-move look, and
# the path grows three moves at a time. Against a beam this is the cheap way to
# buy the same descent: a beam of 5000 spends 120000 states per move and 360000
# to fall 3.85, where the same fall costs 9600 states from a single point.
#
# The middle ply is trimmed to WIDTH states, or the third would be a quarter of
# a million.
#
# Run with:  Rscript inst/examples/demo_cube4_model_deepen.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE  <- 12L      # quarter turns away from solved
N_CUBES   <- 1L

DEPTH     <- 3L       # plies looked ahead per step
WIDTH     <- 400L     # states carried from one ply to the next
MAX_STEPS <- 25L      # steps before giving up
MIN_GAIN  <- 0.5      # a step must improve by at least this, or the search stops

ARCHIVE   <- "/mnt/Data2/DS_projects/444/archive"
SEED      <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)
set.seed(SEED)

g  <- cube_group(4)
mv <- cube_moves(4)
names(mv) <- cube_move_names(4)
mn <- names(mv)
solved <- seq_len(96)

score <- function(M) {
  if (!is.matrix(M)) M <- matrix(as.integer(M), nrow = 1L)
  storage.mode(M) <- "integer"
  cayley_distance("cube4_model")(M, solved, 4L)
}

# Look DEPTH moves ahead and return the best state found with the moves that
# reach it. Each ply expands every state it is given by all 24 moves, scores the
# lot, and hands on the best WIDTH of them; the paths ride along so the winner
# can say how it got there.
look_ahead <- function(state) {
  states <- matrix(as.integer(state), nrow = 1L)
  paths  <- list(character(0))
  best   <- list(q = Inf, state = NULL, path = NULL)

  for (ply in seq_len(DEPTH)) {
    nb  <- nrow(states)
    nxt <- matrix(0L, nb * 24L, 96L)
    npath <- vector("list", nb * 24L)
    r <- 0L
    for (i in seq_len(nb)) {
      s <- states[i, ]
      for (j in seq_len(24L)) {
        r <- r + 1L
        nxt[r, ] <- s[mv[[j]]]
        npath[[r]] <- c(paths[[i]], mn[j])
      }
    }

    q <- score(nxt)

    # Solving outright beats any score: take it and stop looking.
    hit <- which(apply(nxt, 1L, function(s) identical(as.integer(s), solved)))
    if (length(hit)) {
      return(list(q = 0, state = solved, path = npath[[hit[1L]]], solved = TRUE))
    }

    j <- which.min(q)
    if (q[j] < best$q)
      best <- list(q = q[j], state = nxt[j, ], path = npath[[j]])

    if (ply < DEPTH) {
      keep   <- order(q)[seq_len(min(WIDTH, length(q)))]
      states <- nxt[keep, , drop = FALSE]
      paths  <- npath[keep]
    }
  }
  c(best, list(solved = FALSE))
}

descend <- function(state) {
  cur  <- as.integer(state)
  q    <- score(cur)
  path <- character(0)

  cat(sprintf("%5s %8s %8s %8s\n", "step", "q", "moves", "sec"))
  cat(sprintf("%5d %8.2f %8d %8s\n", 0L, q, 0L, "-"))

  for (step in seq_len(MAX_STEPS)) {
    t0 <- proc.time()[["elapsed"]]
    r  <- look_ahead(cur)
    el <- proc.time()[["elapsed"]] - t0

    if (isTRUE(r$solved)) {
      path <- c(path, r$path)
      cat(sprintf("%5d %8.2f %8d %8.0f  solved\n", step, 0, length(path), el))
      return(list(found = TRUE, path = path, steps = step))
    }
    if (r$q > q - MIN_GAIN)
      return(list(found = FALSE, why = "no descent found", path = path,
                  steps = step - 1L))

    cur  <- as.integer(r$state)
    q    <- r$q
    path <- c(path, r$path)
    cat(sprintf("%5d %8.2f %8d %8.0f\n", step, q, length(path), el))
  }
  list(found = FALSE, why = "out of steps", path = path, steps = MAX_STEPS)
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("scramble %d   depth %d   width %d   min gain %.1f\n\n",
            SCRAMBLE, DEPTH, WIDTH, MIN_GAIN))

for (i in seq_len(N_CUBES)) {
  scr <- generate_state(group = g, n_moves = SCRAMBLE)
  t0  <- proc.time()[["elapsed"]]
  res <- descend(scr)
  el  <- proc.time()[["elapsed"]] - t0

  ok <- isTRUE(res$found) &&
    identical(as.integer(replay(scr, res$path)), solved)
  cat(sprintf("\ncube %d: %s  verified %s  moves %d  steps %d  %.0fs\n",
              i, if (isTRUE(res$found)) "SOLVED" else paste("stuck --", res$why),
              ok, length(res$path), res$steps, el))
  if (isTRUE(res$found) && length(res$path) <= 80)
    cat("path:", paste(res$path, collapse = " "), "\n")
}
