#!/usr/bin/env Rscript
# Descend on the 4x4x4 by throwing random sequences until one helps.
#
# A random sequence applied over and over traces a cycle through the group.
# Nearly every such cycle wanders away from the solved cube and comes back
# without ever passing closer than it started -- measured on one scramble, three
# to seven out of forty did better than their starting point. But three out of
# forty is not nothing, and the model can tell which three: it scores a state in
# quarter turns from solved, so the best point of a cycle is read off, not
# searched for.
#
# So: throw sequences, keep the first that improves, move there, repeat. The
# model is the navigator -- it never proposes a move, it only says which of the
# proposals is worth taking. Every step is exact, because the path is the
# sequence actually applied; a wrong answer from the model costs a wasted throw,
# never a wrong path.
#
# What this is not: it is not a search for the shortest path. It walks downhill
# on a prediction and stops when it reaches the solved cube, and the walk it
# leaves behind is as long as the sequences it accepted.
#
# Run with:  Rscript inst/examples/demo_cube4_model_descent.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE   <- 12L      # quarter turns away from solved
N_CUBES    <- 1L

COMBO_LEN  <- 3L       # length of each random sequence; 3 did best when measured
REPS       <- 6L       # times a sequence is applied -- how far along its cycle
                       # we look. The good point, when there is one, comes early
MAX_TRIES  <- 200L      # sequences thrown per step before giving up
MAX_STEPS  <- 40L      # steps before giving up on the cube
MIN_GAIN   <- 1.5      # a step must improve the value by at least this

ARCHIVE    <- "/mnt/Data2/DS_projects/444/archive"
SEED       <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)
set.seed(SEED)

g  <- cube_group(4)
mv <- cube_moves(4)
names(mv) <- cube_move_names(4)
mnames <- names(mv)
solved <- seq_len(96)

score <- function(M) {
  if (!is.matrix(M)) M <- matrix(as.integer(M), nrow = 1L)
  storage.mode(M) <- "integer"
  cayley_distance("cube4_model")(M, solved, 4L)
}

# One throw: apply a random sequence REPS times, keeping every state passed
# through, and report the best point on it along with the moves that reach it.
throw <- function(state) {
  cmb <- sample(mnames, COMBO_LEN, replace = TRUE)
  s <- state
  states <- vector("list", COMBO_LEN * REPS)
  for (i in seq_len(COMBO_LEN * REPS)) {
    s <- s[mv[[cmb[((i - 1L) %% COMBO_LEN) + 1L]]]]
    states[[i]] <- s
  }
  M <- do.call(rbind, states)
  q <- score(M)
  j <- which.min(q)
  list(q = q[j], state = states[[j]],
       path = rep(cmb, length.out = COMBO_LEN * REPS)[seq_len(j)])
}

descend <- function(state) {
  cur <- state
  q   <- score(cur)
  path <- character(0)
  cat(sprintf("%5s %8s %8s %7s\n", "step", "q", "tries", "moves"))
  cat(sprintf("%5d %8.2f %8s %7d\n", 0L, q, "-", 0L))

  for (step in seq_len(MAX_STEPS)) {
    if (identical(as.integer(cur), solved))
      return(list(found = TRUE, path = path, steps = step - 1L))

    got <- NULL
    for (t in seq_len(MAX_TRIES)) {
      r <- throw(cur)
      if (r$q <= q - MIN_GAIN) { got <- r; break }
      # Landing exactly on the solved cube reads as q = 0 and is taken whatever
      # the gain rule says.
      if (identical(as.integer(r$state), solved)) { got <- r; break }
    }
    if (is.null(got))
      return(list(found = FALSE, why = "no improving sequence", path = path,
                  steps = step - 1L))

    cur  <- got$state
    q    <- got$q
    path <- c(path, got$path)
    cat(sprintf("%5d %8.2f %8d %7d\n", step, q, t, length(path)))

    if (identical(as.integer(cur), solved))
      return(list(found = TRUE, path = path, steps = step))
  }
  list(found = FALSE, why = "out of steps", path = path, steps = MAX_STEPS)
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("scramble %d   combo %d x %d reps   tries %d   min gain %.1f\n\n",
            SCRAMBLE, COMBO_LEN, REPS, MAX_TRIES, MIN_GAIN))

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
}
