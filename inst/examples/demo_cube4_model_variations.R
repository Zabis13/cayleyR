#!/usr/bin/env Rscript
# Descend on the 4x4x4 by varying sequences that worked, not by drawing new ones.
#
# Drawing sequences at random and keeping the ones that help does work, but it
# does not scale: measured on a scramble twelve moves out, the first real step
# took 144 throws and the second did not come in 200. Each move closer shrinks
# the target by roughly an order of magnitude, so uniform sampling runs into a
# wall almost immediately.
#
# The way round it is to stop drawing uniformly. A sequence that helped is
# evidence about where other helpful sequences are: its neighbours -- the same
# moves with one replaced, or one appended, or one dropped -- are far likelier
# to help than a fresh draw. So the search keeps a working sequence and mutates
# it, falling back to a fresh draw only when the neighbourhood is exhausted.
#
# The model's job is unchanged: it never proposes anything, it only says which
# of the proposals lands closer. Every accepted step is exact, being the moves
# actually applied.
#
# Run with:  Rscript inst/examples/demo_cube4_model_variations.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE   <- 12L      # quarter turns away from solved
N_CUBES    <- 1L

COMBO_LEN  <- 3L       # length of a fresh random sequence
REPS       <- 6L       # how far along its cycle each sequence is followed
MAX_LEN    <- 8L       # a mutated sequence may grow to this
N_VARIANTS <- 24L      # neighbours tried per round before drawing fresh
MAX_TRIES  <- 300L     # sequences tried per step, mutations and draws together
MAX_STEPS  <- 60L
MIN_GAIN   <- 1.0      # a step must improve the value by at least this

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

# Follow a sequence REPS times round its cycle and report the best point on it.
follow <- function(state, cmb) {
  n <- length(cmb) * REPS
  s <- state
  states <- vector("list", n)
  for (i in seq_len(n)) {
    s <- s[mv[[cmb[((i - 1L) %% length(cmb)) + 1L]]]]
    states[[i]] <- s
  }
  q <- score(do.call(rbind, states))
  j <- which.min(q)
  list(q = q[j], state = states[[j]],
       path = rep(cmb, length.out = n)[seq_len(j)])
}

# One neighbour of a sequence: swap a move, append one, or drop one.
mutate <- function(cmb) {
  what <- sample(if (length(cmb) <= 1L) c("swap", "grow")
                 else if (length(cmb) >= MAX_LEN) c("swap", "shrink")
                 else c("swap", "swap", "grow", "shrink"), 1L)
  switch(what,
    swap   = { i <- sample.int(length(cmb), 1L)
               cmb[i] <- sample(mnames, 1L); cmb },
    grow   = c(cmb, sample(mnames, 1L)),
    shrink = cmb[-sample.int(length(cmb), 1L)])
}

descend <- function(state) {
  cur  <- state
  q    <- score(cur)
  path <- character(0)
  seed_cmb <- NULL          # the sequence that last worked, if any

  cat(sprintf("%5s %8s %7s %7s %7s\n", "step", "q", "tries", "moves", "from"))
  cat(sprintf("%5d %8.2f %7s %7d %7s\n", 0L, q, "-", 0L, "-"))

  for (step in seq_len(MAX_STEPS)) {
    if (identical(as.integer(cur), solved))
      return(list(found = TRUE, path = path, steps = step - 1L))

    got <- NULL; how <- "draw"; tries <- 0L
    # A round is N_VARIANTS neighbours of the working sequence, then a fresh
    # draw that becomes the working sequence if the neighbourhood gave nothing.
    while (tries < MAX_TRIES && is.null(got)) {
      if (!is.null(seed_cmb)) {
        for (v in seq_len(N_VARIANTS)) {
          tries <- tries + 1L
          cand <- mutate(seed_cmb)
          r <- follow(cur, cand)
          if (r$q <= q - MIN_GAIN || identical(as.integer(r$state), solved)) {
            got <- r; how <- "mutate"; seed_cmb <- cand; break
          }
          if (tries >= MAX_TRIES) break
        }
      }
      if (!is.null(got)) break
      tries <- tries + 1L
      cand <- sample(mnames, COMBO_LEN, replace = TRUE)
      r <- follow(cur, cand)
      if (r$q <= q - MIN_GAIN || identical(as.integer(r$state), solved)) {
        got <- r; how <- "draw"; seed_cmb <- cand
      } else if (is.null(seed_cmb)) seed_cmb <- cand
    }

    if (is.null(got))
      return(list(found = FALSE, why = "no improving sequence", path = path,
                  steps = step - 1L))

    cur  <- got$state
    q    <- got$q
    path <- c(path, got$path)
    cat(sprintf("%5d %8.2f %7d %7d %7s\n", step, q, tries, length(path), how))

    if (identical(as.integer(cur), solved))
      return(list(found = TRUE, path = path, steps = step))
  }
  list(found = FALSE, why = "out of steps", path = path, steps = MAX_STEPS)
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("scramble %d   variants %d   tries %d   min gain %.1f   max len %d\n\n",
            SCRAMBLE, N_VARIANTS, MAX_TRIES, MIN_GAIN, MAX_LEN))

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
