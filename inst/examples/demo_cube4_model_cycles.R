#!/usr/bin/env Rscript
# Descend on the 4x4x4 by cycling structured words and letting the model pick.
#
# The search is the one this package already describes: throw a word, follow its
# cycle, score the states along it, and move to the best point found. What took a
# while to learn is that the WORD MATTERS more than anything else about the
# search.
#
# Measured from one state twelve moves out, thirty words of each shape:
#
#   shape                       improve   by >1   best gain   min at first move
#   random, 8 moves               6/30       1       1.27          21/30
#   commutator  A B A' B'        15/30       5       1.53           9/30
#   conjugate   S A B A' B' S'   30/30       3       2.65           1/30
#
# A random word scrambles what it touches, so its cycle leaves and comes back
# without passing closer -- which is why its best point is usually the very first
# move, and why cycling it buys nothing over trying single moves. A conjugate
# moves a few pieces and puts everything else back, so its cycle threads through
# states that are genuinely nearer, and the good point lies INSIDE the cycle,
# where only following it will find it.
#
# What is logged, and why: the descent so far has stalled in a particular zone
# rather than everywhere, so every step reports the same profile as the table
# above -- how many words improved, how many by more than a move, how big the
# best gain was. If those collapse the run stops and says so, rather than
# grinding on; near the solved cube the remaining distance is short enough that
# an exact search is the better tool, and this one should hand over rather than
# pretend.
#
# Run with:  Rscript inst/examples/demo_cube4_model_cycles.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE   <- 12L      # quarter turns away from solved
N_CUBES    <- 1L

N_WORDS    <- 100L     # words tried per step; each costs REPS x |word| states,
                       # so 100 conjugates is about 5000 model calls a step
REPS       <- 6L       # times each word is applied round its cycle
SHAPE      <- "conjugate"  # "conjugate", "commutator" or "mixed"
MAX_STEPS  <- 60L
# Any real descent is worth taking. An earlier run stopped itself at a gain of
# 0.26 because the threshold was half a move -- but the state WAS closer, and
# the step cost four moves of path for it. Small steps are how the flat zones
# get crossed; the rule below is only here to catch a genuine dead end.
MIN_GAIN   <- 0.05     # a step must improve by this, or the search gives up
MIN_HITS   <- 1L       # ... and at least this many words must have improved

ARCHIVE    <- "/mnt/Data2/DS_projects/444/archive"
SEED       <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)
set.seed(SEED)

g  <- cube_group(4)
mv <- cube_moves(4)
names(mv) <- cube_move_names(4)
mn <- names(mv)
solved <- seq_len(96)

# Scored in chunks. One call per state wastes the batching; one call for ten
# thousand states asks the model to hold every embedding at once and the process
# gets killed. A few hundred at a time is the middle that survives both.
CHUNK <- 500L

score <- function(M) {
  if (!is.matrix(M)) M <- matrix(as.integer(M), nrow = 1L)
  storage.mode(M) <- "integer"
  n <- nrow(M)
  if (n <= CHUNK)
    return(cayley_distance("cube4_model")(M, solved, 4L))
  out <- numeric(n)
  for (from in seq(1L, n, by = CHUNK)) {
    to <- min(from + CHUNK - 1L, n)
    out[from:to] <- cayley_distance("cube4_model")(M[from:to, , drop = FALSE],
                                                   solved, 4L)
  }
  out
}

# A move name carries its own inverse: the prime is the whole difference.
inv_word <- function(w)
  rev(vapply(w, function(m)
    if (endsWith(m, "'")) substr(m, 1L, nchar(m) - 1L) else paste0(m, "'"),
    character(1), USE.NAMES = FALSE))

make_word <- function(shape) {
  A <- sample(mn, 2L, replace = TRUE)
  B <- sample(mn, 2L, replace = TRUE)
  comm <- c(A, B, inv_word(A), inv_word(B))
  if (shape == "commutator") return(comm)
  S <- sample(mn, sample(1:2, 1L), replace = TRUE)
  c(S, comm, inv_word(S))
}

# Follow every word round its cycle and score the whole lot in ONE call.
#
# Walking the cycles is arithmetic on integer vectors and costs almost nothing;
# the model costs milliseconds per state and does not care whether it is asked
# about fifty states or five thousand, so asking once per step instead of once
# per word is most of the run's time back. Returns the best point over all
# words, and the per-word minima, which are what the profile is read from.
follow_all <- function(state, words) {
  blocks <- lapply(words, function(w) {
    n <- length(w) * REPS
    s <- state
    m <- matrix(0L, n, 96L)
    for (i in seq_len(n)) {
      s <- s[mv[[w[((i - 1L) %% length(w)) + 1L]]]]
      m[i, ] <- s
    }
    m
  })
  sizes <- vapply(blocks, nrow, integer(1))
  M <- do.call(rbind, blocks)
  q <- score(M)

  ends   <- cumsum(sizes)
  starts <- ends - sizes + 1L
  per_word <- vapply(seq_along(words), function(i)
    min(q[starts[i]:ends[i]]), numeric(1))

  b <- which.min(per_word)
  j <- starts[b] + which.min(q[starts[b]:ends[b]]) - 1L
  list(per_word = per_word,
       q = q[j], state = M[j, ],
       path = rep(words[[b]], length.out = sizes[b])[seq_len(j - starts[b] + 1L)])
}

descend <- function(state) {
  cur  <- as.integer(state)
  q    <- score(cur)
  path <- character(0)

  cat(sprintf("%5s %8s %8s %7s %7s %8s %7s\n",
              "step", "q", "gain", "impr", "impr>1", "moves", "sec"))
  cat(sprintf("%5d %8.2f %8s %7s %7s %8d %7s\n", 0L, q, "-", "-", "-", 0L, "-"))

  for (step in seq_len(MAX_STEPS)) {
    t0 <- proc.time()[["elapsed"]]

    words <- lapply(seq_len(N_WORDS), function(i)
      make_word(if (SHAPE == "mixed")
                  sample(c("conjugate", "commutator"), 1L) else SHAPE))
    best  <- follow_all(cur, words)
    impr  <- sum(best$per_word < q)
    impr1 <- sum(best$per_word <= q - 1)
    el    <- proc.time()[["elapsed"]] - t0

    if (identical(as.integer(best$state), solved)) {
      path <- c(path, best$path)
      cat(sprintf("%5d %8.2f %8s %7d %7d %8d %7.0f  solved\n",
                  step, 0, "-", impr, impr1, length(path), el))
      return(list(found = TRUE, path = path, steps = step))
    }

    # The stopping rule is about the profile, not just this step's number: a
    # step that barely gains while almost nothing improved means the zone has
    # gone flat, and grinding here is worse than handing the rest to an exact
    # search over what is by then a short distance.
    if (best$q > q - MIN_GAIN || impr < MIN_HITS) {
      cat(sprintf("%5d %8.2f %8.2f %7d %7d %8d %7.0f  <- flat\n",
                  step, best$q, q - best$q, impr, impr1, length(path), el))
      return(list(found = FALSE,
                  why = sprintf("flat at q=%.2f (gain %.2f, %d/%d improved)",
                                q, q - best$q, impr, N_WORDS),
                  path = path, steps = step - 1L, q = q))
    }

    cur  <- as.integer(best$state)
    path <- c(path, best$path)
    cat(sprintf("%5d %8.2f %8.2f %7d %7d %8d %7.0f\n",
                step, best$q, q - best$q, impr, impr1, length(path), el))
    q <- best$q
  }
  list(found = FALSE, why = "out of steps", path = path, steps = MAX_STEPS, q = q)
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("scramble %d   words %d x %d reps   shape %s   min gain %.1f\n\n",
            SCRAMBLE, N_WORDS, REPS, SHAPE, MIN_GAIN))

for (i in seq_len(N_CUBES)) {
  scr <- generate_state(group = g, n_moves = SCRAMBLE)
  t0  <- proc.time()[["elapsed"]]
  res <- descend(scr)
  el  <- proc.time()[["elapsed"]] - t0

  ok <- isTRUE(res$found) &&
    identical(as.integer(replay(scr, res$path)), solved)
  cat(sprintf("\ncube %d: %s  verified %s  moves %d  steps %d  %.0fs\n",
              i, if (isTRUE(res$found)) "SOLVED" else paste("stopped --", res$why),
              ok, length(res$path), res$steps, el))
}
