#!/usr/bin/env Rscript
# Iterative search on the 4x4x4, with the model choosing the bridges.
#
# find_path_iterative() does not descend a heuristic. It throws random operation
# sequences, expands their cycles, and looks for a state the two sides have in
# common; the path is then read off that meeting point. Nothing about it trusts
# a distance -- the distance only decides WHICH of the states found is worth
# carrying forward as the next bridge.
#
# That is the one place a model belongs. On a cube the array distances say very
# little: two states one quarter turn apart can differ in most of their
# stickers, so "manhattan" ranks bridges close to blindly. The model was trained
# to answer exactly the question being asked -- how many quarter turns from
# solved -- and a wrong answer costs a slower search, not a wrong path, because
# the meeting points themselves are exact.
#
# The search runs from the scramble towards the solved cube, which is the
# direction the model can score: it knows distance to the solved cube and to
# nothing else.
#
# Watch out for memory. Every state of every expanded cycle is stored, and a
# random sequence on a cube can have order in the tens of thousands: expanded
# five moves at a time that is gigabytes from one sequence. SORT_BY is the
# control that matters -- ask for short cycles and the question does not arise.
# N_TOP is next, being how many cycles are expanded at all; N_SAMPLES only says
# how many are measured to choose from, and costs little.
#
# Run with:  Rscript inst/examples/demo_cube4_iterative_model.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------
# Edit these; nothing is read from the command line.

SCRAMBLE   <- 12L      # quarter turns away from solved
N_CUBES    <- 1L       # how many to try

DISTANCE   <- "cube4_model"   # "cube4_model" or "manhattan", to compare
ONE_SIDED  <- FALSE     # expand the target side once, then advance from the start
MAX_ITER   <- 10L       # search cycles
N_SAMPLES  <- 20L      # random sequences per cycle
N_TOP      <- 5L       # of those, how many are expanded in full
COMBO_LEN  <- 5L       # length of each random sequence -- drives cycle size
PTR        <- 5L       # intersections examined per cycle

# Which of the sampled sequences get expanded. The default the function carries,
# c("longest", "most_unique"), picks the biggest cycles it can find -- on a cube
# that is what fills memory, since a sequence of order 27720 expanded ten moves
# at a time is a hundred gigabytes of stored states. "shortest" asks for the
# opposite and keeps every cycle it takes whole, which is what the intersection
# search wants: a truncated cycle can hide the meeting point.
SORT_BY    <- c("shortest", "most_unique")
# The model costs about 20 ms a state, so what matters is how many states it is
# asked about. A cycle holds hundreds; scoring them all puts the whole run in
# the metric. The candidates of one cycle sit along one sequence anyway, so a
# sample ranks about as well as the lot.
MAX_SCORED <- 40L      # candidates scored per bridge choice; NULL for all

# one_sided freezes the target side after cycle 1, which only works if that
# side is not then cleared -- so it needs KEEP.
KEEP       <- TRUE     # keep every cycle's states (memory grows)
REUSE      <- FALSE    # generate the random sequences once and reuse them
VERBOSE    <- TRUE

ARCHIVE    <- "/mnt/Data2/DS_projects/444/archive"   # holds model/model.pth
SEED       <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)
set.seed(SEED)

g      <- cube_group(4)
mv     <- cube_moves(4)
names(mv) <- cube_move_names(4)
solved <- seq_len(96)

# Replayed independently of the search: a path is checked by walking it, not by
# the flag the search returns.
replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("scramble %d   distance %s   one_sided %s\n",
            SCRAMBLE, DISTANCE, ONE_SIDED))
cat(sprintf("iter %d   samples %d   top %d   combo %d   ptr %d\n\n",
            MAX_ITER, N_SAMPLES, N_TOP, COMBO_LEN, PTR))

for (i in seq_len(N_CUBES)) {
  scr <- generate_state(group = g, n_moves = SCRAMBLE)

  t0 <- proc.time()[["elapsed"]]
  r  <- try(find_path_iterative(
    start_state    = scr,
    final_state    = solved,
    group          = g,
    distance_method = DISTANCE,
    one_sided      = ONE_SIDED,
    max_iterations = MAX_ITER,
    n_samples      = N_SAMPLES,
    n_top          = N_TOP,
    combo_length   = COMBO_LEN,
    ptr            = PTR,
    sort_by        = SORT_BY,
    max_scored     = MAX_SCORED,
    keep_states    = KEEP,
    reuse_combos   = REUSE,
    verbose        = VERBOSE), silent = TRUE)
  el <- proc.time()[["elapsed"]] - t0

  if (inherits(r, "try-error")) {
    cat(sprintf("cube %d: error after %.1fs -- %s\n", i, el,
                trimws(conditionMessage(attr(r, "condition")))))
    next
  }

  ok <- isTRUE(r$found) && identical(as.integer(replay(scr, r$path)), solved)
  cat(sprintf("\ncube %d: found %s  verified %s  moves %s  cycles %s  %.1fs\n",
              i, isTRUE(r$found), ok,
              if (is.null(r$path)) "-" else length(r$path),
              r$cycles, el))
  if (isTRUE(r$found) && length(r$path) <= 60)
    cat("path:", paste(r$path, collapse = " "), "\n")
}
