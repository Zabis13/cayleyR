#!/usr/bin/env Rscript
# How short must a cycle be for the two sides to meet on a 4x4x4?
#
# find_path_iterative() finds a path where the forward and backward state sets
# intersect. Whether they ever do is a question about volume: two random sets of
# size n in a space of size N meet when n is around sqrt(N), and on this cube N
# is about 10^45. No practical n comes close -- IF the sets are random.
#
# The way out is to stop them being random. A short sequence expanded into its
# cycle stays near the state it started from; a long one wanders off across the
# whole group. With the scramble twelve moves out, the two sides need to meet
# somewhere around six moves from each -- so the cycles have to be small enough
# to still be inside that ball, and short sequences are what keeps them there.
#
# This sweeps COMBO_LEN from short to long and reports where intersections
# appear and where they stop. Everything else is held fixed.
#
# Run with:  Rscript inst/examples/demo_cube4_iterative_sweep.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

COMBO_LENS <- c(2L, 3L, 4L, 5L, 6L, 8L)   # the sweep
SCRAMBLE   <- 12L      # quarter turns away from solved
N_CUBES    <- 2L       # cubes per combo length

DISTANCE   <- "cube4_model"
MAX_ITER   <- 8L
N_SAMPLES  <- 30L
N_TOP      <- 8L
PTR        <- 5L
SORT_BY    <- c("shortest", "most_unique")
MAX_SCORED <- 40L
KEEP       <- TRUE
ONE_SIDED  <- FALSE

ARCHIVE    <- "/mnt/Data2/DS_projects/444/archive"
SEED       <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)

g      <- cube_group(4)
mv     <- cube_moves(4)
names(mv) <- cube_move_names(4)
solved <- seq_len(96)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("scramble %d   distance %s   %d cubes per length\n",
            SCRAMBLE, DISTANCE, N_CUBES))
cat(sprintf("iter %d   samples %d   top %d   ptr %d\n\n",
            MAX_ITER, N_SAMPLES, N_TOP, PTR))

cat(sprintf("%6s %6s %8s %8s %9s %8s  %s\n",
            "combo", "cube", "found", "moves", "cycles", "sec", "states s/f"))

rows <- list()
for (L in COMBO_LENS) {
  set.seed(SEED)          # same cubes at every length, so lengths compare
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
      combo_length   = L,
      ptr            = PTR,
      sort_by        = SORT_BY,
      max_scored     = MAX_SCORED,
      keep_states    = KEEP,
      verbose        = FALSE), silent = TRUE)
    el <- proc.time()[["elapsed"]] - t0

    if (inherits(r, "try-error")) {
      cat(sprintf("%6d %6d %8s %8s %9s %8.1f  %s\n", L, i, "ERR", "-", "-", el,
                  trimws(conditionMessage(attr(r, "condition")))))
      next
    }

    ok <- isTRUE(r$found) &&
      identical(as.integer(replay(scr, r$path)), solved)
    cat(sprintf("%6d %6d %8s %8s %9d %8.1f\n", L, i,
                if (ok) "yes" else if (isTRUE(r$found)) "BAD" else "no",
                if (is.null(r$path)) "-" else length(r$path),
                r$cycles, el))

    rows[[length(rows) + 1L]] <- data.frame(
      combo = L, cube = i, ok = ok,
      moves = if (ok) length(r$path) else NA_integer_,
      cycles = r$cycles, sec = el)
  }
}

# ---- what the sweep says ---------------------------------------------------

if (length(rows)) {
  d <- do.call(rbind, rows)
  cat("\n", strrep("=", 52), "\n", sep = "")
  for (L in sort(unique(d$combo))) {
    s <- d[d$combo == L, ]
    cat(sprintf("combo %2d: solved %d/%d", L, sum(s$ok), nrow(s)))
    if (any(s$ok))
      cat(sprintf("   moves %s   %.0fs median",
                  paste(s$moves[s$ok], collapse = "/"),
                  stats::median(s$sec)))
    cat("\n")
  }
}
