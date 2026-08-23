# ---------------------------------------------------------------------------
# cayley-py-444-cube -- Kociemba reduction with a fallback, then branching and
# a depth-limited shortener.
#
# What is different from kaggle_444_branch_notebook.R: the solve is
# cube_solve4_cascade() rather than cube_solve4(). The cascade tries to reduce
# the cube to a 3x3x3 by search -- phases 1, 2 and 3 across four orientations
# and four phase-2 solutions each -- and hands the cube to cube_solve4() only
# when no candidate reduces it inside the budget.
#
# What that buys, measured over thirty random cubes on prefilled tables:
#
#     reduce succeeded    9 of 30, 126 to 316 moves
#     fell back          21 of 30, 252 to 396 moves
#     median                       296 moves, 25.5 s per cube
#
# So it is not uniformly better: a cube that reduces cheaply comes out at half
# the length, and one that does not costs twenty-five seconds to learn that.
# Both passes of the old notebook are kept and run on whichever path comes back,
# because they take out different things -- branching cannot see a turn undone
# three moves later, and the shortener sees only that.
#
# The tables are the reason this is worth running at all. Phase 3 with its 256MB
# table loaded judges a node in a fraction of what an empty table costs, and a
# search that never grows its table never writes to it, which is what lets the
# forked workers share one copy. Point TABLE_DIR at them.
# ---------------------------------------------------------------------------

library(cayleyR)

DATA <- "/kaggle/input/competitions/cayley-py-444-cube"
if (!dir.exists(DATA)) DATA <- "."            # running outside Kaggle

# Where build_phase_tables.R left phase1_d*.bin, phase2_d*.bin, phase3_d*.bin.
# A phase whose file is missing is not fatal: it fills lazily inside the search,
# which is correct but slow, and the load report below says which happened.
TABLE_DIR <- "/kaggle/input/cayley-phase-tables"
if (!dir.exists(TABLE_DIR)) TABLE_DIR <- "/mnt/Data2/DS_projects/phase3"

NODE_BUDGET    <- 5e7        # phase 3 ceiling, divided by BUDGET_STEPS
PREP_BUDGET    <- 5e6        # phases 1-2, spent whole
BUDGET_STEPS   <- c(0.1, 0.3)
MAX_CANDIDATES <- 4L         # of 16; a cap on the wait, not on the quality
WORKERS        <- 6L         # candidates at a time; needs the tables loaded
SHUFFLE        <- TRUE       # which four of the sixteen this cube gets

BUDGET        <- 20L   # branches tried per cube; gains stop around fifty
SHORTEN_DEPTH <- 3L    # 24^d windows; depth 4 costs 24x this for less

# A cube that has already cost this long stops getting branches. The cascade's
# own time is not bounded by it -- MAX_CANDIDATES is what bounds that -- so this
# only stops the cheap passes from being spent on a cube that was expensive.
BRANCH_TIME_CAP <- 5

test <- read.csv(file.path(DATA, "test.csv"), stringsAsFactors = FALSE)
cat("puzzles:", nrow(test), "\n")

g  <- cube_group(4)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
mn <- names(mv)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# ---- the tables, once ------------------------------------------------------
#
# Loading pins min_size and max_size to the file's size, which stops a search
# from regrowing the table -- grow_to() reallocates and discards the contents,
# so a regrow throws away everything the file paid for.

cat("\ntables from", TABLE_DIR, "\n")
loaded <- rep(FALSE, 3)

for (ph in 1:3) {
  # The depth in the name is the depth reached, not the depth asked for, so the
  # deepest file wins by parsed depth rather than by sort order.
  cand <- Sys.glob(file.path(TABLE_DIR, sprintf("phase%d_d*.bin", ph)))
  if (length(cand) == 0) {
    cat(sprintf("  phase %d: no file -- fills lazily during the search\n", ph))
    next
  }
  d <- as.integer(sub("^.*_d([0-9]+)\\.bin$", "\\1", cand))
  cand <- cand[order(d, decreasing = TRUE)]

  ld <- try(cayleyR:::cube_kociemba4_load_phase_cpp(cand[1], ph), silent = TRUE)
  if (inherits(ld, "try-error") || !isTRUE(ld$ok)) {
    cat(sprintf("  phase %d: %s refused -- fills lazily instead\n",
                ph, basename(cand[1])))
    next
  }
  loaded[ph] <- TRUE
  cat(sprintf("  phase %d: %s, depth %d, %s entries, %.1f%% full\n",
              ph, basename(cand[1]), ld$built_depth,
              format(ld$n_writes, big.mark = ","),
              100 * ld$n_writes / ld$size))
}
cat(sprintf("  loaded %d of 3\n", sum(loaded)))

# Several workers are only safe on a loaded table: a search that has to grow its
# own writes to it, each fork's copy-on-write copy becomes real, and N workers
# then cost N tables rather than one.
workers <- if (all(loaded)) WORKERS else 1L
if (workers != WORKERS)
  cat(sprintf("  workers %d -> %d (tables incomplete)\n", WORKERS, workers))

# ---- solve -----------------------------------------------------------------
#
# The submission is written as we go, so a run cut short by the clock still
# leaves a valid file covering the cubes it reached.

paths  <- character(nrow(test))
status <- character(nrow(test))
method <- character(nrow(test))
secs   <- numeric(nrow(test))
n_red  <- integer(nrow(test))
n_out  <- integer(nrow(test))

write_submission <- function()
  write.csv(data.frame(initial_state_id = test$initial_state_id,
                       path = paths, stringsAsFactors = FALSE),
            "submission.csv", row.names = FALSE, quote = TRUE)

t_start <- proc.time()[["elapsed"]]

for (i in seq_len(nrow(test))) {
  t0 <- proc.time()[["elapsed"]]
  colours <- as.integer(strsplit(test$initial_state[i], ",", fixed = TRUE)[[1]])

  # Kaggle's colours into the package's sticker order; the path goes back the
  # other way at the end.
  state <- suppressWarnings(cube_colour_state(cube_santa_state(colours, 4), 4))

  res <- try(cube_solve4_cascade(state,
                                 node_budget = NODE_BUDGET,
                                 prep_budget = PREP_BUDGET,
                                 budget_steps = BUDGET_STEPS,
                                 max_candidates = MAX_CANDIDATES,
                                 shuffle_candidates = SHUFFLE,
                                 workers = workers),
             silent = TRUE)

  # The cascade always returns something when it returns at all -- cube_solve4()
  # is the floor -- so a try-error here is a real failure rather than a cube it
  # declined. Falling back to the plain solver keeps one bad cube from costing
  # the row entirely.
  if (inherits(res, "try-error")) {
    res <- try(cube_solve4(state), silent = TRUE)
    if (inherits(res, "try-error") || !isTRUE(res$found)) {
      paths[i]  <- ""
      status[i] <- "failed"
      method[i] <- "error"
      secs[i]   <- proc.time()[["elapsed"]] - t0
      next
    }
    method[i] <- "solve4-direct"
  } else {
    method[i] <- res$method
  }
  n_red[i] <- length(res$path)

  # Branch, then shorten -- on whatever the cascade produced, reduced or not.
  # Each branch costs one cube_solve4(), not one cascade: replacing the tail of
  # a path is a job for the fast solver, and paying the cascade twenty more
  # times per cube would cost minutes for a handful of moves.
  best <- res$path
  if (proc.time()[["elapsed"]] - t0 < BRANCH_TIME_CAP) {
    for (b in seq_len(BUDGET)) {
      p <- sample.int(length(best), 1L) - 1L
      s <- state
      for (m in best[seq_len(p)]) s <- s[mv[[m]]]
      mvn <- sample(mn, 1L)
      r <- try(cube_solve4(as.integer(s[mv[[mvn]]])), silent = TRUE)
      if (inherits(r, "try-error") || !isTRUE(r$found)) next
      if (p + 1L + length(r$path) < length(best))
        best <- c(best[seq_len(p)], mvn, r$path)
    }
  }

  sh <- try(short_path_bfs(best, state, depth = SHORTEN_DEPTH, group = g),
            silent = TRUE)
  if (!inherits(sh, "try-error") && !is.null(sh$path) &&
      length(sh$path) < length(best)) best <- sh$path

  # Verified by replaying, not by any solver's own flag, and by COLOUR: four
  # indistinguishable centres per face mean a finished cube need not have its
  # sticker numbers back in order.
  if (!cube_is_colour_solved(replay(state, best))) {
    best <- res$path                      # fall back to the cascade's own path
    status[i] <- if (cube_is_colour_solved(replay(state, best)))
                   "cascade" else "failed"
  } else {
    status[i] <- "shortened"
  }

  if (identical(status[i], "failed")) {
    paths[i] <- ""
    secs[i]  <- proc.time()[["elapsed"]] - t0
    next
  }

  n_out[i] <- length(best)
  paths[i] <- cube_santa_path_out(best, 4)
  secs[i]  <- proc.time()[["elapsed"]] - t0

  if (i %% 5 == 0 || i == nrow(test)) {
    write_submission()
    el <- proc.time()[["elapsed"]] - t_start
    done <- seq_len(i)
    cat(sprintf("%d of %d  |  %.0f -> %.0f moves  |  reduce %d  |  %.0f s, ~%.0f s left\n",
                i, nrow(test),
                mean(n_red[done][n_red[done] > 0]),
                mean(n_out[done][n_out[done] > 0]),
                sum(method[done] == "reduce"),
                el, el / i * (nrow(test) - i)))
    flush.console()
  }
}

# ---- what came out ---------------------------------------------------------

ok <- status != "failed" & status != ""

cat("\n", strrep("=", 58), "\n", sep = "")
cat("solved      :", sum(ok), "of", nrow(test), "\n\n")

# Which road each cube took. A high solved count on a low reduce count is the
# cascade working around phase 3 rather than phase 3 working -- the fallback is
# what finished those cubes, at roughly twice the length.
for (m in names(sort(table(method[ok]), decreasing = TRUE)))
  cat(sprintf("method %-14s: %d\n", m, sum(method[ok] == m)))

if (any(ok)) {
  cat(sprintf("\nsolver out  : median %5g moves\n", stats::median(n_red[ok])))
  cat(sprintf("after both  : median %5g moves  (%.1f%% off)\n",
              stats::median(n_out[ok]),
              100 * (1 - mean(n_out[ok]) / mean(n_red[ok]))))

  # Separately, because the two populations differ by a factor of two and a
  # single median hides which one a given batch is mostly made of.
  for (m in c("reduce", "solve4")) {
    sel <- ok & method == m
    if (any(sel))
      cat(sprintf("  %-10s: median %5g moves, %.1f s each  (%d cubes)\n",
                  m, stats::median(n_out[sel]), mean(secs[sel]), sum(sel)))
  }

  cat(sprintf("\nper cube    : %.1f s   total %.0f s\n",
              mean(secs[ok]), sum(secs)))
}
if (any(!ok)) cat("\nfailed      :", sum(!ok), "\n")

write_submission()
cat("\nwrote submission.csv --", nrow(test), "rows\n")
