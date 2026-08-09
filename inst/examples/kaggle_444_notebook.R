# ---------------------------------------------------------------------------
# cayley-py-444-cube -- solving a 4x4x4 with cayleyR
#
# Kaggle notebook, R. The competition gives a scrambled 4x4x4 as 96 colours and
# wants a path of its own moves. cayleyR speaks a different alphabet and a
# different sticker order, so the notebook is three steps: read their state
# into ours, solve, write the path back out.
#
# The cube is solved outright, by reduction: build the centres so each face
# acts as one, pair the edges so each acts as one, repair the two parity cases
# an even cube can reach and a 3x3x3 cannot, then finish with CFOP and lift
# that solution back to 4x4x4 moves.
# ---------------------------------------------------------------------------

# ---- 1. Load the data -----------------------------------------------------

DATA <- "/kaggle/input/competitions/cayley-py-444-cube"
if (!dir.exists(DATA)) DATA <- "."          # running outside Kaggle

puzzle <- jsonlite::fromJSON(file.path(DATA, "puzzle_info.json"))
test   <- read.csv(file.path(DATA, "test.csv"), stringsAsFactors = FALSE)

cat("puzzle      :", puzzle$name, "\n")
cat("generators  :", length(puzzle$generators),
    paste0("(", paste(head(names(puzzle$generators), 6), collapse = " "), " ...)"), "\n")
cat("state length:", length(puzzle$central_state), "\n")
cat("puzzles     :", nrow(test), "\n\n")
str(test[1:2, c("initial_state_id", "comment")])

# ---- 2. The two conversions ------------------------------------------------
#
# Neither can be guessed from the names, and both already live in the package.
#
#   cube_santa_state()  - Kaggle numbers the faces U F R B L D and we number
#                         them U R F D L B, so the sticker positions are
#                         relabelled. Their f3 is our B', not F'.
#   cube_colour_state() - their state is 96 COLOURS, ours is a permutation of
#                         96 positions. On a 4x4x4 a colouring does not name
#                         one state (pieces of a kind are interchangeable), so
#                         this returns a state showing those colours. That is
#                         all reduction needs -- it works on colours, and two
#                         pieces of a kind are interchangeable on the real cube
#                         too. Hence the warning, which is safe to suppress.

library(cayleyR)

read_state <- function(csv_row) {
  colours <- as.integer(strsplit(csv_row, ",", fixed = TRUE)[[1]])
  suppressWarnings(cube_colour_state(cube_santa_state(colours, 4), 4))
}

# ---- 3. Solve --------------------------------------------------------------
#
# Two things a long run needs and a short one can do without.
#
# A solve that throws must not take the run with it: one bad cube out of
# thousands would otherwise lose every solve made before it. Whatever the
# solver managed is kept, and the reason it stopped is kept beside it.
#
# The submission is written as we go rather than at the end, so a run cut short
# -- by the clock, by anything -- still leaves a valid file on disk covering
# the cubes it reached.

paths  <- character(nrow(test))
why    <- character(nrow(test))     # empty when solved; the failure otherwise
secs   <- numeric(nrow(test))

write_submission <- function() {
  write.csv(data.frame(initial_state_id = test$initial_state_id,
                       path = paths, stringsAsFactors = FALSE),
            "submission.csv", row.names = FALSE, quote = TRUE)
}

t_start <- proc.time()[["elapsed"]]

for (i in seq_len(nrow(test))) {
  t0    <- proc.time()[["elapsed"]]
  state <- read_state(test$initial_state[i])
  res   <- try(cube_solve4(state), silent = TRUE)

  if (inherits(res, "try-error")) {
    paths[i] <- ""
    why[i]   <- trimws(conditionMessage(attr(res, "condition")))
  } else {
    # The path is worth writing whether or not the solver finished: a cube left
    # part-solved is closer than one not touched, and the check below reports
    # what actually landed either way.
    paths[i] <- if (length(res$path)) cube_santa_path_out(res$path, 4) else ""
    why[i]   <- if (isTRUE(res$found)) "" else res$failure
  }
  secs[i] <- proc.time()[["elapsed"]] - t0

  if (i %% 100 == 0 || i == nrow(test)) {
    write_submission()
    el <- proc.time()[["elapsed"]] - t_start
    cat(sprintf("solved %d of %d  |  %.1f s elapsed, ~%.1f s left\n",
                i, nrow(test), el, el / i * (nrow(test) - i)))
  }
}

# ---- 4. Check the paths in Kaggle's own terms ------------------------------
#
# Not by asking the solver whether it succeeded -- by replaying each path with
# Kaggle's generators on Kaggle's state and looking at the result. A solver
# checked with its own arithmetic proves nothing.

apply_kaggle <- function(colours, path) {
  if (!nchar(path)) return(colours)
  for (m in strsplit(path, ".", fixed = TRUE)[[1]])
    colours <- colours[puzzle$generators[[m]] + 1L]
  colours
}

solved <- function(colours) {
  # every face one colour -- the whole cube, not just its centres
  all(vapply(0:5, function(f)
    length(unique(colours[f * 16L + 1:16])) == 1L, logical(1)))
}

ok <- vapply(seq_len(nrow(test)), function(i) {
  colours <- as.integer(strsplit(test$initial_state[i], ",", fixed = TRUE)[[1]])
  solved(apply_kaggle(colours, paths[i]))
}, logical(1))

lens <- vapply(strsplit(paths, ".", fixed = TRUE), length, integer(1))

cat("\ncubes solved  :", sum(ok), "of", nrow(test), "\n")

# Lengths over the cubes that came out solved. Averaging an unsolved cube's
# path in with the rest measures nothing -- a cube left alone scores a length
# of zero and would pull the median down.
if (any(ok))
  cat(sprintf("path length   : median %g, range %g-%g\n",
              median(lens[ok]), min(lens[ok]), max(lens[ok])))

cat(sprintf("time          : %.1f s total, %.2f s per cube\n",
            sum(secs), mean(secs)))

# The solver says why it stopped; that is worth more than the count of
# failures on its own, since the reasons point at different repairs.
if (any(!ok)) {
  cat("\nnot solved:\n")
  print(table(ifelse(nzchar(why[!ok]), why[!ok], "path replayed unsolved")))
}

# A path the solver called good but Kaggle's replay does not: that is a fault
# in the translation, not in the solve, and it is worth saying loudly.
disagree <- which(!ok & !nzchar(why))
if (length(disagree))
  cat("\nWARNING:", length(disagree),
      "path(s) the solver called solved did not replay solved --",
      "check the move translation\n")

# ---- 5. The submission -----------------------------------------------------
#
# Already on disk -- written every hundred cubes above, and once more at the
# end of the loop. This is the last write, and a look at what went out.

write_submission()
cat("\nwrote submission.csv --", nrow(test), "rows\n")
head(submission <- data.frame(initial_state_id = test$initial_state_id,
                              path = paths, stringsAsFactors = FALSE), 3)
