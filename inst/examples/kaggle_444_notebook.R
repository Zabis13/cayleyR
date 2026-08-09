# ---------------------------------------------------------------------------
# cayley-py-444-cube -- solving the centres of a 4x4x4 with cayleyR
#
# Kaggle notebook, R. The competition gives a scrambled 4x4x4 as 96 colours and
# wants a path of its own moves. cayleyR speaks a different alphabet and a
# different sticker order, so the notebook is three steps: read their state
# into ours, solve, write the path back out.
#
# What is solved here is the CENTRES -- the first stage of reduction. On a
# 3x3x3 a centre is one sticker and cannot move; from 4x4x4 up each face
# carries four of them and they have to be gathered first.
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
#                         this returns a state showing those colours, which is
#                         all the centres stage needs. Hence the warning.

library(cayleyR)

read_state <- function(csv_row) {
  colours <- as.integer(strsplit(csv_row, ",", fixed = TRUE)[[1]])
  suppressWarnings(cube_colour_state(cube_santa_state(colours, 4), 4))
}

# ---- 3. Solve --------------------------------------------------------------

paths <- character(nrow(test))

for (i in seq_len(nrow(test))) {
  state <- read_state(test$initial_state[i])
  res   <- cube_solve_centres(state)
  paths[i] <- if (isTRUE(res$found)) cube_santa_path_out(res$path, 4) else ""
  if (i %% 100 == 0) cat("solved", i, "of", nrow(test), "\n")
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

centres_built <- function(colours) {
  # the inner 2x2 of each face: local indices 5,6,9,10 of a 16-sticker block
  all(vapply(0:5, function(f)
    length(unique(colours[f * 16L + c(6L, 7L, 10L, 11L)])) == 1L, logical(1)))
}

ok <- vapply(seq_len(nrow(test)), function(i) {
  colours <- as.integer(strsplit(test$initial_state[i], ",", fixed = TRUE)[[1]])
  centres_built(apply_kaggle(colours, paths[i]))
}, logical(1))

lens <- vapply(strsplit(paths, ".", fixed = TRUE), length, integer(1))

cat("\ncentres built :", sum(ok), "of", nrow(test), "\n")
cat("path length   : median", median(lens), " range", min(lens), "-", max(lens), "\n")

# ---- 5. Write the submission ----------------------------------------------

submission <- data.frame(initial_state_id = test$initial_state_id,
                         path = paths, stringsAsFactors = FALSE)
write.csv(submission, "submission.csv", row.names = FALSE, quote = TRUE)
cat("\nwrote submission.csv\n")
head(submission, 3)
