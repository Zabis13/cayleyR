# ---------------------------------------------------------------------------
# cayley-py-444-cube -- solving a 4x4x4 with cayleyR
#
# Kaggle notebook, R. The competition gives a scrambled 4x4x4 as 96 colours and
# wants a path of its own moves. cayleyR speaks a different alphabet and a
# different sticker order, so the notebook is: read their state into ours,
# solve, shorten, write the path back out.
#
# The cube is solved outright, by reduction: build the centres so each face
# acts as one, pair the edges so each acts as one, repair the two parity cases
# an even cube can reach and a 3x3x3 cannot, then finish with CFOP and lift
# that solution back to 4x4x4 moves.
# ---------------------------------------------------------------------------

library(cayleyR)

DEPTH <- 4L    # BFS depth for the shortener

# ---- 1. Load the data -----------------------------------------------------

DATA <- "/kaggle/input/competitions/cayley-py-444-cube"
if (!dir.exists(DATA)) DATA <- "."          # running outside Kaggle

test <- read.csv(file.path(DATA, "test.csv"), stringsAsFactors = FALSE)
cat("puzzles:", nrow(test), "\n\n")

# ---- 2. Read a state -------------------------------------------------------
#
# Two conversions, neither guessable from the names.
#
#   cube_santa_state()  - Kaggle numbers the faces U F R B L D and we number
#                         them U R F D L B, so the sticker positions are
#                         relabelled. Their f3 is our B', not F'.
#   cube_colour_state() - their state is 96 COLOURS, ours is a permutation of
#                         96 positions. On a 4x4x4 a colouring does not name
#                         one state (pieces of a kind are interchangeable), so
#                         this returns a state showing those colours. That is
#                         all reduction needs. Hence the warning, safely
#                         suppressed.

read_state <- function(csv_row) {
  colours <- as.integer(strsplit(csv_row, ",", fixed = TRUE)[[1]])
  suppressWarnings(cube_colour_state(cube_santa_state(colours, 4), 4))
}

# ---- 3. Solve and shorten --------------------------------------------------
#
# Reduction writes its stages one after another and none looks back at what the
# one before left, so the joins hold turns that undo each other. The shortener
# takes those out knowing nothing about cubes -- it works in the package's
# alphabet, so it runs before the path is translated, not after.

g     <- cube_group(4)
paths <- character(nrow(test))

write_submission <- function() {
  write.csv(data.frame(initial_state_id = test$initial_state_id,
                       path = paths, stringsAsFactors = FALSE),
            "submission.csv", row.names = FALSE, quote = TRUE)
}

t_start <- proc.time()[["elapsed"]]

for (i in seq_len(nrow(test))) {
  state <- read_state(test$initial_state[i])
  res   <- cube_solve4(state)
  path  <- res$path

  if (length(path))
    path <- short_path_bfs(path, state, depth = DEPTH, group = g)$path

  paths[i] <- if (length(path)) cube_santa_path_out(path, 4) else ""

  # Written as we go, so a run cut short still leaves a valid file on disk.
  if (i %% 100 == 0 || i == nrow(test)) {
    write_submission()
    el <- proc.time()[["elapsed"]] - t_start
    cat(sprintf("solved %d of %d  |  %.1f s elapsed, ~%.1f s left\n",
                i, nrow(test), el, el / i * (nrow(test) - i)))
  }
}

# ---- 4. Submit -------------------------------------------------------------

write_submission()

lens <- vapply(strsplit(paths, ".", fixed = TRUE), length, integer(1))
cat(sprintf("\npath length: median %g, range %g-%g\n",
            median(lens), min(lens), max(lens)))
cat("wrote submission.csv --", nrow(test), "rows\n")
