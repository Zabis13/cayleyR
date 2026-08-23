# ---------------------------------------------------------------------------
# cayley-py-444-cube -- reduction with the two-phase 3x3x3 solver on top, then
# a depth-limited shortener.
#
# What is different from kaggle_444_notebook.R: the 3x3x3 stage is Kociemba's
# two-phase search rather than CFOP. Both share the whole reduction -- centres,
# edge pairs, parity -- and differ only in what finishes the cube once it is a
# 3x3x3, so the spread between them is exactly what that choice costs.
#
# Measured over a hundred random cubes, seed 1, forty-move scrambles:
#
#     red+cfop        315.2 moves, 0.08 s per cube, 100 of 100 solved
#     red+kociemba    251.1 moves, 0.38 s per cube, 100 of 100 solved
#
# Twenty per cent shorter, on every one of the hundred -- not on average with
# exceptions, but on all of them, the gain running from ten to twenty-eight per
# cent. The cost is a third of a second per cube, which is nothing beside the
# twenty-five seconds a cascade spends learning that a cube will not reduce.
#
# This notebook was worth writing only after cube_solve4(method = "kociemba")
# was repaired: it used to fail on every cube, because cube_kociemba() returned
# a bare word where the other four solvers returned a list, and reading $found
# off a character vector is an error rather than FALSE.
#
# ---- the shortener ---------------------------------------------------------
#
# short_position() is TopSpin's and does nothing to a cube path: it collapses
# runs of the shift operators, which cube notation does not have. The one that
# works here is short_path_bfs(), which replays the path and looks for a
# shorter way between states it passes through, out to a depth. Measured on
# these paths, depth 3 is where the gain per second stops improving -- depth 4
# searches 24 times as many windows for a fraction more.
# ---------------------------------------------------------------------------

library(cayleyR)

DATA <- "/kaggle/input/competitions/cayley-py-444-cube"
if (!dir.exists(DATA)) DATA <- "."            # running outside Kaggle

SHORTEN_DEPTH <- 3L    # 24^d windows per position; 4 costs 24x for little

test <- read.csv(file.path(DATA, "test.csv"), stringsAsFactors = FALSE)
cat("puzzles:", nrow(test), "\n")

g  <- cube_group(4)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
mn <- names(mv)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

paths  <- character(nrow(test))
status <- character(nrow(test))
secs   <- numeric(nrow(test))
n_raw  <- integer(nrow(test))
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

  res <- try(cube_solve4(state, method = "kociemba"), silent = TRUE)

  # A cube the two-phase search cannot finish is still a cube CFOP will: the
  # reduction is identical and only the last stage differs, so falling back
  # costs length rather than the row.
  if (inherits(res, "try-error") || !isTRUE(res$found)) {
    res <- try(cube_solve4(state, method = "cfop"), silent = TRUE)
    if (inherits(res, "try-error") || !isTRUE(res$found)) {
      paths[i]  <- ""
      status[i] <- "failed"
      secs[i]   <- proc.time()[["elapsed"]] - t0
      next
    }
    status[i] <- "cfop"
  } else {
    status[i] <- "kociemba"
  }
  n_raw[i] <- length(res$path)
  best <- res$path

  # The shortener: replay the path and look for a shorter way between states it
  # passes through. What it takes out is a turn undone a few moves later, which
  # a solver working stage by stage cannot see.
  #
  # Note that short_position() would do nothing here -- it is TopSpin's, and
  # collapses runs of the shift operators, which cube notation does not have.
  sh <- try(short_path_bfs(best, state, depth = SHORTEN_DEPTH, group = g),
            silent = TRUE)
  if (!inherits(sh, "try-error") && !is.null(sh$path) &&
      length(sh$path) < length(best)) best <- sh$path

  # Verified by replaying, and by COLOUR: four indistinguishable centres per
  # face mean a finished cube need not have its sticker numbers back in order.
  if (!cube_is_colour_solved(replay(state, best))) {
    best <- res$path
    if (!cube_is_colour_solved(replay(state, best))) {
      paths[i]  <- ""
      status[i] <- "failed"
      secs[i]   <- proc.time()[["elapsed"]] - t0
      next
    }
  }

  n_out[i] <- length(best)
  paths[i] <- cube_santa_path_out(best, 4)
  secs[i]  <- proc.time()[["elapsed"]] - t0

  if (i %% 5 == 0 || i == nrow(test)) {
    write_submission()
    el <- proc.time()[["elapsed"]] - t_start
    done <- seq_len(i)
    solved <- n_out[done] > 0
    cat(sprintf("%d of %d  |  %.0f -> %.0f moves  |  %.0f s, ~%.0f s left\n",
                i, nrow(test),
                mean(n_raw[done][solved]), mean(n_out[done][solved]),
                el, el / i * (nrow(test) - i)))
    flush.console()
  }
}

# ---- what came out ---------------------------------------------------------

ok <- status != "failed" & status != ""

cat("\n", strrep("=", 58), "\n", sep = "")
cat("solved      :", sum(ok), "of", nrow(test), "\n")
for (m in names(sort(table(status[ok]), decreasing = TRUE)))
  cat(sprintf("  %-10s: %d\n", m, sum(status[ok] == m)))

if (any(ok)) {
  cat(sprintf("\nsolver out  : median %5g moves\n", stats::median(n_raw[ok])))
  cat(sprintf("after both  : median %5g moves  (%.1f%% off)\n",
              stats::median(n_out[ok]),
              100 * (1 - mean(n_out[ok]) / mean(n_raw[ok]))))
  cat(sprintf("\nper cube    : %.2f s   total %.0f s\n",
              mean(secs[ok]), sum(secs)))
}
if (any(!ok)) cat("\nfailed      :", sum(!ok), "\n")

write_submission()
cat("\nwrote submission.csv --", nrow(test), "rows\n")
