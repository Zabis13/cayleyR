#!/usr/bin/env Rscript
# What the two cheap improvements are worth on a properly scrambled cube:
# choosing the starting face by the finished solution, then shortening.
#
# The reduction follows a schedule -- one centre, the layer beside it, the top
# slice emptied, the edges paired -- and runs it whatever the cube arrived like.
# Its price therefore turns on how the scramble happens to sit relative to the
# face the schedule starts on, and that face used to be written into the
# pipeline. cube_solve4(start_face = "full") solves from all six and keeps the
# shortest answer: measured over 40 scrambles of depth 2 to 20 it came to 62% of
# the fixed-face length, and it cannot come out worse, since the fixed face is
# one of the six it compares.
#
# Choosing on the finished solution rather than on the reduction matters. A
# short reduction can leave the cube in a parity a longer one avoids, and the
# fifty moves that costs outweigh what it saved -- over those same 40 scrambles
# the two criteria disagreed on 48% of cubes, and picking by the reduction came
# out LONGER than the fixed face on four of them.
#
# The shortener is the other half. It replays the path and looks for a shorter
# way between states it passes through, which takes out a turn undone a few
# moves later -- something a solver working stage by stage cannot see.
#
# Those measurements were taken near the solved state. This runs the pair where
# it actually matters: ten cubes 500 moves out, far enough that no cube is
# accidentally easy.
#
# Every path is replayed and checked BY COLOUR, not by sticker order: four
# indistinguishable centres to a face mean a finished cube need not have its
# sticker numbers back where they started.

library(cayleyR)

N             <- 4L
SCRAMBLE      <- 500L
N_CUBES       <- 10L
SHORTEN_DEPTH <- 3L
METHOD        <- "kociemba"

g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)
nm <- names(mv)
id <- group_identity(g)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

cat(sprintf("\n== %d cubes, %d moves from solved ---------------------------\n\n",
            N_CUBES, SCRAMBLE))
cat(sprintf("method     : red+%s, start_face = \"full\"\n", METHOD))
cat(sprintf("shortener  : short_path_bfs, depth %d\n\n", SHORTEN_DEPTH))

cat(sprintf("%5s  %8s  %8s  %8s  %9s  %s\n",
            "state", "moves", "short", "seconds", "status", "stages"))

rows <- list()
set.seed(500)
for (i in seq_len(N_CUBES)) {
  walk  <- sample(nm, SCRAMBLE, replace = TRUE)
  state <- replay(id, walk)

  # The row appears before the solve so that a long one is visibly running
  # rather than silently absent.
  cat(sprintf("%5d  %8s  %8s  %8s  %9s\r", i, "", "", "", "solving"))
  flush(stdout())

  t0  <- proc.time()[["elapsed"]]
  res <- tryCatch(cube_solve4(state, method = METHOD, start_face = "full"),
                  error = function(e) NULL)
  t_solve <- proc.time()[["elapsed"]] - t0

  if (is.null(res) || !isTRUE(res$found)) {
    cat(sprintf("%5d  %8s  %8s  %8.2f  %9s\n", i, "", "", t_solve,
                if (is.null(res)) "error" else "unsolved"))
    rows[[i]] <- data.frame(cube = i, raw = NA_integer_, short = NA_integer_,
                            t_solve = t_solve, t_short = NA_real_, ok = FALSE)
    next
  }

  raw <- res$path

  t0 <- proc.time()[["elapsed"]]
  sh <- tryCatch(short_path_bfs(raw, state, depth = SHORTEN_DEPTH, group = g),
                 error = function(e) NULL)
  t_short <- proc.time()[["elapsed"]] - t0

  # A shorter word is kept only if it still solves the cube. Checking here
  # rather than trusting the shortener is the point: a path that is short and
  # wrong would otherwise look like the best result on the row.
  short <- raw
  if (!is.null(sh) && !is.null(sh$path) && length(sh$path) < length(raw) &&
      isTRUE(cube_is_colour_solved(replay(state, sh$path))))
    short <- sh$path

  ok <- isTRUE(cube_is_colour_solved(replay(state, raw))) &&
        isTRUE(cube_is_colour_solved(replay(state, short)))

  # Seconds are the whole cost of getting the short word -- six solves and the
  # shortener -- not what the solver alone took.
  stages <- paste(sprintf("%s %d", res$stages$name, res$stages$n_moves),
                  collapse = ", ")

  cat(sprintf("%5d  %8d  %8d  %8.2f  %9s  %s\n",
              i, length(raw), length(short), t_solve + t_short,
              if (ok) "solved" else "WRONG", stages))

  rows[[i]] <- data.frame(cube = i, raw = length(raw), short = length(short),
                          t_solve = t_solve, t_short = t_short, ok = ok)
}

tb <- do.call(rbind, rows)
ok <- !is.na(tb$raw)

cat("\n== summary ---------------------------------------------------\n\n")
cat(sprintf("  solved            : %d of %d\n", sum(ok), nrow(tb)))
cat(sprintf("  every path solves : %s\n",
            if (all(tb$ok[ok])) "yes" else "NO -- see above"))

cat(sprintf("\n  solver out  : median %4.0f  mean %6.1f  range %d..%d\n",
            stats::median(tb$raw[ok]), mean(tb$raw[ok]),
            min(tb$raw[ok]), max(tb$raw[ok])))
cat(sprintf("  shortened   : median %4.0f  mean %6.1f  range %d..%d\n",
            stats::median(tb$short[ok]), mean(tb$short[ok]),
            min(tb$short[ok]), max(tb$short[ok])))

cut <- tb$raw[ok] - tb$short[ok]
cat(sprintf("  taken out   : median %3.0f moves, %.1f%% of the total (%d of %d)\n",
            stats::median(cut), 100 * sum(cut) / sum(tb$raw[ok]),
            sum(cut), sum(tb$raw[ok])))

cat(sprintf("\n  solve   : median %5.2f s  mean %5.2f  worst %5.2f\n",
            stats::median(tb$t_solve[ok]), mean(tb$t_solve[ok]),
            max(tb$t_solve[ok])))
cat(sprintf("  shorten : median %5.2f s  mean %5.2f  worst %5.2f\n",
            stats::median(tb$t_short[ok]), mean(tb$t_short[ok]),
            max(tb$t_short[ok])))
cat(sprintf("  total   : %.1f s over %d cubes, %.2f s each\n\n",
            sum(tb$t_solve[ok]) + sum(tb$t_short[ok]), sum(ok),
            (sum(tb$t_solve[ok]) + sum(tb$t_short[ok])) / sum(ok)))
