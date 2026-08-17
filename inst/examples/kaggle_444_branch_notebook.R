# ---------------------------------------------------------------------------
# cayley-py-444-cube -- reduction, random branching, and a depth-limited
# shortener. No model.
#
# Kaggle notebook, R. Three passes over each cube, in order:
#
#   reduction    cube_solve4(): centres, edge pairs, parity, then CFOP on what
#                is left. Finishes every cube, in about three hundred quarter
#                turns. Costs 0.14 s.
#   branching    play one random move at a random point of that path and solve
#                again from where it lands; keep the result if the whole path
#                came out shorter. Repeat BUDGET times.
#   shortening   sweep a depth-limited search along the result and replace any
#                stretch it can do in fewer moves.
#
# Why no model. A trained transformer was tried on exactly this and does work --
# it scores every move and a beam descends on it. But on this hardware one state
# costs 21 ms against 0.14 s for a whole solve, so the exact answer is about six
# model calls' worth of time; and the model's own gain correlated with the moves
# actually saved at -0.24, the largest gain it ever reported producing the worst
# branch of the run. Branching at random cut more than twice as much in a
# hundredth of the time. Where an exact answer is this cheap, measuring beats
# predicting.
#
# The two passes take out different things and neither subsumes the other.
# Branching only ever accepts a complete solve that came out shorter, so it
# cannot see a turn undone three moves later; the shortener sees exactly that
# and nothing else. On twenty cubes: reduction 306 moves, branching 288,
# shortener 246 -- and on cubes where branching accepted nothing at all the
# shortener still took out forty.
# ---------------------------------------------------------------------------

library(cayleyR)

DATA <- "/kaggle/input/competitions/cayley-py-444-cube"
if (!dir.exists(DATA)) DATA <- "."            # running outside Kaggle

BUDGET        <- 20L   # branches tried per cube; gains stop around fifty
SHORTEN_DEPTH <- 3L    # 24^d windows; depth 4 costs 24x this for less



test <- read.csv(file.path(DATA, "test.csv"), stringsAsFactors = FALSE)
cat("puzzles:", nrow(test), "\n")

g  <- cube_group(4)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
mn <- names(mv)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# ---- solve -----------------------------------------------------------------
#
# Each branch costs one solve, which is why the budget can be spent freely:
# twenty are under three seconds. Only one or two are usually accepted, and
# three hundred tries find nothing that fifty had not.
#
# The submission is written as we go, so a run cut short by the clock still
# leaves a valid file covering the cubes it reached.

paths  <- character(nrow(test))
status <- character(nrow(test))
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

  res <- try(cube_solve4(state), silent = TRUE)
  if (inherits(res, "try-error") || !isTRUE(res$found)) {
    paths[i]  <- ""
    status[i] <- "failed"
    secs[i]   <- proc.time()[["elapsed"]] - t0
    next
  }
  n_red[i] <- length(res$path)

  # Branch, then shorten. Each pass keeps whatever the one before produced.
  best <- res$path
  kept <- 0L
  for (b in seq_len(BUDGET)) {
    p <- sample.int(length(best), 1L) - 1L
    s <- state
    for (m in best[seq_len(p)]) s <- s[mv[[m]]]
    mvn <- sample(mn, 1L)
    r <- try(cube_solve4(as.integer(s[mv[[mvn]]])), silent = TRUE)
    if (inherits(r, "try-error") || !isTRUE(r$found)) next
    if (p + 1L + length(r$path) < length(best)) {
      best <- c(best[seq_len(p)], mvn, r$path)
      kept <- kept + 1L
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
    best <- res$path                      # fall back to the plain solve
    status[i] <- "reduction"
  } else {
    status[i] <- "shortened"
  }

  n_out[i] <- length(best)
  paths[i] <- cube_santa_path_out(best, 4)
  secs[i]  <- proc.time()[["elapsed"]] - t0

  if (i %% 25 == 0 || i == nrow(test)) {
    write_submission()
    el <- proc.time()[["elapsed"]] - t_start
    cat(sprintf("%d of %d  |  %.0f -> %.0f moves  |  %.0f s, ~%.0f s left\n",
                i, nrow(test), mean(n_red[seq_len(i)][n_red[seq_len(i)] > 0]),
                mean(n_out[seq_len(i)][n_out[seq_len(i)] > 0]),
                el, el / i * (nrow(test) - i)))
    flush.console()
  }
}

# ---- what came out ---------------------------------------------------------

ok <- status != "failed"

cat("\n", strrep("=", 58), "\n", sep = "")
cat("solved      :", sum(ok), "of", nrow(test), "\n\n")
if (any(ok)) {
  cat(sprintf("reduction   : median %5g moves\n", stats::median(n_red[ok])))
  cat(sprintf("after both  : median %5g moves  (%.1f%% off)\n",
              stats::median(n_out[ok]),
              100 * (1 - mean(n_out[ok]) / mean(n_red[ok]))))
  cat(sprintf("\nper cube    : %.1f s   total %.0f s\n",
              mean(secs[ok]), sum(secs)))
}
if (any(!ok)) cat("\nfailed      :", sum(!ok), "\n")

write_submission()
cat("\nwrote submission.csv --", nrow(test), "rows\n")
