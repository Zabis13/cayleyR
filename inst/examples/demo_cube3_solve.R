#!/usr/bin/env Rscript
# Four human methods on the 3x3x3, measured.
#
# Ten random states, each solved four ways, with the solving word, its length,
# and the length after a general shortener has taken out the detours. The first
# three each give up something the one before relied on, and pay for it in
# moves; the fourth buys some of it back:
#
#   CFOP         pairs a corner with its edge and inserts both at once
#   LBL          gives up the pairing: bottom layer outright, then the middle
#                edges one at a time -- roughly twice the moves
#   OldPochmann  gives up looking at the cube: one algorithm per piece, with
#                setup moves around it -- roughly four times
#   M2           the same blindfold idea with a two-move swap for the edges
#                instead of a whole PLL -- cheaper than Old Pochmann
#
# Move counts are quarter turns, the metric the whole package uses. Speedcubers
# count a half turn as one move, so their figures for the same solve are
# smaller.
#
# A stage of the first two methods is an exact search to a fixed depth, so a
# hard state can take a long time or fail outright. Nothing here interrupts it.
# The two blindfold methods search for nothing and their length barely varies.
#
# Run with:  Rscript inst/examples/demo_cube3_solve.R

library(cayleyR)

N        <- 3L        #cube_group
n_states <- 10L       # samples
n_moves  <- 985L      # quarter turns walked away from the solved cube
shorten_depth <- 2L   # BFS depth for the shortener; see solve_and_report()

set.seed(2026)

g <- cube_group(N)

hr <- function(title) cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")

solve_and_report <- function(method_name, method, state) {
  cat("\n  ", method_name, "\n", sep = "")

  # Timed from here to the end of the shortener: the reported figure is what it
  # costs to get the short word, not what the solver alone took.
  t0 <- proc.time()[["elapsed"]]
  r <- try(method(state), silent = TRUE)

  if (inherits(r, "try-error")) {
    secs <- proc.time()[["elapsed"]] - t0
    msg <- trimws(sub("^.*cube_solve: ", "",
                      conditionMessage(attr(r, "condition"))))
    cat(sprintf("    FAILED after %.2fs: %s\n", secs, msg))
    return(list(status = "error", moves = NA_integer_, short = NA_integer_,
                seconds = secs))
  }

  cat("    ", paste(r$path, collapse = " "), "\n", sep = "")

  # The same solve with the detours taken out. Neither method looks back at
  # what it has already written -- a stage ends where the next one starts, and
  # the join often contains a turn and its inverse -- so a general shortener
  # finds moves to drop without knowing anything about cubes.
  #
  # Depth 2 rather than the default 5: the cube's alphabet is 18 moves against
  # TopSpin's 3, so the neighbourhood grows as 18^d. Measured on one solve,
  # depth 2 took 0.06s and saved 14 moves, depth 3 took 0.78s for 16, and depth
  # 4 took 14s for the same 16.
  sh <- short_path_bfs(r$path, state, depth = shorten_depth, group = g)
  secs <- proc.time()[["elapsed"]] - t0
  cat("    short: ", paste(sh$path, collapse = " "), "\n", sep = "")

  cat(sprintf("    %s in %d moves, %d short, %.2fs\n",
              if (isTRUE(r$found)) "SOLVED" else "NOT SOLVED",
              length(r$path), sh$new_length, secs))

  list(status = if (isTRUE(r$found)) "solved" else "unsolved",
       moves = length(r$path), short = sh$new_length, seconds = secs)
}

rows <- list()

for (i in seq_len(n_states)) {
  # A random state of the cube group. The alphabet has all 18 moves, slices
  # included, so a walk may leave the centres turned; both solvers put them
  # back before they start, which costs a rotation and moves no piece relative
  # to another.
  final_state <- generate_state(group = g, n_moves = n_moves)

  hr(sprintf("state %d of %d", i, n_states))
  cat("final_state:", paste(final_state, collapse = " "), "\n")
  cat("centres home:",
      all(final_state[cube_centre_positions()] == cube_centre_positions()), "\n")

  for (nm in c("CFOP", "LBL", "OldPochmann", "M2")) {
    method <- switch(nm,
                     CFOP = cube_solve_cfop,
                     LBL = cube_solve_lbl,
                     OldPochmann = cube_solve_old_pochmann,
                     M2 = cube_solve_m2)
    r <- solve_and_report(nm, method, final_state)
    rows[[length(rows) + 1L]] <- data.frame(
      state = i, method = nm, status = r$status,
      moves = r$moves, short = r$short, seconds = round(r$seconds, 2),
      stringsAsFactors = FALSE)
  }
}

res <- do.call(rbind, rows)

# ---------------------------------------------------------------- summary
#
# Averaged over the states that finished. A method that solved none has no mean
# to report, and saying so is more use than a NaN.

hr("summary")
cat(sprintf("%d states, %d moves from solved\n\n", n_states, n_moves))

summary_row <- function(nm) {
  d <- res[res$method == nm, ]
  ok <- d$status == "solved"
  data.frame(
    method = nm,
    solved = sprintf("%d/%d", sum(ok), nrow(d)),
    mean_moves = if (any(ok)) round(mean(d$moves[ok]), 1) else NA_real_,
    mean_short = if (any(ok)) round(mean(d$short[ok]), 1) else NA_real_,
    mean_sec = if (any(ok)) round(mean(d$seconds[ok]), 2) else NA_real_,
    errors = sum(d$status == "error"),
    stringsAsFactors = FALSE)
}

print(rbind(summary_row("CFOP"), summary_row("LBL"),
            summary_row("OldPochmann"), summary_row("M2")), row.names = FALSE)

# What the comparison is for: each method costs more than the one before, and
# the reason is the same each time. LBL gives up CFOP's pairing and does the
# bottom layer outright; Old Pochmann gives up looking at the cube at all and
# places one piece per algorithm. Measured against CFOP on the states every
# method finished.
both <- Reduce(intersect,
               lapply(c("CFOP", "LBL", "OldPochmann", "M2"),
                      function(m) res$state[res$method == m &
                                            res$status == "solved"]))
if (length(both) > 0) {
  mean_for <- function(m) mean(res$moves[res$method == m & res$state %in% both])
  base <- mean_for("CFOP")
  cat(sprintf("\non the %d state(s) every method solved:\n", length(both)))
  for (m in c("CFOP", "LBL", "OldPochmann", "M2")) {
    cat(sprintf("  %-12s %6.1f moves   %.2fx CFOP\n",
                m, mean_for(m), mean_for(m) / base))
  }
} else {
  cat("\nno state was solved by every method\n")
}

fails <- res[res$status != "solved", ]
if (nrow(fails) > 0) {
  cat("\ndid not solve:\n")
  print(fails[, c("state", "method", "status", "seconds")], row.names = FALSE)
}
