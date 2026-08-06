#!/usr/bin/env Rscript
# Two human methods on the 3x3x3, measured.
#
# Ten random states, each solved by CFOP and by layer by layer, with the solving
# word, its length, and the length after a general shortener has taken out the
# detours. The two methods differ in the middle -- CFOP pairs a corner with its
# edge and inserts both at once, LBL finishes the bottom layer and then puts the
# middle edges in one at a time -- and that difference is supposed to show up as
# roughly half the moves.
#
# Move counts are quarter turns, the metric the whole package uses. Speedcubers
# count a half turn as one move, so their figures for the same solve are
# smaller: expect around 55 quarter turns for CFOP and 110 for LBL where the
# literature says 25 and 55.
#
# A stage of either method is an exact search to a fixed depth, so a hard state
# can take a long time or fail outright. Nothing here interrupts it.
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

  for (nm in c("CFOP", "LBL")) {
    method <- if (nm == "CFOP") cube_solve_cfop else cube_solve_lbl
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

print(rbind(summary_row("CFOP"), summary_row("LBL")), row.names = FALSE)

# The comparison the two methods are for: how much longer LBL is, on the states
# where both finished.
both <- intersect(res$state[res$method == "CFOP" & res$status == "solved"],
                  res$state[res$method == "LBL" & res$status == "solved"])
if (length(both) > 0) {
  c_moves <- res$moves[res$method == "CFOP" & res$state %in% both]
  l_moves <- res$moves[res$method == "LBL" & res$state %in% both]
  cat(sprintf("\non the %d state(s) both solved: CFOP %.1f moves, LBL %.1f, ratio %.2f\n",
              length(both), mean(c_moves), mean(l_moves),
              mean(l_moves) / mean(c_moves)))
} else {
  cat("\nno state was solved by both methods\n")
}

fails <- res[res$status != "solved", ]
if (nrow(fails) > 0) {
  cat("\ndid not solve:\n")
  print(fails[, c("state", "method", "status", "seconds")], row.names = FALSE)
}
