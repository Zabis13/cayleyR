# Can the four-phase solver actually solve a 4x4x4, and up to what scramble?
#
# Three translation bugs were fixed in the phases (centre_class, goals2 and
# phase3_gens4), and the reduction went from seven of eight scrambles to eight
# of eight. What that leaves open is the question worth asking: how hard a
# cube can it finish now? Before the fixes the answer was "five moves, mostly"
# -- ten-move scrambles failed four times out of four.
#
# cube_solve4 is measured alongside as the reference. It always finishes, by
# algorithm rather than search, at about three hundred moves. The four-phase
# solver is worth having only where it is much shorter, so the comparison is
# the point rather than a formality.
#
# Run with:
#   Rscript inst/examples/bench_solve_scaling.R

library(cayleyR)

SCRAMBLE_LENGTHS <- c(5, 10, 20)
PER_LENGTH <- 4
BUDGET <- 5e7

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)
apply_path <- function(state, path) {
  for (mv in path) state <- state[moves4[[mv]]]
  state
}

for (n_moves in SCRAMBLE_LENGTHS) {
  cat(sprintf("=== scrambles of %d moves ===\n", n_moves))

  solved <- 0L
  lengths <- integer(0)
  times <- numeric(0)
  ref_lengths <- integer(0)

  for (i in seq_len(PER_LENGTH)) {
    set.seed(30000 + n_moves * 100 + i)
    scramble <- generate_state(group = cube_group(4), n_moves = n_moves)

    started <- Sys.time()
    result <- cube_kociemba4(scramble, node_budget = BUDGET)
    elapsed <- as.numeric(Sys.time() - started, units = "secs")
    times <- c(times, elapsed)

    # Solved means solved: apply the path and look at the cube, rather than
    # trusting the flag the search returned.
    ok <- isTRUE(result$found) &&
          cube_is_colour_solved(apply_path(scramble, result$path))
    if (ok) {
      solved <- solved + 1L
      lengths <- c(lengths, length(result$path))
    }

    reference <- cube_solve4(scramble)
    if (is.list(reference)) reference <- reference$path
    ref_lengths <- c(ref_lengths, length(reference))

    cat(sprintf("  %d: %-7s %4s moves  %6.1fs   (cube_solve4: %d moves)\n",
                i, if (ok) "solved" else "FAILED",
                if (ok) length(result$path) else "-", elapsed,
                length(reference)))
    flush.console()
  }

  cat(sprintf("  -> %d of %d solved", solved, PER_LENGTH))
  if (length(lengths)) {
    cat(sprintf(", mean %.0f moves against cube_solve4's %.0f (%.1fx shorter)",
                mean(lengths), mean(ref_lengths),
                mean(ref_lengths) / mean(lengths)))
  }
  cat(sprintf(", median %.1fs\n\n", median(times)))
}

cat("The four-phase solver earns its keep by the length ratio; where it fails\n")
cat("cube_solve4 still finishes, so the cube is always solvable -- the\n")
cat("question is only whether it is solvable *briefly*.\n")
