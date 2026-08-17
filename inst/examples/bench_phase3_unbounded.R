# Does phase 3 finish if it is simply allowed to?
#
# The audit against twips found one categorical difference between their
# 4x4x4 solver and this one: they have no node budget at all. Their
# `IndividualSearchOptions` knows only minimum solutions and depth bounds, and
# for phase 3 even the depth bound is commented out -- that search runs until
# it wins. Ours gives up at fifty million nodes and returns an empty path.
#
# Everything else matched: orbits, centre masks, generator sets, the phase-2
# goal list and solution filter, the prune table's shape and size cap, the
# metric. So this script tests the one thing left: raise the ceiling far enough
# that it stops binding, and see whether phase 3 was ever stuck or merely
# stopped.
#
# Two things to read off the result, not one:
#
#   - whether it finishes at all, and
#   - if it does, how many nodes it actually needed.
#
# The second is the number missing from every earlier measurement. Level 11 was
# never counted to completion, so the branching factor of 2.6 quoted earlier is
# a lower bound, not a measurement, and no honest estimate of the cost of level
# 12 or beyond can be built on it.
#
# Run with:
#   Rscript inst/examples/bench_phase3_unbounded.R
#
# This can run for a long time. That is the point of it; the progress lines are
# there so a long run can be told from a hung one.

library(cayleyR)

SCRAMBLE_SEED  <- 8101
SCRAMBLE_MOVES <- 10

# High enough not to bind, rather than "correct". Neither is a proposal for a
# default -- an interactive function cannot search without a ceiling, and what
# that ceiling should be has to be calibrated over a sample of scrambles, not
# from one seed. Across eight five-move scrambles the cost ranged from 47 nodes
# to fifty million, six orders of magnitude, so one number here would say
# almost nothing about the next cube.
MAX_DEPTH3   <- 20L
NODE_BUDGET  <- 3e8
PROGRESS_EVERY <- 1e7

cat(sprintf("scramble: seed %d, %d moves\n", SCRAMBLE_SEED, SCRAMBLE_MOVES))
cat(sprintf("max_depth3 %d, node_budget %.0f\n\n", MAX_DEPTH3, NODE_BUDGET))

set.seed(SCRAMBLE_SEED)
state <- generate_state(group = cube_group(4), n_moves = SCRAMBLE_MOVES)

started <- Sys.time()
path <- cube_kociemba4_reduce(state,
                              max_depth3 = MAX_DEPTH3,
                              node_budget = NODE_BUDGET,
                              progress_every = PROGRESS_EVERY)
elapsed <- as.numeric(Sys.time() - started, units = "secs")
report <- cube_kociemba4_report()

cat(sprintf("\nphase1 %-11s %12d nodes\n", report$phase1, report$phase1_nodes))
cat(sprintf("phase2 %-11s %12d nodes\n", report$phase2, report$phase2_nodes))
cat(sprintf("phase3 %-11s %12d nodes\n", report$phase3, report$phase3_nodes))
cat(sprintf("%.1f seconds, path %d moves\n", elapsed, length(path)))

if (length(path)) {
  moves4 <- cube_moves(4)
  names(moves4) <- cube_move_names(4)
  final <- state
  for (mv in path) final <- final[moves4[[mv]]]

  cat(sprintf("reduced: %s\n", cube_is_reduced(final)))
  cat("\nPhase 3 was stopping short, not stuck. The nodes it needed are the\n")
  cat("figure above -- one sample, so treat it as an order of magnitude and\n")
  cat("calibrate any default over a spread of scrambles.\n")
} else {
  cat("\nStill unfinished. The budget was not the whole story, so the next\n")
  cat("question is where the cost actually goes -- and note that the prune\n")
  cat("table is not the obvious suspect: twips uses the same plain hash table\n")
  cat("with the same size cap and the same depth/2 rule, with no symmetry\n")
  cat("compression on either side.\n")
}
