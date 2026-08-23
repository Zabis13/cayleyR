# How fast phase 3 of the 4x4x4 reduction grows, level by level.
#
# Why this exists. Measured on a ten-move scramble, phase 3 fails the same way
# whatever it is given: raising max_depth3 from 12 to 18 changes nothing (it
# drowns on level 12 and never reaches the ceiling), and raising node_budget
# from 20 million to 200 million changes nothing either. Neither limit is the
# binding one -- the prune table is. It is too weak to cut level 12 down to a
# searchable size, so the search goes wide, and budget only pushes the wall
# back linearly against a tree that grows exponentially.
#
# What this script measures is that growth: how many nodes each level costs
# and what the ratio between consecutive levels is. That ratio is the number
# that says how much stronger the table would have to be, and how deep a table
# would have to reach to make the phase finish.
#
# Run with:
#   Rscript inst/examples/bench_phase3_branching.R
#
# It walks levels from cheap to expensive and stops as soon as one exceeds the
# budget, so it costs seconds, not the sixteen minutes a single deep run does.

library(cayleyR)

SCRAMBLE_SEED  <- 8101
SCRAMBLE_MOVES <- 10

# Per-level budget. Small on purpose: the levels below the wall cost millions
# of nodes, not hundreds of millions, and the first level that blows past this
# is the wall itself -- which is the answer, not a failure to measure.
LEVEL_BUDGET <- 5e6

# Levels to walk. Starts below anything interesting so the ratios have a base
# to be measured from.
DEPTHS <- 6:14

# Nodes between progress lines from inside the search. The cheap levels finish
# before saying anything; the wall level is the one that needs it, and there a
# line every half million nodes shows the search is running rather than hung.
PROGRESS_EVERY <- 5e5

cat(sprintf("scramble: seed %d, %d moves\n", SCRAMBLE_SEED, SCRAMBLE_MOVES))
cat(sprintf("per-level budget: %.0f nodes\n\n", LEVEL_BUDGET))

set.seed(SCRAMBLE_SEED)
state <- generate_state(group = cube_group(4), n_moves = SCRAMBLE_MOVES)

previous_total <- 0
previous_level <- 0
rows <- list()

for (depth in DEPTHS) {
  # Announced before the run, not after: the level that hits the wall is the
  # slow one, and a heading that appears only on completion leaves the screen
  # blank during the part worth watching.
  cat(sprintf("--- depth %d ---\n", depth))
  flush.console()

  started <- Sys.time()
  # Each run searches to `depth` and no further, so the nodes it reports are
  # everything up to and including that level. The level's own cost is the
  # difference from the run before -- there is no way to ask for one level in
  # isolation, since iterative deepening re-walks the shallow ones every time.
  path <- cube_kociemba4_reduce(state, max_depth3 = depth,
                                node_budget = LEVEL_BUDGET,
                                progress_every = PROGRESS_EVERY)$path
  elapsed <- as.numeric(Sys.time() - started, units = "secs")
  report <- cube_kociemba4_report()

  total <- report$phase3_nodes
  level <- total - previous_total
  ratio <- if (previous_level > 0) sprintf("%.1f", level / previous_level) else "-"

  cat(sprintf("  cumulative %d, this level %d, ratio %s, %s, %.1fs\n\n",
              total, level, ratio, report$phase3, elapsed))
  flush.console()

  rows[[length(rows) + 1]] <- data.frame(
    depth = depth, cumulative = total, level = level,
    ratio = ratio, outcome = report$phase3, sec = round(elapsed, 1),
    stringsAsFactors = FALSE
  )

  # A level that ran out of budget did not finish, so its node count is the
  # budget rather than its true cost, and the ratio computed from it would be
  # a floor, not a measurement. Stop rather than print numbers that look like
  # data and are not.
  if (report$phase3 != "no_solution") {
    if (report$phase3 == "found") {
      cat(sprintf("\nsolved at depth %d, path %d moves\n", depth, length(path)))
    } else {
      cat(sprintf("\nwall at depth %d: the level did not fit in %.0f nodes.\n",
                  depth, LEVEL_BUDGET))
      cat("Everything below it did fit, so the ratios above are real; this\n")
      cat("level's cost is at least the budget and its true size is unknown.\n")
    }
    break
  }

  previous_level <- level
  previous_total <- total
}

cat("\n===== summary =====\n")
print(do.call(rbind, rows), row.names = FALSE)

cat("\nThe ratio column is the branching factor the prune table is failing to\n")
cat("suppress. A table that reached one level deeper would divide the cost of\n")
cat("the wall level by roughly that factor.\n")
