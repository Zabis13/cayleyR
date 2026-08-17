# What the phase 3 prune table is worth by the time the search gives up.
#
# The parity story is closed: diag_pll_bit_check.R showed every state has
# twelve of the twenty-four goals to aim at, whichever value its PLL bit
# carries, so nothing is unreachable and every "exhausted" is a search that ran
# out of budget. This asks why it ran out.
#
# The suspect is the heuristic. The table starts empty -- build_prune_table is
# called with max_depth 0 (kociemba4.h:1102), so it holds only the goals -- and
# is filled during the search, one round per iterative-deepening level, to half
# the depth about to be searched (kociemba_core.h:1034). Two things can stop
# that working, and they need opposite fixes:
#
#   the table is too small   collisions throw the walk away. First-writer-wins
#                            keeps the bound admissible, so the search stays
#                            correct, but the entry that survives belongs to
#                            some other state and the bound it gives is far
#                            below the true distance. waste_ratio counts this.
#
#   the table keeps growing  grow_to() rebuilds from scratch -- the hash
#                            depends on the size -- so built_depth goes back to
#                            zero and every level filled so far is walked
#                            again. n_grows counts this.
#
# Both end as a bound of built_depth + 1 on a state the fill never recorded,
# which is what a search with no heuristic looks like from outside.

suppressMessages(library(cayleyR))

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

node_budget <- 2e6
n_states    <- 6L
n_moves     <- 6L

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)

set.seed(2026)   # the same cubes diag_kociemba4_vs_solve4.R reports on

hr("setup")
cat(sprintf("cubes       : %d scrambles of %d quarter turns\n", n_states, n_moves))
cat(sprintf("node budget : %s per phase\n",
            format(node_budget, big.mark = ",", scientific = FALSE)))

# What the table looks like before anything has searched, for contrast. A bound
# of 1 everywhere here is expected and is not the defect -- it is a table that
# holds the goals and nothing else yet.
hr("before any search")
p0 <- cayleyR:::cube_phase3_coord_cpp(solved)
cat(sprintf("  bound on the solved cube : %d   (0 = it is a goal)\n",
            p0$prune_bound))
cat(sprintf("  table_built_depth        : %d\n", p0$table_built_depth))

hr("after the reduction has run")

rows <- list()

for (i in seq_len(n_states)) {
  scr   <- sample(names(mv), n_moves, replace = TRUE)
  state <- replay(solved, scr)

  t0  <- proc.time()[["elapsed"]]
  red <- cube_kociemba4_reduce(state, node_budget = node_budget)
  el  <- proc.time()[["elapsed"]] - t0

  rep  <- cube_kociemba4_report()
  reduced <- length(red) > 0 && cube_is_reduced(replay(state, red))
  p3   <- cayleyR:::cube_kociemba4_tables_cpp()$phase3

  cat(sprintf("\n  cube %d  %s  %.1f s  phase3 %s, %s nodes\n",
              i, if (reduced) "reduced" else "NOT reduced", el,
              rep$phase3,
              format(rep$phase3_nodes, big.mark = ",", scientific = FALSE)))
  cat(sprintf("    table   : size %s  built_depth %d  grows %d  fill %.3f\n",
              format(p3$size, big.mark = ",", scientific = FALSE),
              p3$built_depth, p3$n_grows, p3$fill_ratio))
  cat(sprintf("    fill    : visits %s  writes %s  collisions %s  waste %.3f\n",
              format(p3$n_visits, big.mark = ",", scientific = FALSE),
              format(p3$n_writes, big.mark = ",", scientific = FALSE),
              format(p3$n_collisions, big.mark = ",", scientific = FALSE),
              p3$waste_ratio))
  cat(sprintf("    depths  : %s\n",
              paste(sprintf("%d:%s", seq_along(p3$depth_counts) - 1L,
                            format(p3$depth_counts, big.mark = "",
                                   scientific = FALSE, trim = TRUE)),
                    collapse = "  ")))

  rows[[length(rows) + 1L]] <- data.frame(
    cube = i, reduced = reduced, outcome = rep$phase3,
    nodes = rep$phase3_nodes, secs = round(el, 2),
    size = p3$size, built_depth = p3$built_depth, grows = p3$n_grows,
    fill_ratio = round(p3$fill_ratio, 3),
    visits = p3$n_visits, writes = p3$n_writes,
    waste = round(p3$waste_ratio, 3),
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

hr("every cube")
print(tab, row.names = FALSE)

hr("what this says")

cat(sprintf("  reduced                     : %d of %d\n",
            sum(tab$reduced), nrow(tab)))
cat(sprintf("  deepest table built         : %d  (of the levels searched)\n",
            max(tab$built_depth)))
cat(sprintf("  mean waste ratio            : %.3f\n", mean(tab$waste)))
cat(sprintf("  tables that grew more than once : %d\n", sum(tab$grows > 1)))

cat("\n")
cat("  A high waste ratio with few grows is a table too small for the level\n")
cat("  it is filling: raise the size it is allowed to reach. Many grows is the\n")
cat("  opposite problem -- the table is being rebuilt and refilled repeatedly,\n")
cat("  and each rebuild throws away every level already walked. A built_depth\n")
cat("  that stays at 0 or 1 while the search runs to twelve levels means the\n")
cat("  heuristic was never in play at all, whatever the cause.\n")
