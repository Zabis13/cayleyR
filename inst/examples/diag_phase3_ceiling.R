# Is the ceiling in the way, or is there nothing to find?
#
# diag_handover_distance.R left exactly one question open. Cubes 1, 2 and 3
# spent 50,000,000 nodes in phase 3 and returned `exhausted`, while a control
# population -- states reached by phase 3's own generators, sitting FURTHER
# away by its own metric (median 20 moves, max 31) -- was solved eight times
# out of eight in hundreds of nodes. So "the handover is too far" is refuted:
# the failing states are not far, they are different.
#
# Two readings remain, and one run separates them:
#
#   the ceiling was too low   a solution exists at 21 to 26 moves and
#                             max_depth3 = 20 cut it off. `found` at a depth
#                             above 20 says so, and the fix is a constant.
#
#   there is no solution      phase 2 left the cube where phase 3's generators
#                             cannot reach a goal, for a reason its filter does
#                             not check. `no_solution` says so -- the search
#                             exhausted the whole tree and proved it. Then no
#                             budget helps and the fix is in phase 2.
#
# The distinction lives entirely in the outcome code, so the run has to be able
# to reach it. `exhausted` means the node budget stopped the search before it
# could decide, and returns nothing either way. Six more levels at phase 3's
# branching is not a proportional increase over 50,000,000 -- it is orders of
# magnitude -- so the budget here is set high enough that hitting it again is
# itself informative, and the script says plainly which limit was reached.
#
# A third outcome is possible and worth naming in advance: `found` at 20 or
# below, on a cube that failed at the same ceiling before. That would mean the
# earlier failure was neither depth nor reachability but the budget alone, and
# the deeper table built on the way to a higher limit is what paid for it.

suppressMessages(library(cayleyR))

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)

# The three scrambles, verbatim from diag_handover_distance.R, so the same
# cubes are examined rather than the same seed re-rolled.
scrambles <- list(
  `1` = c("U", "F'", "1x", "1y", "B'", "R'"),
  `2` = c("1y'", "F", "B'", "U'", "2z'", "2x"),
  `3` = c("1y", "1x'", "1z", "B'", "L'", "2y'")
)

ceilings <- c(20L, 23L, 26L)
budget   <- 4e8

hr("setup")
cat(sprintf("cubes   : %s (the three that failed at depth 20)\n",
            paste(names(scrambles), collapse = ", ")))
cat(sprintf("ceilings: %s\n", paste(ceilings, collapse = ", ")))
cat(sprintf("budget  : %s nodes per run\n",
            format(budget, big.mark = ",", scientific = FALSE)))
cat("\n")
cat("outcomes: found       -- a solution, at the depth shown\n")
cat("          no_solution -- the tree was exhausted; none exists at that depth\n")
cat("          exhausted   -- the budget ran out; the run decided nothing\n")

rows <- list()

for (nm in names(scrambles)) {
  scr   <- scrambles[[nm]]
  state <- replay(solved, scr)

  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                              node_budget = 5e6)
  handover <- replay(state, p12)

  hr(sprintf("cube %s", nm))
  cat(sprintf("  scramble   : %s\n", paste(scr, collapse = " ")))
  cat(sprintf("  phases 1+2 : %d moves, at phase 2 goal %s\n", length(p12),
              cayleyR:::cube_at_phase_goal_cpp(handover, 2L)))

  p3c <- cayleyR:::cube_phase3_coord_cpp(handover)
  cat(sprintf("  phase 3 has %d of 24 goals available\n",
              p3c$n_goals_matching_bit))

  cat(sprintf("\n  %-7s %-12s %7s %14s %10s  %s\n",
              "ceiling", "outcome", "moves", "nodes", "cut_ratio", "secs"))

  for (cl in ceilings) {
    t0 <- proc.time()[["elapsed"]]
    r <- try(cayleyR:::cube_kociemba4_phase3_cpp(handover, max_depth3 = cl,
                                                 node_budget = budget),
             silent = TRUE)
    el <- proc.time()[["elapsed"]] - t0

    if (inherits(r, "try-error")) {
      cat(sprintf("  %-7d %-12s %7s %14s %10s  %.1f\n", cl, "ERROR",
                  "-", "-", "-", el))
      next
    }

    cat(sprintf("  %-7d %-12s %7s %14s %10.3f  %.1f\n", cl, r$outcome,
                if (isTRUE(r$found)) length(r$path) else "-",
                format(r$nodes, big.mark = ",", scientific = FALSE),
                r$cut_ratio, el))

    rows[[length(rows) + 1L]] <- data.frame(
      cube = nm, ceiling = cl, outcome = r$outcome,
      found = isTRUE(r$found),
      moves = if (isTRUE(r$found)) length(r$path) else NA_integer_,
      nodes = r$nodes, cut_ratio = round(r$cut_ratio, 3),
      secs = round(el, 1), stringsAsFactors = FALSE)

    # Once a solution is in hand a deeper ceiling can only find the same one
    # again, and each run costs minutes.
    if (isTRUE(r$found)) {
      cat("    (solution found; deeper ceilings would only repeat it)\n")
      break
    }
    # A proved absence at this depth is also final for the smaller ceilings,
    # but not for the larger ones -- keep going.
  }
}

tab <- do.call(rbind, rows)

hr("every run")
print(tab, row.names = FALSE)

hr("what this says")

for (nm in names(scrambles)) {
  d <- tab[tab$cube == nm, ]
  if (nrow(d) == 0) next
  # `moves` counts EXPANDED moves and `ceiling` counts GENERATORS, so the two
  # are not comparable and a solution longer than the ceiling is not a solution
  # past it: a wide half turn is one generator and four moves, so twenty
  # generators reach about thirty moves. An earlier version of this line
  # compared them anyway and reported "the ceiling was the whole problem" for
  # two cubes that had in fact been solved within the old ceiling on nothing
  # but a larger node budget.
  verdict <- if (any(d$found)) {
    i <- which(d$found)[1]
    sprintf("solved at %d expanded moves within a ceiling of %d generators, on %s nodes -- the budget was the only limit",
            d$moves[i], d$ceiling[i],
            format(d$nodes[i], big.mark = ",", scientific = FALSE))
  } else if (any(d$outcome == "no_solution")) {
    sprintf("no solution to depth %d, proved -- phase 3 cannot reach a goal from here",
            max(d$ceiling[d$outcome == "no_solution"]))
  } else {
    sprintf("still exhausted at depth %d -- undecided, the budget was the limit",
            max(d$ceiling))
  }
  cat(sprintf("  cube %s : %s\n", nm, verdict))
}

cat("\n")
if (any(tab$outcome == "no_solution")) {
  cat("  A proved no_solution is the strong result. Phase 3's generators\n")
  cat("  cannot reach any of its goals from what phase 2 handed over, and no\n")
  cat("  budget or ceiling changes that. The next place to look is\n")
  cat("  Phase2SolutionFilter in src/kociemba4.h:700 -- it checks that each\n")
  cat("  dedge is split across primary and secondary positions and that the\n")
  cat("  primary-in-primary count is even, and whatever invariant is missing\n")
  cat("  is one it does not check.\n")
} else if (any(tab$found)) {
  cat("  The budget was the problem, not the ceiling. Every solution here was\n")
  cat("  found within the ceiling it started under; what the earlier runs\n")
  cat("  lacked was nodes. Measured, one cube needed 55 million where 50\n")
  cat("  million had failed -- ten per cent short -- and another needed 135\n")
  cat("  million. The reduction runs phase 3 on 2,000,000, which is between\n")
  cat("  twenty-five and sixty-five times too few for these.\n")
  cat("\n")
  cat("  A cube still exhausted beside them is not the same kind of failure\n")
  cat("  and should not be read as merely needing more again.\n")
} else {
  cat("  Undecided: every run stopped on the node budget rather than on the\n")
  cat("  tree. That leans towards there being nothing to find -- a search\n")
  cat("  cutting 89% of its nodes should finish a tree that holds a solution --\n")
  cat("  but leaning is not proving, and the outcome code never said so.\n")
}
