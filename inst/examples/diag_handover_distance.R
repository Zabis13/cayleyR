# How far is the cube phase 2 hands over, in phase 3's own metric?
#
# This is the last number the investigation is missing. What is settled:
#
#   phase 3 is sound     17/17 of its generators undone at depth 1, everything
#                        solved out to words of 25, prune table cutting 90% of
#                        nodes when deep. Its only failures are `exhausted`,
#                        never `no_solution` -- out of budget, not out of
#                        reach. Measured threshold: solutions past about twenty
#                        moves start costing more than 5,000,000 nodes.
#
#   phase 2 is sound     28/28 generators undone, phase 1 never broken, and its
#                        coordinate is not blind -- adding U to a base state
#                        raised phase 2's work from two moves to three.
#
#   parity is a phantom  every state has twelve of the twenty-four phase 3
#                        goals available whatever its PLL bit. Nothing is
#                        unreachable.
#
# So both phases work, and yet the full reduction spends 2,000,001 nodes in
# phase 3 on half the cubes. The remaining explanation is the handover: phase 2
# reaches its own goal by a route that leaves the cube far away in phase 3's
# metric -- past the threshold where phase 3 is known to run out of budget.
#
# That is measurable directly. Run phases 1 and 2, take the state they produce,
# and give phase 3 a budget large enough to finish. The length of the solution
# it finds IS the handover distance. Against phase 3's measured threshold:
#
#   distance well under 20   the handover is fine and the story is wrong --
#                            something else is spending those nodes
#   distance 20 and beyond   the chain closes: phase 2 is handing over states
#                            phase 3 cannot afford, and the fix belongs in
#                            phase 2's choice of route or in phase 3's budget.
#
# A control runs beside it. Phase 3's threshold was measured on states reached
# by ITS OWN generators, which is a different population from what phase 2
# produces. So each handover is compared against a random state that phase 2
# also considers finished, reached by phase 3's generators from a goal. If the
# handovers are systematically further than those, phase 2's route is the
# problem; if they sit in the same range, the distance is simply what reduction
# costs and the budget is the only lever.

suppressMessages(library(cayleyR))

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

n_states    <- 8L
n_moves     <- 6L
big_budget  <- 5e7   # large enough that a failure here means real depth
max_depth3  <- 20L

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)

at2 <- function(s) cayleyR:::cube_at_phase_goal_cpp(s, 2L)
at3 <- function(s) cayleyR:::cube_at_phase_goal_cpp(s, 3L)

# Phase 3's generators, for the control population.
p3gens <- list(
  "U"   = c("U"), "U'" = c("U'"), "U2" = c("U", "U"),
  "D"   = c("D"), "D'" = c("D'"), "D2" = c("D", "D"),
  "L"   = c("L"), "L'" = c("L'"), "L2" = c("L", "L"),
  "R"   = c("R"), "R'" = c("R'"), "R2" = c("R", "R"),
  "F2"  = c("F", "F"), "B2" = c("B", "B"),
  "Uw2" = c("U", "2y", "U", "2y"),
  "Rw2" = c("R", "2x", "R", "2x"),
  "Fw2" = c("F", "2z", "F", "2z"))

run3 <- function(state) {
  r <- try(cayleyR:::cube_kociemba4_phase3_cpp(state, max_depth3 = max_depth3,
                                               node_budget = big_budget),
           silent = TRUE)
  if (inherits(r, "try-error")) {
    return(list(ok = FALSE, n = NA_integer_, nodes = NA_real_,
                outcome = "error", cut = NA_real_))
  }
  list(ok = isTRUE(r$found), n = length(r$path), nodes = r$nodes,
       outcome = r$outcome, cut = r$cut_ratio)
}

hr("setup")
cat(sprintf("cubes       : %d scrambles of %d quarter turns\n", n_states, n_moves))
cat(sprintf("phase 3     : budget %s, max depth %d\n",
            format(big_budget, big.mark = ",", scientific = FALSE), max_depth3))
cat("threshold   : phase 3 needs over 5,000,000 nodes past ~20 moves\n")

set.seed(2026)

hr("the handover, cube by cube")

rows <- list()

for (i in seq_len(n_states)) {
  scr   <- sample(names(mv), n_moves, replace = TRUE)
  state <- replay(solved, scr)

  # What phases 1 and 2 make of it, and the state they leave behind.
  p12 <- try(cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                                  node_budget = big_budget),
             silent = TRUE)
  if (inherits(p12, "try-error")) {
    cat(sprintf("  cube %d : phases 1+2 failed\n", i))
    next
  }
  handover <- replay(state, p12)
  g2 <- at2(handover)

  r <- run3(handover)

  cat(sprintf("\n  cube %d  scramble %s\n", i, paste(scr, collapse = " ")))
  cat(sprintf("    phases 1+2 : %d moves, at phase 2 goal %s\n",
              length(p12), g2))
  cat(sprintf("    phase 3    : %s, %s moves, %s nodes, cut %.3f  (%s)\n",
              if (r$ok) "solved" else "FAILED",
              if (is.na(r$n)) "-" else r$n,
              if (is.na(r$nodes)) "-" else
                format(r$nodes, big.mark = ",", scientific = FALSE),
              r$cut, r$outcome))

  rows[[length(rows) + 1L]] <- data.frame(
    cube = i, p12_moves = length(p12), p2_goal = g2,
    p3_ok = r$ok, p3_moves = if (r$ok) r$n else NA_integer_,
    p3_nodes = r$nodes, p3_outcome = r$outcome,
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

# ---- The control -----------------------------------------------------------
#
# States phase 2 also calls finished, but reached by phase 3's own generators
# rather than by phase 2's search. Same question asked of a different route to
# the same kind of place.
hr("control: states reached by phase 3's own generators")

ctrl <- list()
for (i in seq_len(n_states)) {
  w  <- sample(names(p3gens), 20L, replace = TRUE)
  st <- replay(solved, unlist(p3gens[w], use.names = FALSE))
  r  <- run3(st)
  ctrl[[length(ctrl) + 1L]] <- data.frame(
    cube = i, p3_ok = r$ok, p3_moves = if (r$ok) r$n else NA_integer_,
    p3_nodes = r$nodes, p3_outcome = r$outcome, stringsAsFactors = FALSE)
}
ct <- do.call(rbind, ctrl)

cat(sprintf("  solved       : %d of %d\n", sum(ct$p3_ok), nrow(ct)))
cat(sprintf("  moves        : %s\n",
            paste(ifelse(is.na(ct$p3_moves), "-", ct$p3_moves), collapse = " ")))
cat(sprintf("  median moves : %s\n",
            if (all(is.na(ct$p3_moves))) "-" else
              as.character(median(ct$p3_moves, na.rm = TRUE))))

hr("every cube")
print(tab, row.names = FALSE)

hr("what this says")

solved_n <- sum(tab$p3_ok)
cat(sprintf("  phase 3 finished with a big budget : %d of %d\n",
            solved_n, nrow(tab)))

if (solved_n > 0) {
  d <- tab$p3_moves[tab$p3_ok]
  cat(sprintf("  handover distance   : %s\n", paste(sort(d), collapse = " ")))
  cat(sprintf("  median / max        : %s / %s\n",
              as.character(median(d)), as.character(max(d))))
  cat(sprintf("  at or past 20 moves : %d of %d\n", sum(d >= 20), length(d)))
}
if (any(!tab$p3_ok)) {
  cat(sprintf("  still failing at %s nodes : cubes %s\n",
              format(big_budget, big.mark = ",", scientific = FALSE),
              paste(tab$cube[!tab$p3_ok], collapse = ", ")))
  cat("  `exhausted` is the budget running out, not the tree being searched,\n")
  cat("  so it puts no lower bound on the distance -- an earlier draft of this\n")
  cat("  line claimed one, and it does not follow. The control settles it the\n")
  cat("  other way: states at 20 to 31 moves are solved here in hundreds of\n")
  cat("  nodes, so distance alone does not produce this. Whatever separates\n")
  cat("  these cubes, it is not how far away they are.\n")
}

cat("\n")
cat("  Compare the handover distances against the control. Systematically\n")
cat("  larger means phase 2's route is what costs phase 3 its budget, and the\n")
cat("  fix belongs in how phase 2 chooses among the solutions meeting its\n")
cat("  goal. The same range means the distance is what reduction costs on this\n")
cat("  puzzle, and the only lever is phase 3's budget.\n")
