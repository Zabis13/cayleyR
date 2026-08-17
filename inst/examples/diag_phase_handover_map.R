# What each phase may turn, and what the cube looks like as it passes between
# them.
#
# Everything here is measured through the solver's own entry points rather than
# read off the generator lists in src/kociemba4.h. The comments there are good,
# but a comment is not a measurement -- and this file has been wrong before:
# the wide half turns under phase 3 once had layer 1 written where layer 2
# belongs, so the phase searched a set other than the one it documented.
#
# Three questions, in order:
#
#   1. Are the moves the same across phases? No -- each phase turns less than
#      the one before, which is what keeps its search shallow and stops it
#      undoing the phase before it. The table shows exactly what each may turn
#      and what phase N lost relative to phase N-1.
#
#   2. How does the state pass from one phase to the next? It does not get
#      converted. The same PieceState keeps being turned by a smaller set of
#      moves; what changes is the Deriver each phase views it through --
#      d1, d2, d3 -- which crushes the full state down to the coordinate that
#      phase can distinguish. Two states differing only in what a phase's
#      coordinate ignores are the same state to it.
#
#   3. What does that look like along a real solution? Walk a reduction move by
#      move and mark where each phase's goal is reached and where the next
#      phase's coordinate starts to move. A phase that reaches its goal while
#      the next phase's coordinate is still far from any of its goals is the
#      handover the whole investigation has been circling.
#
# One asymmetry in what can be measured: phase 3 exposes its coordinate
# (cube_phase3_coord_cpp), phases 1 and 2 do not. So for those two the reading
# is at-goal / not-at-goal rather than a distance, and that is a limit of the
# instruments, not a finding.

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

atg <- function(s, ph) cayleyR:::cube_at_phase_goal_cpp(s, ph)

# ---- 1. The move sets ------------------------------------------------------
hr("what each phase may turn")

gens <- lapply(1:3, function(ph) cayleyR:::cube_phase_generators_cpp(ph))
names(gens) <- paste0("phase", 1:3)

for (ph in 1:3) {
  g <- gens[[ph]]
  cat(sprintf("\n  phase %d : %d moves\n", ph, length(g$names)))
  cat(sprintf("    %s\n", paste(g$names, collapse = " ")))
}

cat("\n  what each phase lost relative to the one before:\n")
for (ph in 2:3) {
  gone <- setdiff(gens[[ph - 1]]$names, gens[[ph]]$names)
  kept <- intersect(gens[[ph - 1]]$names, gens[[ph]]$names)
  added <- setdiff(gens[[ph]]$names, gens[[ph - 1]]$names)
  cat(sprintf("\n    phase %d -> %d : kept %d, dropped %d\n",
              ph - 1, ph, length(kept), length(gone)))
  cat(sprintf("      dropped : %s\n", paste(gone, collapse = " ")))
  if (length(added) > 0) {
    # Phase 3's wide half turns are words, not single layer turns, so they
    # appear here as names phase 2 never had rather than as a widening of what
    # it could do.
    cat(sprintf("      new     : %s\n", paste(added, collapse = " ")))
  }
}

# ---- 2. Goals, and how coarse each coordinate is ---------------------------
#
# Several goals is the solved cube in several orientations. Phase 3's
# generators cannot produce 1x2 without a whole-cube rotation they do not have,
# so rotating the goal supplies what the generators lack -- a solution may
# finish with the cube turned, which costs nothing because phase 4 hands it to
# a 3x3x3 solver that does not care.
#
# distinct_keys below goals would be the coordinate merging goals it cannot
# tell apart. Equal counts mean it tells all of them apart.
hr("goals per phase")

cat(sprintf("  %-7s %7s %15s  %s\n", "phase", "goals", "distinct keys", "moves"))
for (ph in 1:3) {
  k <- cayleyR:::cube_phase_goal_keys_cpp(ph)
  cat(sprintf("  %-7d %7d %15d  %d\n", ph, k$goals, k$distinct_keys,
              length(gens[[ph]]$names)))
}

# How many of a phase's own moves take the solved cube off that phase's goal.
# A move that does not is one the coordinate cannot see -- either because it
# carries one goal to another (harmless) or because the coordinate is too
# coarse to notice (not).
cat("\n  moves that leave the phase's own goal, from solved:\n")
for (ph in 1:3) {
  nm <- gens[[ph]]$names
  # The generator names are the phase's own alphabet; the wide half turns are
  # words that cube_moves() does not hold, so only the ones it does are walked.
  known <- nm[nm %in% names(mv)]
  off <- known[vapply(known, function(m) !atg(replay(solved, m), ph),
                      logical(1))]
  cat(sprintf("    phase %d : %d of %d walkable  (%s)\n", ph, length(off),
              length(known),
              if (length(off) == 0) "none" else paste(off, collapse = " ")))
}

# ---- 3. A real solution, phase by phase ------------------------------------
#
# The handover made visible: run the reduction, then walk its moves one at a
# time and mark where each phase's goal is met.
hr("along a real reduction")

set.seed(2026)
scr   <- sample(names(mv), 6L, replace = TRUE)
state <- replay(solved, scr)
cat(sprintf("  scramble : %s\n", paste(scr, collapse = " ")))

p12 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                            node_budget = 5e6)
p1  <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 1L,
                                            node_budget = 5e6)
cat(sprintf("  phase 1 contributed %d moves, phases 1+2 %d\n",
            length(p1), length(p12)))

handover <- replay(state, p12)
cat(sprintf("  at handover : phase 1 goal %s, phase 2 goal %s, phase 3 goal %s\n",
            atg(handover, 1L), atg(handover, 2L), atg(handover, 3L)))

# Phase 3's view of that state: how far the prune table thinks it is, and how
# many goals it has available. n_goals_matching_bit is the number that matters
# -- zero would mean no goal is reachable, and anything else means the search
# has somewhere to aim.
p3c <- cayleyR:::cube_phase3_coord_cpp(handover)
cat(sprintf("  phase 3 sees: prune bound %d, goals sharing its parity %d of 24\n",
            p3c$prune_bound, p3c$n_goals_matching_bit))

cat("\n  step by step (goal columns are at-goal after that move):\n")
cat(sprintf("  %4s %-6s  %-5s %-5s %-5s\n", "step", "move", "ph1", "ph2", "ph3"))

cur <- state
cat(sprintf("  %4s %-6s  %-5s %-5s %-5s   <- scrambled\n", "0", "-",
            atg(cur, 1L), atg(cur, 2L), atg(cur, 3L)))
for (j in seq_along(p12)) {
  cur <- cur[mv[[p12[j]]]]
  mark <- if (j == length(p1)) "   <- phase 1 ends"
          else if (j == length(p12)) "   <- phase 2 ends, handover"
          else ""
  cat(sprintf("  %4d %-6s  %-5s %-5s %-5s%s\n", j, p12[j],
              atg(cur, 1L), atg(cur, 2L), atg(cur, 3L), mark))
}

hr("what this says")
cat("  The move sets shrink phase by phase, and the state is never converted\n")
cat("  between them -- the same cube keeps being turned by fewer moves, seen\n")
cat("  through a different coordinate each time. A phase reaching its goal\n")
cat("  says only that ITS coordinate is satisfied; the next phase's coordinate\n")
cat("  may still be far from any of its own goals, and how far is what decides\n")
cat("  whether the next phase can afford the search.\n")
