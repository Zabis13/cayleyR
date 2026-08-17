#!/usr/bin/env Rscript
# Two solvers on the same cube, compared phase by phase.
#
# This file used to open with an argument that turned out to be its own answer,
# and the argument is kept here because it is a trap worth marking. It ran: a
# scramble of six quarter turns fails, phase 3 spends twenty million nodes on a
# cube whose solution cannot be more than six moves away, no honest IDA* does
# that, so the search must be looking in the wrong place.
#
# Every step of that is wrong, and the error is in the second clause. A
# six-move scramble is not a six-move problem for phase 3. Phase 3 turns
# seventeen generators where the cube has twenty-four moves: it has no single
# slice turns at all, so a slice the scramble used cannot be undone, only
# worked around. Measured, one Uw2 costs it four moves. Distances of twenty and
# more are ordinary, and the control in diag_handover_distance.R solves states
# at 31 in hundreds of nodes.
#
# What actually failed was this script's own node budget -- 2e6 against the
# package default of 5e7. Restored, the cubes reduce. bench_reduce_budget.R
# measured ten of ten at the default, several after twenty to fifty million
# nodes in phase 3.
#
# So the comparison below still earns its place, but not as a hunt for a broken
# phase. It is a second opinion:
#
#   1  scramble a cube, keeping the moves that made it
#   2  solve it with cube_solve4(), which works by reduction
#   3  run phases 1 and 2 alone, then the whole reduction
#   4  after each phase, ask two questions:
#        - did this phase reach its own goal?
#        - can the NEXT phase reach its goal from here?
#
# Step 4 was written to assign blame between phases, and it can -- but read it
# carefully. "Phase 3 could not finish from here" is a statement about the
# budget it was given, not about reachability: `exhausted` means the search ran
# out of nodes, and only `no_solution` means the tree was searched and held
# nothing. Reading the first as the second is what sent a day after phantoms.
#
# The inverse scramble is checked too, as an upper bound on the whole
# reduction's length -- not on any single phase's, for the reason above.
#
# Run with:  Rscript inst/examples/diag_kociemba4_vs_solve4.R
#            Rscript inst/examples/diag_kociemba4_vs_solve4.R 8    # 8 cubes
#            Rscript inst/examples/diag_kociemba4_vs_solve4.R 8 6  # 6 moves each

library(cayleyR)

args     <- commandArgs(trailingOnly = TRUE)
n_states <- if (length(args) >= 1) as.integer(args[[1]]) else 6L
n_moves  <- if (length(args) >= 2) as.integer(args[[2]]) else 6L

N           <- 4L
# The package default, deliberately, after a smaller value cost a day.
#
# This read 2e6 with a comment arguing that a phase needing millions of nodes
# on a six-move cube has already said what it has to say. The argument is
# wrong, and the reason is worth keeping: a six-move scramble is not a
# six-move problem for phase 3. It turns seventeen generators where the cube
# has twenty-four moves, so a slice the scramble used has to be worked around
# rather than undone -- a single Uw2 costs it four moves -- and the distances
# it faces are routinely past twenty.
#
# Measured by bench_reduce_budget.R: at 5e7 all ten cubes of a sample reduce,
# several after spending twenty to fifty million nodes in phase 3. At 2e6 the
# same cubes fail, and the failures read as a broken phase 3. They are not.
# Everything the earlier output said about phase 3 giving up was this line.
#
# Raising it further is not free either -- at 2e8 the same ten still reduce but
# take three times as long, because the deeper prune table costs more to fill
# than the search it saves.
node_budget <- 5e7

set.seed(2026)

g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# The inverse of a word: the moves reversed, each one undone. This is a
# solution to the scramble by construction, so its length is an upper bound on
# the true distance and a phase that cannot match it is not merely unlucky.
invert <- function(path) {
  inv <- rev(path)
  vapply(inv, function(m) {
    if (grepl("'$", m)) sub("'$", "", m)
    else if (grepl("2$", m)) m
    else paste0(m, "'")
  }, character(1), USE.NAMES = FALSE)
}

yn <- function(x) if (isTRUE(x)) "yes" else "no"

# Scramble, remembering the word.
#
# generate_state() is what every other script here uses, and it draws its word
# the same way this does -- sample.int with replacement over the move index,
# rejecting a word that lands back on the identity. It does not hand the word
# back, though, and the word is the point here: it gives an inverse that is
# known to solve the cube, and so an upper bound on the true distance. So the
# draw is repeated rather than called, deliberately matching generate_state()
# including its rejection of the identity.
scramble <- function(n, max_attempts = 100L) {
  names_all <- cube_move_names(N)
  id <- cube_identity(N)
  for (i in seq_len(max_attempts)) {
    word <- sample(names_all, n, replace = TRUE)
    state <- replay(id, word)
    if (!identical(state, id)) return(list(state = state, word = word))
  }
  stop("scramble: failed to produce a non-identity state", call. = FALSE)
}

hr("setup")
cat("cubes        : ", n_states, " scrambles of ", n_moves,
    " quarter turns\n", sep = "")
cat("node budget  : ", format(node_budget, scientific = FALSE, big.mark = ","),
    " per phase\n", sep = "")

# How the goals of each phase collapse under that phase's own coordinate. Not
# per cube -- it is a property of the solver -- so it is read once, up front.
hr("goals per phase coordinate")
cat(sprintf("  %-8s %8s  %14s\n", "phase", "goals", "distinct keys"))
for (ph in 1:3) {
  k <- cayleyR:::cube_phase_goal_keys_cpp(ph)
  cat(sprintf("  %-8d %8d  %14d\n", ph, k$goals, k$distinct_keys))
}

rows <- list()

for (i in seq_len(n_states)) {
  hr(paste("cube", i))
  sc <- scramble(n_moves)
  state <- sc$state

  cat("  scramble : ", paste(sc$word, collapse = " "), "\n", sep = "")
  cat("  state    : ", paste(state, collapse = " "), "\n", sep = "")

  # The word we know solves it. Its length bounds the true distance.
  back <- invert(sc$word)
  ok_back <- cube_is_colour_solved(replay(state, back))
  cat("  inverse  : ", paste(back, collapse = " "),
      "  (solves it: ", yn(ok_back), ")\n", sep = "")

  # ---- the solver that works
  t0 <- proc.time()[["elapsed"]]
  r4 <- try(cube_solve4(state), silent = TRUE)
  t_solve4 <- proc.time()[["elapsed"]] - t0
  solve4_ok <- !inherits(r4, "try-error") && isTRUE(r4$found) &&
               cube_is_colour_solved(replay(state, r4$path))
  cat(sprintf("\n  cube_solve4  : %s, %d moves, %.2f s\n",
              if (solve4_ok) "solved" else "FAILED",
              if (inherits(r4, "try-error")) 0L else length(r4$path),
              t_solve4))

  # ---- the cascade: a short leash on the reduction, then the solver above
  #
  # The two solvers on either side of it answer different questions.
  # cube_solve4 always finishes and its solutions are long -- the human pairing
  # method takes about 180 moves. The reduction is short when it lands and
  # never finishes when it does not, and no budget tells those apart in
  # advance. The cascade spends a bounded amount on the short answer and takes
  # the long one when that runs out, so what it is worth is measured in two
  # numbers: how often it gets the short path, and what the wait costs when it
  # does not.
  t0 <- proc.time()[["elapsed"]]
  rc <- try(cube_solve4_cascade(state), silent = TRUE)
  t_casc <- proc.time()[["elapsed"]] - t0
  casc_ok <- !inherits(rc, "try-error") && isTRUE(rc$solved)
  casc_method <- if (inherits(rc, "try-error")) "error" else rc$method
  casc_moves <- if (inherits(rc, "try-error")) NA_integer_ else rc$n_moves

  cat(sprintf("  cascade      : %s via %s, %s moves, %.2f s\n",
              if (casc_ok) "solved" else "FAILED", casc_method,
              if (is.na(casc_moves)) "?" else format(casc_moves),
              t_casc))

  # ---- the four-phase search, a phase at a time
  #
  # Phases 1 and 2 are run through the entry point that stops early, so each
  # one's output can be inspected before the next gets hold of it.
  cat("\n  four-phase search, stopping after each phase:\n")
  cat(sprintf("    %-9s %7s  %-11s  %-13s  %s\n",
              "after", "moves", "own goal", "p3 finishes", "note"))

  p1 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 1L,
                                             node_budget = node_budget)
  s1 <- replay(state, p1)
  g1 <- cayleyR:::cube_at_phase_goal_cpp(s1, 1L)

  p2 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                             node_budget = node_budget)
  s2 <- replay(state, p2)
  g2 <- cayleyR:::cube_at_phase_goal_cpp(s2, 2L)

  # Whether phase 3 can finish from where phase 2 left the cube. This has to be
  # a search: asking whether the state already SITS at phase 3's goal is a
  # different question, and its answer is "no" for every cube that still needs
  # work -- which would convict phase 2 on every row.
  p3run <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = node_budget)
  g2_next <- isTRUE(p3run$found)

  cat(sprintf("    %-9s %7d  %-11s  %-13s  %s\n", "phase 1", length(p1),
              yn(g1), "-",
              if (!g1) "phase 1 did not reach its goal" else ""))
  cat(sprintf("    %-9s %7d  %-11s  %-13s  %s\n", "phase 2", length(p2),
              yn(g2), yn(g2_next),
              if (!g2) "phase 2 did not reach its goal" else ""))
  cat(sprintf("    %-9s %7s  %-11s  %-13s  %s nodes\n", "phase 3 alone",
              if (g2_next) length(p3run$path) else 0L,
              yn(g2_next), "-",
              format(p3run$nodes, scientific = FALSE, big.mark = ",")))

  # The two parities, after phase 2 has had its say. Phase 2's filter enforces
  # the first, and refuses a solution whose value is odd.
  #
  # The second is not a thing to satisfy. Until 2026-08-14 this line read
  # "phase 3 goal wants 0" and invited the reading that a cube arriving with
  # the bit set is a cube phase 3 cannot solve. It is not: the twenty-four
  # goals split evenly between the two values -- Uw2, Rw2 and Fw2 flip the bit,
  # so the orbit halves -- and every state therefore has twelve goals to aim
  # at, whichever value it carries. Measured by diag_pll_bit_check.R, which
  # also confirms the bit is real PLL parity: a parity algorithm flips it,
  # ordinary turns do not, and the solved cube itself carries 1.
  #
  # So the number to watch is the last one. Zero would be a state with no
  # reachable goal; anything else means an "exhausted" below is a search that
  # ran out of budget, not a goal out of reach.
  par <- cayleyR:::cube_wing_parities_cpp(s2)
  p3c <- cayleyR:::cube_phase3_coord_cpp(s2)
  cat(sprintf("    parities after phase 2: primary-in-primary %d (filter wants 0), ",
              par$primary_in_primary))
  cat(sprintf("PLL bit %d, goals sharing it %d of 24\n",
              par$pll_bit, p3c$n_goals_matching_bit))

  # ---- the whole reduction, for the outcome codes and the node counts
  t0 <- proc.time()[["elapsed"]]
  red <- cube_kociemba4_reduce(state, node_budget = node_budget)
  t_red <- proc.time()[["elapsed"]] - t0
  rep3 <- cube_kociemba4_report()
  reduced <- length(red) > 0 && cube_is_reduced(replay(state, red))

  cat(sprintf("\n  reduction    : %s, %.2f s   (%s / %s / %s)\n",
              if (reduced) paste0("reduced in ", length(red), " moves")
              else "did not reduce",
              t_red, rep3$phase1, rep3$phase2, rep3$phase3))
  cat(sprintf("    nodes      : phase 1 %s, phase 2 %s, phase 3 %s\n",
              format(rep3$phase1_nodes, scientific = FALSE, big.mark = ","),
              format(rep3$phase2_nodes, scientific = FALSE, big.mark = ","),
              format(rep3$phase3_nodes, scientific = FALSE, big.mark = ",")))

  # ---- Kociemba as a whole solver, for the summary table
  #
  # `red` is the reduction alone: it turns the 4x4x4 into a 3x3x3 in disguise
  # and stops. Comparing its length against cube_solve4 and the cascade, which
  # return solved cubes, would be comparing two different jobs -- so the 3x3x3
  # stage is run on top of it here, the same way cube_solve4_cascade finishes
  # its own reduction.
  koc_ok <- FALSE
  koc_moves <- NA_integer_
  koc_path <- NULL
  t_koc <- t_red
  if (reduced) {
    t0 <- proc.time()[["elapsed"]]
    tail4 <- try(cube_solve4(replay(state, red)), silent = TRUE)
    t_koc <- t_red + (proc.time()[["elapsed"]] - t0)
    if (!inherits(tail4, "try-error")) {
      koc_path <- c(red, tail4$path)
      koc_ok <- cube_is_colour_solved(replay(state, koc_path))
      if (koc_ok) koc_moves <- length(koc_path)
    }
  }
  cat(sprintf("  kociemba     : %s, %s moves, %.2f s\n",
              if (koc_ok) "solved" else "FAILED",
              if (is.na(koc_moves)) "?" else format(koc_moves), t_koc))

  # ---- the verdict for this cube
  #
  # Written as a sentence because the useful output here is an accusation, not
  # a number: which phase is at fault, and on what evidence.
  verdict <- if (reduced) {
    "reduction finished"
  } else if (!g1) {
    "phase 1 failed to reach its own goal"
  } else if (!g2) {
    "phase 2 failed to reach its own goal"
  } else if (!g2_next) {
    # This read "PHASE 2 IS AT FAULT ... a cube phase 3 has no path from",
    # which does not follow: phase 3 stopping is `exhausted`, the budget, and
    # says nothing about whether a path exists. Two cubes accused this way were
    # later solved from the very same handover, on 55 and 135 million nodes.
    paste("phase 3 did not finish within this script's budget, run alone on",
          "phase 2's output. Whether a path exists is a separate question --",
          "read p3_outcome: `exhausted` is the budget, `no_solution` is proof")
  } else if (rep3$phase3 != "found") {
    paste("phase 3 finishes when run alone on this very state, but not inside",
          "the full reduction -- the phases interact, look at what differs")
  } else {
    "no solution found within the depth given"
  }
  cat("    -> ", verdict, "\n", sep = "")

  rows[[length(rows) + 1L]] <- data.frame(
    cube = i, scramble_len = n_moves,
    solve4 = solve4_ok, solve4_moves = if (solve4_ok) length(r4$path) else NA_integer_,
    p1_goal = g1, p2_goal = g2, p3_reachable_from_p2 = g2_next,
    pll_bit = par$pll_bit, prim_in_prim = par$primary_in_primary,
    p3_goals_matching = p3c$n_goals_matching_bit,
    p3_prune_bound = p3c$prune_bound,
    # Each solution after short_path_bfs(), so the comparison is of routes
    # rather than of how verbosely each solver spells the same one.
    solve4_short = if (solve4_ok)
      length(short_path_bfs(r4$path, state, group = g)$path) else NA_integer_,
    casc_short = if (casc_ok && !is.null(casc_path))
      length(short_path_bfs(casc_path, state, group = g)$path) else NA_integer_,
    reduced = reduced,
    p3_outcome = rep3$phase3,
    p3_nodes = rep3$phase3_nodes,
    casc_ok = casc_ok, casc_method = casc_method,
    casc_moves = casc_moves, casc_secs = round(t_casc, 2),
    solve4_moves_all = if (inherits(r4, "try-error")) NA_integer_
                       else length(r4$path),
    solve4_secs = round(t_solve4, 2),
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

hr("every cube")
print(tab, row.names = FALSE)

hr("what this says")

cat(sprintf("cube_solve4 solved            : %d of %d\n",
            sum(tab$solve4), nrow(tab)))
cat(sprintf("four-phase reduction finished : %d of %d\n",
            sum(tab$reduced), nrow(tab)))
cat(sprintf("phase 1 reached its goal      : %d of %d\n",
            sum(tab$p1_goal), nrow(tab)))
cat(sprintf("phase 2 reached its goal      : %d of %d\n",
            sum(tab$p2_goal), nrow(tab)))
# Named p3_reachable_from_p2, which overstates it: what it records is whether
# phase 3 finished within the budget this script gave it, and a phase that did
# not may still have had a solution waiting a few million nodes further on.
# Two of the three that failed here at 2e6 were solved later at 55 and 135
# million, from the same handover.
cat(sprintf("phase 3 finished in budget    : %d of %d\n",
            sum(tab$p3_reachable_from_p2), nrow(tab)))
cat(sprintf("cascade solved                : %d of %d\n",
            sum(tab$casc_ok), nrow(tab)))

# The outcome codes say which limit stopped it, and they are the difference
# between a slow search and an impossible one.
cat(sprintf("phase 3 outcomes              : %s\n",
            paste(names(table(tab$p3_outcome)), table(tab$p3_outcome),
                  sep = "=", collapse = " ")))

hr("summary")

cat(sprintf("%d states, %d moves from solved\n\n", nrow(tab), n_moves))

# The table demo_cube4_solve.R prints, for the two solvers this script runs.
# mean_short is the solution after short_path_bfs(), which works over any
# perm_group -- a cube solution shortens the same way a TopSpin path does -- so
# the figure is comparable across methods rather than an artefact of how
# verbosely each one spells its answer.
methods <- list(
  list(name = "cube_solve4", ok = tab$solve4,  moves = tab$solve4_moves_all,
       secs = tab$solve4_secs),
  list(name = "cascade",     ok = tab$casc_ok, moves = tab$casc_moves,
       secs = tab$casc_secs))

srows <- lapply(methods, function(m) {
  ok <- m$ok & !is.na(m$moves)
  data.frame(
    method     = m$name,
    solved     = sprintf("%d/%d", sum(m$ok), nrow(tab)),
    mean_moves = if (any(ok)) round(mean(m$moves[ok]), 1) else NA_real_,
    mean_short = if (any(ok)) round(mean(m$short[ok]), 1) else NA_real_,
    mean_sec   = if (any(ok)) round(mean(m$secs[ok]), 2) else NA_real_,
    stringsAsFactors = FALSE)
})
print(do.call(rbind, srows), row.names = FALSE)

# Head to head, on the states both solvers finished. Averaging each over
# whatever it happened to solve would flatter the one that gave up on the hard
# cubes, so the comparison is restricted to the states they share.
common <- which(tab$solve4 & tab$casc_ok &
                !is.na(tab$solve4_moves_all) & !is.na(tab$casc_moves))

if (length(common) > 0) {
  cat(sprintf("\non the %d state(s) every method solved:\n", length(common)))
  means <- c(cube_solve4 = mean(tab$solve4_moves_all[common]),
             cascade     = mean(tab$casc_moves[common]))
  best <- names(which.min(means))
  for (nm in names(sort(means))) {
    cat(sprintf("  %-12s %6.1f moves   %5.2fx %s\n",
                nm, means[[nm]], means[[nm]] / means[[best]], best))
  }
} else {
  cat("\nno state was solved by both, so there is nothing to compare\n")
}

cat("\n  cascade route:\n")
for (m in names(table(tab$casc_method))) {
  d <- tab[tab$casc_method == m, ]
  cat(sprintf("    %-12s %2d/%-2d %6.0f moves %6.2f s\n", m, nrow(d), nrow(tab),
              mean(d$casc_moves, na.rm = TRUE),
              mean(d$casc_secs, na.rm = TRUE)))
}

hr("where phase 3 fails")

# This section used to score the PLL bit as a predictor of phase 3 -- it read
# `p3_reachable_from_p2 == (pll_bit == 0)` and reported the agreement. That
# comparison has no meaning: both halves of the goal orbit are reachable from
# either value of the bit, so the bit predicts nothing and the score it printed
# was the coin-flip rate. Chasing it cost a session.
#
# What separates the cubes is not parity but budget, so that is what is
# reported: on the cubes phase 3 could not finish, whether it had goals to aim
# at, and what the prune table was able to say about the state it started from.
# A bound far below the true distance is a table that is not pruning.

bad <- tab[tab$p2_goal & !tab$p3_reachable_from_p2, ]
cat(sprintf("  phase 2 ok, phase 3 unfinished : %d of %d  (cubes %s)\n",
            nrow(bad), nrow(tab),
            if (nrow(bad) > 0) paste(bad$cube, collapse = ", ") else "-"))

if (nrow(bad) > 0) {
  cat(sprintf("    of those, reduced anyway     : %d\n", sum(bad$reduced)))
  cat(sprintf("    outcomes                     : %s\n",
              paste(names(table(bad$p3_outcome)), table(bad$p3_outcome),
                    sep = "=", collapse = " ")))
  cat(sprintf("    all had goals to aim at      : %s\n",
              if (all(bad$p3_goals_matching > 0)) "yes" else "NO"))
  cat(sprintf("    prune bound at handover      : %s\n",
              paste(bad$p3_prune_bound, collapse = ", ")))
}
