#!/usr/bin/env Rscript
# Cubes scrambled with outer face turns only.
#
# diag_scramble_shape.R drew one such cube out of 120 and did not solve it.
# One cube is not evidence, but it is the cheapest thing left to check, and it
# is the case where the right answer is least in doubt.
#
# An outer face turn moves no centre off its face and splits no wing pair. So a
# cube scrambled with U, R, F, D, L, B alone is ALREADY REDUCED: its centres
# are built, its wings are paired, and it is a 3x3x3 with fat pieces from the
# start. Phases 1, 2 and 3 have nothing to do, and the reduction should return
# an empty word instantly.
#
# That makes this a sharper test than any short-scramble sweep. There is no
# search to get wrong, no depth to run out of, no handover to botch. If the
# reduction fails here it is not a slow search or an unlucky cube -- it is a
# phase that cannot recognise a state it is already sitting on.
#
# For each cube this reports what should all agree:
#
#   cube_is_reduced()        the package's own test, on the scrambled cube
#   phase 1/2/3 goal tests   each phase asked whether it is already there
#   the reduction            what cube_kociemba4_reduce() actually returns
#
# Run with:  Rscript inst/examples/diag_outer_only.R
#            Rscript inst/examples/diag_outer_only.R 40 8   # 40 cubes, 8 moves

library(cayleyR)

args     <- commandArgs(trailingOnly = TRUE)
n_states <- if (length(args) >= 1) as.integer(args[[1]]) else 25L
n_moves  <- if (length(args) >= 2) as.integer(args[[2]]) else 6L

N           <- 4L
node_budget <- 1e6

set.seed(2026)

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

# The outer alphabet: face turns and their inverses, no inner layers.
outer_moves <- grep("^[URFDLB]", cube_move_names(N), value = TRUE)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

yn <- function(x) if (isTRUE(x)) "yes" else "no"

hr("setup")
cat("cubes        : ", n_states, " scrambles of ", n_moves,
    " outer quarter turns\n", sep = "")
cat("alphabet     : ", paste(outer_moves, collapse = " "), "\n", sep = "")
cat("node budget  : ", format(node_budget, scientific = FALSE, big.mark = ","),
    " per phase\n", sep = "")
cat("\nOuter turns move no centre off its face and split no wing pair, so\n")
cat("every cube below is already reduced. The reduction should return an\n")
cat("empty word without searching at all.\n")

rows <- list()

for (i in seq_len(n_states)) {
  id <- cube_identity(N)
  repeat {
    word <- sample(outer_moves, n_moves, replace = TRUE)
    state <- replay(id, word)
    if (!identical(state, id)) break
  }

  # What ought to be true before any phase runs.
  already <- cube_is_reduced(state)
  g <- vapply(1:3, function(p) cayleyR:::cube_at_phase_goal_cpp(state, p),
              logical(1))

  red <- cube_kociemba4_reduce(state, node_budget = node_budget)
  rep <- cube_kociemba4_report()
  after <- replay(state, red)
  ok <- cube_is_reduced(after)

  stopped <- if (ok) "" else
    if (rep$phase1 != "found") "phase 1"
    else if (rep$phase2 != "found") "phase 2" else "phase 3"

  flag <- if (already && length(red) > 0) "  <- searched a cube already reduced"
          else if (!ok) "  <- FAILED" else ""

  cat(sprintf("\n  cube %2d  %s\n", i, paste(word, collapse = " ")))
  cat(sprintf("    reduced already: %-4s   phase goals: 1=%s 2=%s 3=%s\n",
              yn(already), yn(g[1]), yn(g[2]), yn(g[3])))
  cat(sprintf("    reduction: %d moves, %s%s\n", length(red),
              if (ok) "reduced" else paste("STOPPED at", stopped), flag))
  if (!ok || length(red) > 0) {
    cat(sprintf("    nodes: p1 %s, p2 %s, p3 %s\n",
                format(rep$phase1_nodes, scientific = FALSE, big.mark = ","),
                format(rep$phase2_nodes, scientific = FALSE, big.mark = ","),
                format(rep$phase3_nodes, scientific = FALSE, big.mark = ",")))
  }
  flush.console()

  rows[[i]] <- data.frame(
    cube = i, word = paste(word, collapse = " "),
    already_reduced = already,
    p1_goal = g[1], p2_goal = g[2], p3_goal = g[3],
    red_moves = length(red), reduced_after = ok, stopped_at = stopped,
    p1_nodes = rep$phase1_nodes, p2_nodes = rep$phase2_nodes,
    p3_nodes = rep$phase3_nodes,
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

hr("summary")
cat(sprintf("already reduced before any phase ran : %d of %d\n",
            sum(tab$already_reduced), nrow(tab)))
cat(sprintf("phase 3 goal test says 'already there': %d of %d\n",
            sum(tab$p3_goal), nrow(tab)))
cat(sprintf("reduction returned an empty word      : %d of %d\n",
            sum(tab$red_moves == 0), nrow(tab)))
cat(sprintf("reduced when the reduction finished   : %d of %d\n",
            sum(tab$reduced_after), nrow(tab)))

if (any(!tab$reduced_after)) {
  cat("\ndid not reduce:\n")
  print(tab[!tab$reduced_after,
            c("cube", "word", "already_reduced", "p3_goal", "stopped_at",
              "p3_nodes")],
        row.names = FALSE)
}

hr("verdict")

# The three things that can be wrong here, in the order they would be found.
if (any(!tab$already_reduced)) {
  cat("Some of these cubes are NOT reduced, which contradicts the premise:\n")
  cat("an outer turn cannot move a centre off its face or split a wing pair.\n")
  cat("Either cube_is_reduced() is wrong, or the outer moves in this\n")
  cat("package's alphabet are not what their names say. Check the moves\n")
  cat("first -- diag_phase_generators.R compares them piece by piece.\n")
} else if (any(tab$already_reduced & !tab$p3_goal)) {
  n <- sum(tab$already_reduced & !tab$p3_goal)
  cat(sprintf("%d cube(s) are reduced by the package's own test, and phase 3's\n", n))
  cat("goal test says they are not at its goal. Those two must agree --\n")
  cat("phase 3 exists to produce exactly the states cube_is_reduced() accepts.\n")
  cat("They disagree, so one of them is wrong, and that is the bug: phase 3\n")
  cat("is searching for a goal that is not the goal it is supposed to reach.\n")
  cat("\nRead Phase3Deriver4 in src/kociemba4.h against cube_is_reduced_cpp:\n")
  cat("the coordinate carries centres, canonicalised wings and a parity bit,\n")
  cat("and cube_is_reduced() checks centres and wing pairing. The parity bit\n")
  cat("is the part with no counterpart in the reduction test.\n")
} else if (any(tab$red_moves > 0)) {
  cat("The phases searched cubes that were already reduced, and found words\n")
  cat("for them. Harmless if they finish, but it means no phase checks\n")
  cat("whether it has already arrived before it starts searching.\n")
} else {
  cat("Outer-only scrambles are handled correctly: every cube was already\n")
  cat("reduced, every phase agreed, and the reduction returned nothing to do.\n")
  cat("The failure seen in diag_scramble_shape.R was the one cube in that\n")
  cat("sample, not a property of outer-only scrambles.\n")
}
