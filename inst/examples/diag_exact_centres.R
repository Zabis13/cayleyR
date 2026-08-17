#!/usr/bin/env Rscript
# Phase 3 with the exact centre table, against phase 3 without it.
#
# The table is complete over the centres: 58,800 arrangements, every one with
# its own entry, no hash and no collisions (diag_centre_coord.R measured the
# closure; src/centre_table.h builds it). It is combined with the existing hash
# table by taking the maximum of the two bounds, never the sum -- each is a
# lower bound on the moves remaining, and the larger of two lower bounds is
# still one, while their sum is not.
#
# What to expect, stated before the run so the result cannot be read to taste:
#
#   The centre bound does NOT bound the pairing. On these cubes phases 1 and 2
#   have already put the centres on their axes, so the centre distance is small
#   and the bound it contributes is small with it. What it can do is stop the
#   search walking into branches that take the centres apart -- on seed 8 the
#   closest branch swung the centres 10 -> 14 -> 8 -> 14 while pairing nothing.
#
#   So the honest prediction is: fewer wasted branches, possibly fewer nodes,
#   and quite possibly the same six failures. Pairing is bounded by the wing
#   coordinate (12! = 479,001,600, verified by bench_coord12_bfs.R), which is
#   not built into the solver yet because it costs 324 s and 228 MB.
#
# A result where nothing changes at all is worth having: it would say the
# centre half was never what held the search back, and put the whole weight on
# the wings.
#
# Run with:  Rscript inst/examples/diag_exact_centres.R
#            Rscript inst/examples/diag_exact_centres.R 4,8,12   # pick seeds

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)

N <- 4L
BUDGET <- 1e6

seeds <- if (length(args) >= 1L && nzchar(args[[1]])) {
  as.integer(strsplit(args[[1]], "[, ]+")[[1]])
} else {
  c(4L, 8L, 12L, 20L, 23L, 24L)   # the ones diag_phase3_seeds.R found failing
}

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

.wing_geom <- cayleyR:::cube_wing_geometry_cpp()
n_paired <- function(state) {
  perm <- cayleyR:::cube_to_pieces4_cpp(state)$perm
  w <- perm[9:32] - 8L
  paired <- 0L
  seen <- logical(24)
  for (slot in seq_len(24)) {
    if (seen[[slot]]) next
    piece <- w[[slot]]
    mate_piece <- .wing_geom$partner[[piece + 1L]]
    mate_slot <- which(w == mate_piece)
    if (!length(mate_slot)) next
    seen[[slot]] <- TRUE
    seen[[mate_slot[[1]]]] <- TRUE
    if (.wing_geom$dedge[[slot]] == .wing_geom$dedge[[mate_slot[[1]]]]) {
      paired <- paired + 1L
    }
  }
  paired
}

local({
  chk <- n_paired(cube_identity(N))
  if (chk != 12L) {
    stop("n_paired() reports ", chk, " on a solved cube, expected 12",
         call. = FALSE)
  }
})

handed_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = BUDGET)
  replay(s, p12)
}

# ---- is the table what the measurement said it is? ------------------------

hr("the centre table")

probe <- cayleyR:::cube_kociemba4_phase3_cpp(cube_identity(N),
                                             node_budget = 1000,
                                             use_exact_centres = TRUE)

cat("arrangements : ", format(probe$centre_states, big.mark = ","), "\n",
    sep = "")
cat("depth        : ", probe$centre_depth, "\n", sep = "")

# 352,800 = 58,800 x 6, and the six is not a mistake in either measurement.
#
# diag_centre_coord.R closed the centres from ONE solved cube and found 58,800
# arrangements. The solver builds from all twenty-four goals -- the solved cube
# in each orientation -- and those carry six distinct centre arrangements
# between them, since twenty-four rotations move the six centre colours through
# six positions with four rotations agreeing on each. So the solver's table is
# six copies of the same structure, one per colour placement, and it is the
# correct one to hold: phase 3 is allowed to finish in any orientation, so the
# distance that matters is to the NEAREST goal, not to one chosen goal.
#
# The depth falls from 11 to 9 for the same reason. More goals, so nothing is
# as far from all of them as it was from one.
# 352,800 is the closure from all twenty-four goals at once, measured
# independently in R (scratch run, 14.08.2026: 24, 96, 912, 6,624, 30,912,
# 94,440, 194,160, ... converging on 352,800 by depth 9).
#
# It is NOT 24 x 58,800. The closures from the individual goals overlap
# heavily -- from one goal's centre arrangement the others are reachable -- so
# the union is six times one goal's closure rather than twenty-four times it.
# Two earlier guesses at this number were wrong for the same reason: both were
# arithmetic reasoned forward from a ratio instead of a closure walked and
# counted. The figure below is the walked one.
expected <- 352800
if (probe$centre_states != expected) {
  cat("\nExpected ", format(expected, big.mark = ","),
      ", the closure from all 24 goals as walked in R.\n", sep = "")
  cat("A different number means the solver's table is not that closure, and\n")
  cat("nothing below can be read until they agree.\n")
  quit(save = "no", status = 1)
}
cat("\nagreed with the closure from all 24 goals (352,800 at depth 9).\n")

# ---- the comparison -------------------------------------------------------

hr("with and without the exact centre bound")

cat(sprintf("%5s  %-11s %-11s %7s %7s %6s %6s %5s %5s\n",
            "seed", "off", "on", "nodes0", "nodes1", "best0", "best1",
            "pair0", "pair1"))
flush.console()

rows <- list()
for (sd in seeds) {
  s2 <- handed_state(sd)
  before <- n_paired(s2)

  a <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = BUDGET,
                                           use_exact_centres = FALSE)
  b <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = BUDGET,
                                           use_exact_centres = TRUE)

  pa <- n_paired(replay(s2, a$path))
  pb <- n_paired(replay(s2, b$path))

  cat(sprintf("%5d  %-11s %-11s %7s %7s %6d %6d %5d %5d\n",
              sd, a$outcome, b$outcome,
              format(a$nodes, big.mark = ",", scientific = FALSE),
              format(b$nodes, big.mark = ",", scientific = FALSE),
              a$best_bound, b$best_bound, pa, pb))
  flush.console()

  rows[[length(rows) + 1L]] <- data.frame(
    seed = sd, off = a$outcome, on = b$outcome,
    nodes_off = a$nodes, nodes_on = b$nodes,
    best_off = a$best_bound, best_on = b$best_bound,
    paired_before = before, paired_off = pa, paired_on = pb,
    solved_off = isTRUE(a$found), solved_on = isTRUE(b$found),
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

hr("what changed")

cat(sprintf("solved without the centre table : %d of %d\n",
            sum(tab$solved_off), nrow(tab)))
cat(sprintf("solved with it                  : %d of %d\n",
            sum(tab$solved_on), nrow(tab)))

fin <- tab[!tab$solved_off & !tab$solved_on, ]
if (nrow(fin)) {
  cat(sprintf("\nstill failing: %s\n", paste(fin$seed, collapse = ", ")))
  cat(sprintf("  best bound   %.2f -> %.2f\n",
              mean(fin$best_off), mean(fin$best_on)))
  cat(sprintf("  bounds at 0  %d -> %d  (of %d)\n",
              sum(fin$best_off == 0L), sum(fin$best_on == 0L), nrow(fin)))
}

hr("reading this")

if (sum(tab$solved_on) > sum(tab$solved_off)) {
  cat("The centre bound solved cubes that were failing. That is more than was\n")
  cat("predicted -- the expectation was fewer wasted branches rather than\n")
  cat("solutions -- so check the paired columns: a cube reported solved must\n")
  cat("end with 12 paired.\n")
} else if (all(tab$nodes_on < tab$nodes_off)) {
  cat("No cube changed outcome, but every one cost fewer nodes. The centre\n")
  cat("bound is pruning real branches; it is simply not the constraint that\n")
  cat("decides these cubes. The wing coordinate is.\n")
} else {
  cat("Neither outcomes nor node counts moved much. On these cubes phases 1\n")
  cat("and 2 have already placed the centres, so the centre distance is near\n")
  cat("zero and the bound it contributes is too small to cut anything the\n")
  cat("hash table was not already cutting.\n\n")
  cat("That is a clean negative, and it puts the whole weight on the wing\n")
  cat("coordinate: 12! = 479,001,600, exact, diameter 10, already built and\n")
  cat("verified by bench_coord12_bfs.R. What it needs is 228 MB of table and\n")
  cat("serialisation, not another idea.\n")
}
