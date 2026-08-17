#!/usr/bin/env Rscript
# Sizing the candidates for an exact phase 3 coordinate.
#
# The hash table is finished as an approach, and the numbers saying so are in
# TODO.md: about 1e11 coordinate states against 2^28 = 2.7e8 slots, waste 0.975,
# best_bound pinned at 0 on every cube that fails. Depth and width were both
# measured and both are the same overflow seen from different ends.
#
# What replaces it is an EXACT coordinate: a quantity small enough to index
# directly, so the table is complete and cannot collide by construction. That
# is what twist/flip/slice are in the original Kociemba -- not hashes of the
# cube, but numbers with a known range that address a full table.
#
# This script does not design that coordinate. It measures the candidates, so
# the design starts from sizes rather than from hope. For each one it reports:
#
#   size          how many values the coordinate can take. Under about 5e8 is
#                 a table that fits in memory as one byte per state; above
#                 that it is another hash by another name.
#
#   closed        whether phase 3's own generators keep the coordinate
#                 well-defined -- if a move can take two states with the same
#                 coordinate to states with different ones, the coordinate is
#                 not a function of the phase's moves and no table over it is
#                 admissible.
#
#   monotone      whether it reaches its goal value only on a paired cube.
#
# The candidates come from the geometry already in kociemba4.h:
#
#   dedges paired         0..12, thirteen values. Exact and trivially small,
#                         but thirteen values over 1e11 states is a lower
#                         bound of about (12-k)/2 moves -- far too weak to cut
#                         a tree branching 12.5. Measured here for what it is:
#                         a gradient, not a bound.
#
#   primary permutation   wing_position_is_primary() picks twelve slots, one
#                         per dedge, closed under <U,L,R,D>. Where the twelve
#                         primary wings sit among those slots is a permutation
#                         of twelve: 12! = 479,001,600. That is a real table --
#                         479 MB at a byte each, 240 MB packed to a nibble --
#                         and it is exact.
#
#   primary combination   which twelve of the twenty-four slots hold primary
#                         wings, ignoring their order: C(24,12) = 2,704,156.
#                         Tiny, and a coarser view of the same fact.
#
# Run with:  Rscript inst/examples/diag_coord_candidates.R

library(cayleyR)

N <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

expand <- function(w) {
  unlist(lapply(w, function(t)
    if (nchar(t) > 1 && substr(t, nchar(t), nchar(t)) == "2")
      rep(substr(t, 1, nchar(t) - 1), 2) else t))
}

geom <- cayleyR:::cube_wing_geometry_cpp()

# Phase 3's generators, from phase3_gens4() in kociemba4.h. The three wide half
# turns are words; the rest are single moves.
gens <- list(
  "U" = "U", "U'" = "U'", "U2" = "U U",
  "D" = "D", "D'" = "D'", "D2" = "D D",
  "L" = "L", "L'" = "L'", "L2" = "L L",
  "R" = "R", "R'" = "R'", "R2" = "R R",
  "F2" = "F F", "B2" = "B B",
  "Uw2" = "U 2y U 2y", "Rw2" = "R 2x R 2x", "Fw2" = "F 2z F 2z")

wings_of <- function(state) {
  cayleyR:::cube_to_pieces4_cpp(state)$perm[9:32] - 8L
}

hr("the geometry")

primary <- which(geom$primary_in_dedge == (seq_along(geom$primary_in_dedge) - 1L))
cat("wings                : ", length(geom$partner), "\n", sep = "")
cat("dedges               : ", length(unique(geom$dedge)), "\n", sep = "")
cat("primary wing pieces  : ", length(primary), "\n", sep = "")

# `primary_in_dedge` names, for each wing, the half of its pair that was chosen
# as primary. A wing is itself primary when it is its own representative.
cat("primary pieces are   : ", paste(primary - 1L, collapse = " "), "\n", sep = "")

hr("candidate sizes")

sizes <- data.frame(
  coordinate = c("dedges paired (0..12)",
                 "primary slots, unordered  C(24,12)",
                 "primary permutation  12!",
                 "primary permutation x parity",
                 "current: hashed 24 wings + centres"),
  size = c(13, choose(24, 12), factorial(12), 2 * factorial(12), 1e11),
  stringsAsFactors = FALSE)
sizes$megabytes <- ifelse(sizes$size > 1e10, NA, sizes$size / 2^20)
sizes$packed_mb <- ifelse(sizes$size > 1e10, NA, sizes$size / 2 / 2^20)
print(sizes, row.names = FALSE, digits = 12)

cat("\nA byte per state is the plain table; packed is two distances to a byte,\n")
cat("which is what a table holding values 0..14 actually needs.\n")

# ---- is the primary set closed under the phase's moves? -------------------

# The question that decides whether a coordinate over primary wings is even
# well defined. wing_position_is_primary() builds the set from <U,L,R,D>, but
# phase 3 also has F2, B2 and the three wide half turns. If any of those moves
# a primary wing onto a slot outside the set, then "where the primary wings
# sit" is not a permutation of twelve slots and the count above is wrong.

hr("do phase 3's generators preserve the primary slots?")

solved <- cube_identity(N)
base <- wings_of(solved)
prim_pieces <- primary - 1L

# The slots holding primary pieces on a solved cube: the reference set.
prim_slots <- which(base %in% prim_pieces)
cat("primary slots on a solved cube: ", paste(prim_slots, collapse = " "),
    "\n\n", sep = "")

bad <- character(0)
for (nm in names(gens)) {
  s <- replay(solved, expand(strsplit(gens[[nm]], " +")[[1]]))
  w <- wings_of(s)
  now <- which(w %in% prim_pieces)
  ok <- identical(sort(now), sort(prim_slots))
  cat(sprintf("  %-5s %s\n", nm, if (ok) "primary slots preserved"
                                 else "MOVES PRIMARY WINGS OFF THE SET"))
  if (!ok) bad <- c(bad, nm)
}

cat("\n")
if (!length(bad)) {
  cat("Every generator maps the primary slots onto themselves. So where the\n")
  cat("twelve primary wings sit IS a permutation of twelve slots, 12! = 479\n")
  cat("million states, and a full table over it is exact -- no hash, no\n")
  cat("collisions, and an admissible bound by construction.\n")
} else {
  cat("These generators take primary wings off the primary slots:\n  ",
      paste(bad, collapse = " "), "\n", sep = "")
  cat("\nSo the twelve-slot picture is not preserved by the phase's own moves,\n")
  cat("and a coordinate built on it would be reading a quantity the search can\n")
  cat("step outside of. The next candidate down is the unordered version --\n")
  cat("which slots hold primary wings, C(24,12) -- which survives any move\n")
  cat("that keeps the SET whole even when it permutes it.\n")
}

# ---- how informative is the pairing count? --------------------------------

# The cheap candidate, measured rather than assumed. If the number of paired
# dedges tracks distance at all, it is worth having as a gradient even though
# it is far too coarse to be a bound.

hr("does the pairing count track distance?")

cat("Walking out from a solved cube: how many dedges stay paired after k\n")
cat("random generator moves. A count that falls steadily is a gradient worth\n")
cat("following; one that collapses at once carries no information.\n\n")

n_paired_from <- function(w) {
  paired <- 0L
  seen <- logical(24)
  for (slot in seq_len(24)) {
    if (seen[[slot]]) next
    piece <- w[[slot]]
    mate <- geom$partner[[piece + 1L]]
    ms <- which(w == mate)
    if (!length(ms)) next
    seen[[slot]] <- TRUE
    seen[[ms[[1]]]] <- TRUE
    if (geom$dedge[[slot]] == geom$dedge[[ms[[1]]]]) paired <- paired + 1L
  }
  paired
}

set.seed(1)
cat(sprintf("  %-6s %-14s %s\n", "moves", "mean paired", "range"))
for (k in c(1, 2, 3, 4, 6, 8, 12)) {
  vals <- vapply(seq_len(40), function(i) {
    w <- sample(names(gens), k, replace = TRUE)
    s <- replay(solved, expand(unlist(strsplit(unlist(gens[w]), " +"))))
    n_paired_from(wings_of(s))
  }, integer(1))
  cat(sprintf("  %-6d %-14.2f %d..%d\n", k, mean(vals), min(vals), max(vals)))
}

cat("\nRead this against the failures: on the cubes phase 3 cannot solve the\n")
cat("count sits at 0..8 and never moves during the search. If the table above\n")
cat("shows the count falling smoothly with distance, then a search ordered by\n")
cat("it would at least be walking downhill, which the present one is not.\n")
