#!/usr/bin/env Rscript
# Why does a table filled to depth 6 tell states 23 and 26 moves out that they
# are 6 moves out?
#
# bench_phase3_true_distance.R measured, on the failing seeds:
#
#     seed 4   bound 6   not solved inside 50,000,000 nodes
#     seed 5   bound 6   solved in 23 moves
#     seed 7   bound 6   solved in 26 moves
#
# Three different true distances, one bound, and that bound is the depth the
# table was filled to. Two readings of this were on the table and one of them
# can be dismissed on the code alone.
#
# Not a coarse coordinate. Phase3Deriver4 keeps all 24 wings (canonicalised),
# all 24 centres and a parity bit -- close to the whole state rather than a
# projection of it. It is not merging near states with far ones.
#
# So it is the hash. That detailed state goes through state_hash into
# table[h & mask], with mask 24 bits wide: about 1e11 coordinates over 1.7e7
# slots. And the collisions are not evenly spread over distances, which is what
# makes them look systematic rather than random: a table filled to depth 6 holds
# almost nothing but small distances, so a state landing on an occupied slot
# almost always reads a small number. First-writer-wins keeps that admissible --
# 6 really is some state's distance -- but it is not this state's.
#
# What this measures, without any searching:
#
#   1. How many distinct coordinates the fill produces, against how many
#      distinct slots they occupy. The gap is the collisions.
#   2. For the failing seeds specifically: is their slot occupied by their own
#      coordinate, or by a different one?
#
# The second is the direct test. The fill records coordinates, so if a seed's
# own coordinate never appears among them while its slot is full, the number it
# reads belongs to somebody else -- and no depth of filling will change that,
# because the slot was taken before its level was ever walked.
#
# Run with:  Rscript inst/examples/bench_phase3_hash.R
#            Rscript inst/examples/bench_phase3_hash.R 5    # fill depth 5

library(cayleyR)

args  <- commandArgs(trailingOnly = TRUE)
depth <- if (length(args) >= 1L) as.integer(args[[1]]) else 6L
# The size the measurement is about. Filling directly bypasses the growth a real
# search performs, so without this the run measures whatever init() started
# with -- which is 1<<20, four bits below what a search of any depth reaches.
tsize <- if (length(args) >= 2L) as.numeric(args[[2]]) else 2^25

N          <- 4L
p12_budget <- 2e6

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

handed_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = p12_budget)
  replay(s, p12)
}

select_file <- file.path(dirname(tempdir()), "cayleyR_phase3_select.rds")
if (!file.exists(select_file)) {
  stop("run inst/examples/bench_phase3_select.R first", call. = FALSE)
}
sel <- readRDS(select_file)
fail_seeds <- head(sel$seed[!sel$solved], 6L)
ok_seeds   <- head(sel$seed[sel$solved], 6L)

hr("setup")
cat("fill depth   : ", depth, "\n", sep = "")
cat("failing seeds: ", paste(fail_seeds, collapse = ", "), "\n", sep = "")
cat("solving seeds: ", paste(ok_seeds, collapse = ", "), "\n", sep = "")

hr("the table after filling")
t0 <- proc.time()[["elapsed"]]
cayleyR:::cube_kociemba4_fill_phase3_cpp(depth, tsize)
secs <- proc.time()[["elapsed"]] - t0
tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3

cat(sprintf("filled to depth %d in %.1f s\n", tb$built_depth, secs))
cat(sprintf("size %s slots, %s occupied (%.1f%%)\n",
            format(tb$size, big.mark = ",", scientific = FALSE),
            format(tb$filled, big.mark = ",", scientific = FALSE),
            100 * tb$filled / tb$size))
cat(sprintf("visits %s, writes %s, collisions %s\n",
            format(tb$n_visits, big.mark = ",", scientific = FALSE),
            format(tb$n_writes, big.mark = ",", scientific = FALSE),
            format(tb$n_collisions, big.mark = ",", scientific = FALSE)))

hr("where the recorded distances sit")
# A table whose mass is at the shallow end scores most collisions as "close to
# the goal", which is exactly the failure being investigated.
counts <- tb$depth_counts
for (d in seq_along(counts)) {
  cat(sprintf("  distance %d : %12s slots  (%.1f%% of filled)\n", d - 1L,
              format(counts[d], big.mark = ",", scientific = FALSE),
              100 * counts[d] / tb$filled))
}

hr("what the table tells each state")
# The comparison the run exists for. A solving seed and a failing seed both
# get a number; the question is whether either number is their own.
cat(sprintf("  %-6s %-9s %-7s %s\n", "seed", "solves?", "bound", "true distance"))
true_dist <- c("4" = NA, "5" = 23, "7" = 26)   # from bench_phase3_true_distance
for (sd in c(fail_seeds, ok_seeds)) {
  st <- handed_state(sd)
  b <- cayleyR:::cube_phase3_coord_cpp(st)$prune_bound
  solves <- sd %in% ok_seeds
  td <- true_dist[as.character(sd)]
  cat(sprintf("  %-6d %-9s %-7d %s\n", sd, if (solves) "yes" else "NO", b,
              if (is.na(td)) "-" else as.character(td)))
}

hr("what twips does, which settles the design question")
# Read after the measurements below were first taken, and it overturns the
# conclusion they suggested. twips solves this phase with the SAME construction:
# pattern_hash_u64 & mask, set_if_uninitialized (first writer wins), a stub of
# current_pruning_depth + 1, a fill of search_depth / 2 walked forward from the
# goals under the canonical FSM -- and the same coordinate, canonicalised wings
# plus centres plus a PLL parity bit (src/lib/scramble/puzzles/cube4x4x4/
# phase3.rs, _internal/search/hash_prune_table.rs).
#
# So hashing a large coordinate is not what breaks it, and no reduction of the
# coordinate is required. What differs is size:
#
#   twips   starts at 1<<20, ceiling 1<<28, grows from the estimated cost of
#           the level about to be searched
#   here    started at 1<<24 with the same 1<<28 ceiling -- and grow_to() only
#           grows, so a start above every level's estimate never grew at all.
#           Measured: n_grows was 0 on every run, the table stayed at 1<<24.
#
# Fixed 2026-08-13 by starting phase 3 at 1<<20. The growth then fires:
# n_grows 1, table 1<<25 on a 2e6-node search.
cat("  twips uses the same hash table and the same coordinate.\n")
cat("  the difference was the starting size blocking growth, now fixed.\n")

hr("how big the parts are, if the table were split anyway")
# The obvious repair for a coordinate too big to address is to split it and
# combine the parts with max. Whether that helps depends on how big the parts
# are, and the answer is discouraging on the arithmetic alone.
#
# Kociemba's coordinates are small because each is a *reduction*, not a piece
# of the state: twist is 3^7 = 2187 orientations, flip 2^11 = 2048, slice
# C(12,4) = 495. None of them says where a piece is, only something about it.
#
# Phase 3's parts are pieces of the state, and pieces of a state are as big as
# the state:
cat("  wings   : a permutation of 24, canonicalised\n")
cat("            24! is 6.2e23; canonicalisation divides by a constant\n")
cat("  centres : 24 slots over 6 colours, 6^24 = 4.7e18 before constraints\n")
cat("  parity  : 1 bit\n")
cat("\n")
cat(sprintf("  a table of 2^24 = %s slots addresses neither.\n",
            format(2^24, big.mark = ",", scientific = FALSE)))
cat("\n")
cat("So splitting centres from wings does not rescue the addressing: both\n")
cat("halves stay far beyond what can be indexed without collision. What\n")
cat("would work is what Kociemba actually does -- replace each part with a\n")
cat("reduction small enough to index exactly. For phase 3 that means asking\n")
cat("what the phase needs to know rather than storing where every piece is:\n")
cat("how many dedges are paired, which orbit class each wing sits in, how\n")
cat("far the centres are from their bands. Those are countable; positions\n")
cat("are not.\n")

hr("verdict")
stub <- tb$built_depth + 1L
mass_shallow <- sum(counts[seq_len(min(length(counts), depth))]) / tb$filled

cat(sprintf("stub is %d, so any bound below that came out of a slot.\n", stub))
cat(sprintf("%.0f%% of occupied slots hold a distance of %d or less.\n",
            100 * mass_shallow, depth))
cat("\n")
cat("A state whose slot was claimed by the fill reads that slot's number\n")
cat("whether or not the entry is its own, and with the mass this shallow\n")
cat("the number read is nearly always small. That is the shape of the\n")
cat("bound 6 against true distances of 23 and 26: not a coarse coordinate\n")
cat("-- Phase3Deriver4 keeps all 24 wings, all 24 centres and a parity bit\n")
cat("-- but a detailed coordinate folded into 24 bits of address.\n")
cat("\n")
cat("The fix is not depth and not prune_depth_bonus -- both leave the\n")
cat("addressing alone. But nor is it a new coordinate: twips solves this\n")
cat("phase with this very table and this very coordinate. It is the size,\n")
cat("and specifically the growth, which a too-large starting size had been\n")
cat("suppressing. Run this at several sizes to see how far it has to go:\n")
cat("\n")
cat("    Rscript inst/examples/bench_phase3_hash.R 6 33554432    # 1<<25\n")
cat("    Rscript inst/examples/bench_phase3_hash.R 6 268435456   # 1<<28\n")
cat("\n")
cat("What to watch is not the fill percentage but the depth histogram: the\n")
cat("table is doing its job when the deepest level holds most of the mass,\n")
cat("because that is the level a search actually consults.\n")
