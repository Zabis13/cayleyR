#!/usr/bin/env Rscript
# Is the exact phase 3 coordinate affordable to build?
#
# diag_coord_candidates.R settled the design question: all seventeen of phase
# 3's generators map the twelve primary wing slots onto themselves, so where
# the twelve primary wings sit is a permutation of twelve slots -- 12! =
# 479,001,600 states. That is smaller than the 2^28 hash table the phase
# already allocates, which means the table can be complete: every state gets
# its own entry, nothing collides, and the bound is the real distance rather
# than whatever else landed in the slot.
#
# What is not settled is the price. A design that is exact and unaffordable is
# not a design, so this measures three things before anything in the package
# changes:
#
#   time            how long the full breadth-first search takes.
#
#   memory          fixed at the table, by construction. There is no frontier
#                   list: the states at depth k are found by scanning the table
#                   for the value k. A frontier-list BFS is where the memory
#                   goes on a space this size -- the peak can exceed the final
#                   table -- so this design does not keep one, and the cost is
#                   one pass over the table per level instead.
#
#   completeness    the counts per depth must sum to exactly 479,001,600. Not
#                   approximately: a sum that falls short means states are
#                   unreachable and the coordinate is wrong about the phase; a
#                   sum that overshoots is impossible and would mean the
#                   ranking is not injective. This is the check that catches a
#                   broken rank/unrank before it becomes a table full of
#                   confident wrong numbers.
#
# The ranking is checked separately first, because everything else is
# meaningless if it is wrong.
#
# Run with:  Rscript inst/examples/bench_coord12_bfs.R
#            Rscript inst/examples/bench_coord12_bfs.R 6    # stop at depth 6
#
# Needs about 500 MB of memory for the table.

library(cayleyR)
library(Rcpp)

args <- commandArgs(trailingOnly = TRUE)
max_depth <- if (length(args) >= 1L) as.integer(args[[1]]) else 20L

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

# Phase 3's generators, from phase3_gens4() in kociemba4.h.
gens <- list(
  "U" = "U", "U'" = "U'", "U2" = "U U",
  "D" = "D", "D'" = "D'", "D2" = "D D",
  "L" = "L", "L'" = "L'", "L2" = "L L",
  "R" = "R", "R'" = "R'", "R2" = "R R",
  "F2" = "F F", "B2" = "B B",
  "Uw2" = "U 2y U 2y", "Rw2" = "R 2x R 2x", "Fw2" = "F 2z F 2z")

geom <- cayleyR:::cube_wing_geometry_cpp()
wings_of <- function(state) {
  cayleyR:::cube_to_pieces4_cpp(state)$perm[9:32] - 8L
}

hr("the coordinate")

solved <- cube_identity(N)
base <- wings_of(solved)
prim_pieces <- which(geom$primary_in_dedge ==
                       (seq_along(geom$primary_in_dedge) - 1L)) - 1L
prim_slots <- which(base %in% prim_pieces)

cat("primary slots : ", paste(prim_slots, collapse = " "), "\n", sep = "")
cat("states        : ", format(factorial(12), big.mark = ",",
                               scientific = FALSE), "\n", sep = "")
cat("table         : ", round(factorial(12) / 2^20), " MB at one byte each\n",
    sep = "")

# Each generator, as a permutation of the twelve primary SLOT POSITIONS.
#
# Read off the cube rather than written down: apply the generator to a solved
# cube, see which primary slot each primary slot's wing came from. Writing
# these by hand is exactly the kind of step that has gone wrong in this project
# before -- Uw takes layer 2, not layer 1 -- so nothing here is stated from the
# name of a move.
slot_index <- integer(24)
slot_index[prim_slots] <- seq_along(prim_slots)

move_rows <- t(vapply(names(gens), function(nm) {
  s <- replay(solved, expand(strsplit(gens[[nm]], " +")[[1]]))
  w <- wings_of(s)
  # w[slot] is the piece now in `slot`. For each primary slot, that piece sits
  # on some primary slot of the solved cube -- the position it came from.
  vapply(prim_slots, function(sl) {
    piece <- w[[sl]]
    from <- which(base == piece)
    if (!length(from) || !(from[[1]] %in% prim_slots)) {
      stop("generator ", nm, " moved a wing off the primary slots -- ",
           "diag_coord_candidates.R says none of them do", call. = FALSE)
    }
    slot_index[[from[[1]]]] - 1L          # 0-based for C++
  }, integer(1))
}, integer(length(prim_slots))))

cat("generators    : ", nrow(move_rows), "\n", sep = "")

sourceCpp(file.path(dirname(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
          "bench_coord12_bfs.cpp"))

# ---- the ranking ----------------------------------------------------------

hr("rank and unrank")

chk <- coord12_check_rank(200000L)
cat("round trip on 200,000 ranks across the range: ",
    if (isTRUE(chk$ok)) "exact" else
      paste0("FAILED at ", format(chk$bad_at, scientific = FALSE)), "\n", sep = "")
if (!isTRUE(chk$ok)) {
  stop("rank/unrank are not inverses; nothing below would mean anything",
       call. = FALSE)
}

# ---- the goals ------------------------------------------------------------

# Phase 3's goals are the solved cube in each of the twenty-four orientations.
# In this coordinate several of them collapse to the same permutation, which is
# the point: the coordinate does not distinguish states the phase does not
# need to distinguish.

hr("the goals in this coordinate")

rot_x <- strsplit("L' 1x 2x R", " +")[[1]]
rot_y <- strsplit("D' 1y 2y U", " +")[[1]]
key <- function(s) paste(s, collapse = ",")
seen <- list(solved); keys <- key(solved); frontier <- list(solved)
repeat {
  nxt <- list()
  for (s in frontier) for (r in list(rot_x, rot_y)) {
    t <- s; for (m in r) t <- t[mv[[m]]]
    if (!(key(t) %in% keys)) {
      keys <- c(keys, key(t)); seen[[length(seen)+1L]] <- t
      nxt[[length(nxt)+1L]] <- t
    }
  }
  if (!length(nxt)) break
  frontier <- nxt
}

perm_of <- function(state) {
  w <- wings_of(state)
  vapply(prim_slots, function(sl) {
    piece <- w[[sl]]
    from <- which(base == piece)
    if (!length(from) || !(from[[1]] %in% prim_slots)) return(NA_integer_)
    slot_index[[from[[1]]]] - 1L
  }, integer(1))
}

goal_perms <- lapply(seen, perm_of)
usable <- !vapply(goal_perms, function(p) any(is.na(p)), logical(1))

cat("orientations              : ", length(seen), "\n", sep = "")
cat("expressible in this coord : ", sum(usable), "\n", sep = "")

# A goal whose primary wings sit off the primary slots cannot be named in this
# coordinate. That is worth knowing rather than silently dropping: it would
# mean the coordinate cannot see some of the states the phase is allowed to
# stop at, and the table built over it would be admissible but not tight.
if (any(!usable)) {
  cat("\n", sum(!usable), " of the orientations put primary wings on\n",
      "non-primary slots, so this coordinate cannot express them. The table\n",
      "will be built from the ones it can, which stays admissible -- it just\n",
      "cannot promise to notice every place the phase could legally stop.\n",
      sep = "")
}

goal_ranks <- unique(vapply(goal_perms[usable], function(p) {
  # rank_perm's own arithmetic, in R, so the goals are ranked by the same rule
  # the search uses rather than by a second implementation of it.
  n <- length(p)
  r <- 0
  # The last position has nothing after it, so the loop stops one short rather
  # than relying on a reversed range to come out empty.
  for (i in seq_len(n - 1L)) {
    c <- sum(p[(i + 1L):n] < p[[i]])
    r <- r + c * factorial(n - i)
  }
  r
}, numeric(1)))

cat("distinct goal states      : ", length(goal_ranks), "\n", sep = "")

# ---- the search -----------------------------------------------------------

hr("breadth-first over the whole coordinate")

cat("no frontier list: each level is found by scanning the table, so memory\n")
cat("is the table and nothing else. Expect one pass per level.\n\n")

t0 <- proc.time()[["elapsed"]]
res <- coord12_bfs(move_rows, as.integer(goal_ranks), max_depth = max_depth)
secs <- proc.time()[["elapsed"]] - t0

hr("the price")

cat(sprintf("time          : %.1f s\n", secs))
cat(sprintf("table         : %.0f MB (one byte per state)\n", res$table_mb))
cat(sprintf("packed        : %.0f MB (two distances per byte)\n",
            res$table_mb / 2))
cat(sprintf("states seen   : %s of %s\n",
            format(res$total, big.mark = ",", scientific = FALSE),
            format(res$n_states, big.mark = ",", scientific = FALSE)))

hr("completeness")

if (isTRUE(res$complete)) {
  cat("The depths sum to exactly 12!. Every permutation is reachable, the\n")
  cat("ranking is injective over the whole range, and the table is complete:\n")
  cat("every lookup returns that state's own distance. No collisions exist to\n")
  cat("measure, which is the whole difference from the present table.\n")
} else {
  short <- res$n_states - res$total
  cat(sprintf("The depths sum to %s, short of 12! by %s.\n",
              format(res$total, big.mark = ",", scientific = FALSE),
              format(short, big.mark = ",", scientific = FALSE)))
  cat("\nEither the generators do not reach every permutation of the primary\n")
  cat("slots -- in which case the coordinate is larger than the reachable set\n")
  cat("and should be indexed over that set instead -- or the search stopped\n")
  cat("early at the depth limit. Check the last level: a level that was still\n")
  cat("producing states when the limit hit is the second case.\n")
}

hr("states by depth")

cnt <- res$counts
for (d in seq_along(cnt)) {
  cat(sprintf("  %2d  %13s  %6.2f%%\n", d - 1L,
              format(cnt[[d]], big.mark = ",", scientific = FALSE),
              100 * cnt[[d]] / res$n_states))
}

hr("what this decides")

if (isTRUE(res$complete) && secs < 1800) {
  cat("Affordable and exact. The next step is (2): build this table in\n")
  cat("kociemba4.h alongside the present one, behind a flag, and measure the\n")
  cat("six failing seeds against it. The bound it returns is a real distance,\n")
  cat("so best_bound can no longer be 0 on a cube that is not solved.\n")
} else if (isTRUE(res$complete)) {
  cat(sprintf("Exact, but %.0f minutes to build. That is a one-off if the\n",
              secs / 60))
  cat("table is written to disk and loaded, and prohibitive if it is rebuilt\n")
  cat("per run -- so the next question is serialisation, not search.\n")
} else {
  cat("The coordinate does not close over 12! as searched here. Settle that\n")
  cat("before building anything: an incomplete table is a hash table with\n")
  cat("extra steps.\n")
}
