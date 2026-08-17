#!/usr/bin/env Rscript
# Where the edge-pairing stage stalls, and what the position looks like there.
#
# cube_edges.h records that this stage was measured on fifteen positions and
# reached all twelve pairs on thirteen of them, the two failures leaving "a
# cycle running through four or five half-edges -- the same reason, that a
# greedy step cannot see a move which pays only on the next one". That
# measurement is not in the repository, so this re-runs it: the comment says
# what shape the failures have, and a shape is worth checking rather than
# quoting.
#
# The stage is the human method -- setup with outer turns, pair with one of six
# algorithms, put the pair away, restore the centres -- and the algorithms in
# edge_algs() are the ones speedcubers use, including the last-pair case
# Dw R F' U R' F Dw'. What it does not have is a fixed ORDER. It takes whatever
# algorithm pairs the most edges this round, which is where a greedy method can
# walk into a position none of its algorithms can improve.
#
# So the questions, in order:
#
#   how often     out of N random cubes, how many stall
#   where         how many pairs were made before it gave up
#   what shape    the cycle structure of the unpaired wings at the stall
#
# The third is the one that decides the repair. If the stalls all show the same
# cycle shape, one more algorithm aimed at that shape fixes them and nothing
# else has to change. If they show different shapes each time, the greedy
# choice itself is the problem and the repair is an order -- pair the edges in
# a fixed sequence, as Yau does, rather than by whichever pays most now.
#
# Run with:  Rscript inst/examples/diag_pair_edges_stall.R
#            Rscript inst/examples/diag_pair_edges_stall.R 60   # cubes to try

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)
n_cubes <- if (length(args) >= 1L) as.integer(args[[1]]) else 30L

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

geom <- cube_wing_geometry_cpp()

n_paired <- function(state) {
  w <- cube_to_pieces4_cpp(state)$perm[9:32] - 8L
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

stopifnot(n_paired(cube_identity(N)) == 12L)

# The shape of what is left unpaired.
#
# Read as a permutation on dedges: for each unpaired wing, the dedge it sits in
# and the dedge its piece belongs to. The cycle lengths of that permutation are
# the "cycle running through four or five half-edges" the comment describes,
# and they are what a repair has to be aimed at.
unpaired_shape <- function(state) {
  w <- cube_to_pieces4_cpp(state)$perm[9:32] - 8L

  # Which slots are not correctly paired.
  bad <- integer(0)
  seen <- logical(24)
  for (slot in seq_len(24)) {
    if (seen[[slot]]) next
    piece <- w[[slot]]
    mate <- geom$partner[[piece + 1L]]
    ms <- which(w == mate)
    if (!length(ms)) { bad <- c(bad, slot); next }
    seen[[slot]] <- TRUE
    seen[[ms[[1]]]] <- TRUE
    if (geom$dedge[[slot]] != geom$dedge[[ms[[1]]]]) {
      bad <- c(bad, slot, ms[[1]])
    }
  }
  if (!length(bad)) return(list(n_bad = 0L, cycles = integer(0)))

  # dedge of the slot -> dedge the piece in it belongs to, over the bad slots.
  from <- geom$dedge[bad]
  to <- geom$dedge[w[bad] + 1L]

  # Cycle lengths of that map, walked over the dedges involved.
  nodes <- unique(from)
  nxt <- setNames(to[match(nodes, from)], as.character(nodes))
  cycles <- integer(0)
  left <- nodes
  while (length(left)) {
    start <- left[[1]]
    len <- 0L
    cur <- start
    repeat {
      len <- len + 1L
      left <- setdiff(left, cur)
      nx <- nxt[[as.character(cur)]]
      if (is.null(nx) || is.na(nx) || nx == start || !(nx %in% nodes)) break
      cur <- nx
      if (!(cur %in% left)) break
    }
    cycles <- c(cycles, len)
  }
  list(n_bad = length(bad), cycles = sort(cycles, decreasing = TRUE))
}

# ---- the run --------------------------------------------------------------

hr("setup")
cat("cubes : ", n_cubes, " random states\n", sep = "")
cat("stage : cube_reduce_cpp -- centres, then the human edge-pairing method\n")
cat("\nA stall is `failure` reporting the edges, which is the stage giving up\n")
cat("after a round that could not improve on the one before.\n\n")

cat(sprintf("%5s %-9s %6s %6s %s\n",
            "cube", "result", "pairs", "moves", "unpaired cycle shape"))
flush.console()

rows <- list()
for (i in seq_len(n_cubes)) {
  set.seed(1000L + i)
  w <- sample(cube_move_names(N), 20L, replace = TRUE)
  s <- replay(cube_identity(N), w)

  red <- cube_reduce_cpp(s)
  end <- if (length(red$states)) red$states[[length(red$states)]] else s

  pairs <- n_paired(end)
  shape <- unpaired_shape(end)
  ok <- isTRUE(red$found)

  cat(sprintf("%5d %-9s %6d %6d %s\n", i,
              if (ok) "reduced" else "STALL", pairs, length(red$path),
              if (!length(shape$cycles)) "-"
              else paste(shape$cycles, collapse = "+")))
  flush.console()

  rows[[length(rows) + 1L]] <- data.frame(
    cube = i, reduced = ok, pairs = pairs, moves = length(red$path),
    n_bad = shape$n_bad,
    shape = if (!length(shape$cycles)) "" else paste(shape$cycles, collapse = "+"),
    failure = if (is.null(red$failure)) "" else red$failure,
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

hr("how often")

cat(sprintf("reduced : %d of %d\n", sum(tab$reduced), nrow(tab)))
cat(sprintf("stalled : %d of %d\n", sum(!tab$reduced), nrow(tab)))

bad <- tab[!tab$reduced, ]
if (!nrow(bad)) {
  cat("\nNo stall in this sample. The comment in cube_edges.h reports two in\n")
  cat("fifteen, so either the sample is too small or the scrambles here are\n")
  cat("easier than the ones it was measured on.\n")
  quit(save = "no")
}

hr("where it stopped")

cat("pairs made before giving up:\n")
print(table(bad$pairs))

cat("\nreasons given:\n")
print(table(bad$failure))

hr("what shape is left")

cat("Cycle lengths of the unpaired wings, read as a permutation on dedges.\n\n")
print(table(bad$shape))

shapes <- unique(bad$shape)
cat("\n")
if (length(shapes) == 1L) {
  cat("Every stall has the same shape: ", shapes, ".\n\n", sep = "")
  cat("One algorithm aimed at this shape would clear all of them, and nothing\n")
  cat("else about the stage has to change. That is the cheap repair: add it to\n")
  cat("edge_algs() in src/cube_edges.h and the greedy step will find it.\n")
} else {
  cat("The stalls do not share a shape: ", paste(shapes, collapse = ", "),
      ".\n\n", sep = "")
  cat("Then no single algorithm covers them, and the greedy choice is the\n")
  cat("problem rather than the algorithm set. The repair is an order -- pair\n")
  cat("the dedges in a fixed sequence the way Yau does, so the stage never\n")
  cat("arrives at a position its algorithms cannot improve, instead of\n")
  cat("choosing whatever pays most this round and hoping.\n")
}
