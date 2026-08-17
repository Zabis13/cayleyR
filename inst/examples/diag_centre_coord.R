#!/usr/bin/env Rscript
# What do phase 3's generators actually do to the centres?
#
# The wing half of the heuristic is settled: 12! = 479,001,600, exact, complete,
# diameter 10, built in 324 s (bench_coord12_bfs.R). The remaining question is
# the other half. Phase 3 has to pair the wings WITHOUT taking the centres
# apart, so a heuristic that only knows about wings will happily walk into
# states that finish the pairing and ruin the centres.
#
# The obvious coordinate for the centres does not fit. Twenty-four centre
# positions in six colours, four of each, is
#
#     24! / (4!)^6 = 3,246,670,537,110
#
# -- 3.2e12, six thousand times the wing coordinate and far past any table. So
# the question is not "how do we index all centre arrangements" but "which
# centre arrangements can phase 3 even reach", and that is a question about its
# generators rather than about the cube.
#
# Fourteen of the seventeen generators are outer-face turns: U, D, L, R and
# their inverses and doubles, plus F2 and B2. An outer turn moves face centres
# around within their own face and does not move a centre piece to another
# face. If that holds, those fourteen leave the CENTRE COLOUR OF EVERY FACE
# unchanged, and the only generators that can disturb the centres are the three
# wide half turns Uw2, Rw2, Fw2.
#
# That would make the centre part of the coordinate very small: not which of
# the 24 centre positions holds which colour, but only what the three wide
# turns have done -- a state reachable in a handful of ways rather than 3.2e12.
#
# This measures it instead of assuming it:
#
#   per generator   how many of the 24 centre positions change colour, and
#                   whether any centre leaves its face.
#
#   closure         starting from a solved cube, close under the generators
#                   and count how many distinct centre arrangements appear.
#                   That number, not 3.2e12, is the size of the table phase 3
#                   needs -- and if it is small the centre half is nearly free.
#
# Run with:  Rscript inst/examples/diag_centre_coord.R
#            Rscript inst/examples/diag_centre_coord.R 500000   # closure cap

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)
cap <- if (length(args) >= 1L) as.numeric(args[[1]]) else 2e6

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

gens <- list(
  "U" = "U", "U'" = "U'", "U2" = "U U",
  "D" = "D", "D'" = "D'", "D2" = "D D",
  "L" = "L", "L'" = "L'", "L2" = "L L",
  "R" = "R", "R'" = "R'", "R2" = "R R",
  "F2" = "F F", "B2" = "B B",
  "Uw2" = "U 2y U 2y", "Rw2" = "R 2x R 2x", "Fw2" = "F 2z F 2z")

# The centres as phase 3 sees them: the colour sitting in each of the 24 centre
# positions. Read through the solver's own conversion, so this is the same
# quantity the deriver copies at kociemba4.h:721 and not a second reading of
# the cube.
centres_of <- function(state) {
  cayleyR:::cube_to_pieces4_cpp(state)$perm[33:56] - 32L
}

# Which colour belongs in each centre position, taken from the solved cube
# rather than from the position's index.
#
# An earlier version of this compared against (i-1) %/% 4 -- the face number --
# on the assumption that colour N lives on face N. The solved cube says
# otherwise: its centre colours read 4 4 4 4 3 3 5 2 ..., so that comparison
# reported 21 centres "off-face" on a solved cube and 21 again after moves that
# changed nothing. A measure that says the same thing about a solved cube and a
# turned one is measuring the numbering, not the cube.
home_colour <- NULL   # set below, once the solved cube has been read

solved <- cube_identity(N)
base <- centres_of(solved)
home_colour <- base

hr("the centres on a solved cube")
cat("colour in each position: ", paste(base, collapse = " "), "\n", sep = "")
cat("face of each position  : ", paste((seq_len(24) - 1L) %/% 4L,
                                       collapse = " "), "\n", sep = "")
cat("\nThe two rows differ, which is why 'off its face' has to be measured\n")
cat("against the first and not the second.\n")

hr("what each generator does to the centres")

cat("`moved` counts positions whose colour changed, and `off-home` counts\n")
cat("positions holding a colour other than the solved cube's. Applied to a\n")
cat("solved cube these are the same number by definition, and they are both\n")
cat("printed so that a disagreement shows up rather than hides.\n\n")

cat(sprintf("  %-5s %-7s %-9s %s\n", "gen", "moved", "off-home", "verdict"))

outer_ok <- character(0)
disturb <- character(0)

for (nm in names(gens)) {
  s <- replay(solved, expand(strsplit(gens[[nm]], " +")[[1]]))
  c2 <- centres_of(s)
  moved <- sum(c2 != base)
  off <- sum(c2 != home_colour)
  verdict <- if (moved == 0L) "centres untouched"
             else sprintf("%d centres displaced", moved)
  if (moved != off) verdict <- paste(verdict, "  [counts disagree!]")
  cat(sprintf("  %-5s %-7d %-9d %s\n", nm, moved, off, verdict))
  if (moved == 0L) outer_ok <- c(outer_ok, nm) else disturb <- c(disturb, nm)
}

cat("\n")
cat("generators that leave the centres alone : ", length(outer_ok),
    "\n  ", paste(outer_ok, collapse = " "), "\n", sep = "")
cat("generators that displace centres        : ", length(disturb),
    "\n  ", paste(disturb, collapse = " "), "\n", sep = "")

# ---- how many centre arrangements are reachable? --------------------------

# The number that decides whether the centre half of the heuristic needs a
# table at all. 24!/(4!)^6 = 3.2e12 is the number of arrangements that exist;
# this is the number phase 3 can actually produce, and they are not the same
# unless the generators are far more free than they look.

hr("closure: how many centre arrangements can phase 3 reach?")

cat("Closing the solved cube's centres under all seventeen generators.\n")
cat("Stops at ", format(cap, big.mark = ",", scientific = FALSE),
    " arrangements if it has not closed by then.\n\n", sep = "")

key <- function(v) paste(v, collapse = ",")

# Full cube states are carried, because a centre arrangement's successors
# depend on the whole cube, not on the centres alone. Only the centre part is
# used as the identity.
seen <- new.env(hash = TRUE, parent = emptyenv())
assign(key(base), TRUE, envir = seen)
frontier <- list(solved)
n_seen <- 1L
depth <- 0L

repeat {
  depth <- depth + 1L
  nxt <- list()
  for (s in frontier) {
    for (nm in names(gens)) {
      t <- replay(s, expand(strsplit(gens[[nm]], " +")[[1]]))
      k <- key(centres_of(t))
      if (!exists(k, envir = seen, inherits = FALSE)) {
        assign(k, TRUE, envir = seen)
        n_seen <- n_seen + 1L
        nxt[[length(nxt) + 1L]] <- t
      }
    }
    if (n_seen > cap) break
  }
  cat(sprintf("  depth %2d: %s new, %s total\n", depth,
              format(length(nxt), big.mark = ","),
              format(n_seen, big.mark = ",")))
  flush.console()
  if (!length(nxt) || n_seen > cap) break
  frontier <- nxt
}

hr("what this decides")

if (n_seen > cap) {
  cat(sprintf("Still growing past %s arrangements. The centre half is not a\n",
              format(cap, big.mark = ",", scientific = FALSE)))
  cat("small table, and needs its own coordinate rather than an enumeration.\n")
  cat("Re-run with a larger cap to find where it closes, or accept that the\n")
  cat("centres need the same treatment the wings just got: a quotient that\n")
  cat("throws away what the phase does not need to tell apart.\n")
} else {
  cat(sprintf("Closed at %s arrangements.\n\n",
              format(n_seen, big.mark = ",")))
  cat("That is the size of the centre table -- against 3,246,670,537,110\n")
  cat("arrangements that exist, and against 479,001,600 for the wings. If it\n")
  cat("is this small the centre half costs nothing: build it the same way,\n")
  cat("take the MAXIMUM of the two bounds rather than the sum, and phase 3\n")
  cat("has an admissible heuristic that knows about both halves of its job.\n")
}
