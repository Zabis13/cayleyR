#!/usr/bin/env Rscript
# One cube, followed move by move through phases 1 and 2.
#
# The controlled table in diag_scramble_shape.R settled which variable
# matters. Splitting the solve rate by axis count AND by how long phases 1 and
# 2 ran:
#
#                 1 axis  2 axes  3 axes
#   short (<=4)     100      91      40
#   medium (5-8)    100     100      78
#   long (>8)        --      15      20
#
# The bottom row is the finding. Once phases 1 and 2 together run past eight
# moves the cube is lost -- 15% and 20% -- whatever the axis count. Within the
# short and medium bands the rate stays high. The axis count rose with the
# phase length (3.6 moves at one axis, 6.1 at two, 9.2 at three) and was
# standing in for it.
#
# So the question is no longer "which scrambles" but "what are phases 1 and 2
# doing". A cube six quarter turns from solved has a six-move solution; two
# phases that spend nine moves on it have carried it somewhere further away
# than it started, and phase 3 is left to undo that.
#
# This traces one such cube. After every move of phases 1 and 2 it reports what
# the cube looks like by measures those phases are supposed to be improving:
#
#   centres home   how many centre pieces sit on the face they belong to
#   faces built    how many faces show one colour across their four centres
#   wings paired   how many of the twelve dedges have their two halves together
#   phase goals    whether each phase's own coordinate says it has arrived
#
# A phase working properly makes its own measures go up. A phase whose
# measures go DOWN -- taking centres off their faces, breaking pairs -- while
# its coordinate reports progress is a phase whose coordinate does not mean
# what it is taken to mean.
#
# Run with:  Rscript inst/examples/diag_trace_phases.R
#            Rscript inst/examples/diag_trace_phases.R "1y U 1z L 2z' 1x"

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)

N           <- 4L
node_budget <- 1e6

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

yn <- function(x) if (isTRUE(x)) "yes" else "no"

# ---- the measures ---------------------------------------------------------

# A centre piece is home when its colour matches the face it sits on. This is
# the plainest reading of "the centres are where they belong" and needs no
# solver to compute.
centres_home <- function(s) {
  n <- 0L
  for (f in 0:5) {
    idx <- f * 16L + c(5L, 6L, 9L, 10L) + 1L
    n <- n + sum((s[idx] - 1L) %/% 16L == f)
  }
  n
}

# A face is built when its four centres agree with each other, wherever they
# now sit. Faces can be built without being home -- the cube may be turned.
faces_built <- function(s) {
  sum(vapply(0:5, function(f) {
    idx <- f * 16L + c(5L, 6L, 9L, 10L) + 1L
    length(unique((s[idx] - 1L) %/% 16L)) == 1L
  }, logical(1)))
}

pairs_made <- function(s) {
  p <- cube_pieces(4)
  e <- p[p$n_stickers == 2L, ]
  st <- lapply(strsplit(e$stickers, ","), as.integer)
  key <- vapply(st, function(i) paste(sort((i - 1L) %/% 16L), collapse = "-"),
                character(1))
  sum(vapply(split(st, key), function(g2) {
    a <- sort((s[g2[[1]]] - 1L) %/% 16L)
    b <- sort((s[g2[[2]]] - 1L) %/% 16L)
    identical(a, b)
  }, logical(1)))
}

# ---- pick a cube ----------------------------------------------------------

# Either the word given on the command line, or a search for one that shows
# the failure: phases 1 and 2 running long on a short scramble.
if (length(args) >= 1 && nzchar(args[[1]])) {
  word <- strsplit(trimws(args[[1]]), " +")[[1]]
  state <- replay(cube_identity(N), word)
  cat("using the scramble given on the command line\n")
} else {
  set.seed(2026)
  cat("looking for a 6-move scramble whose phases 1+2 run past 8 moves")
  found <- FALSE
  for (attempt in seq_len(200L)) {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (identical(s, cube_identity(N))) next
    p2 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = node_budget)
    if (length(p2) > 8L) { word <- w; state <- s; found <- TRUE; break }
    if (attempt %% 10L == 0L) { cat("."); flush.console() }
  }
  cat("\n")
  if (!found) stop("no such cube in 200 attempts", call. = FALSE)
}

hr("the cube")
cat("scramble : ", paste(word, collapse = " "), "  (", length(word),
    " moves)\n", sep = "")
cat("state    : ", paste(state, collapse = " "), "\n", sep = "")

# The word we know solves it, and its length -- an upper bound on the distance.
inv <- rev(vapply(word, function(m) if (grepl("'$", m)) sub("'$", "", m)
                                    else paste0(m, "'"),
                  character(1), USE.NAMES = FALSE))
cat("inverse  : ", paste(inv, collapse = " "), "  (solves it: ",
    yn(cube_is_colour_solved(replay(state, inv))), ")\n", sep = "")

cat(sprintf("\nat the start: centres home %d/24, faces built %d/6, wings paired %d/12\n",
            centres_home(state), faces_built(state), pairs_made(state)))

# ---- the trace ------------------------------------------------------------

p1 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 1L,
                                           node_budget = node_budget)
p12 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                            node_budget = node_budget)
p2 <- if (length(p12) > length(p1)) {
  p12[(length(p1) + 1L):length(p12)]
} else {
  character(0)
}

hr("phase 1, move by move")
cat("Phase 1 puts the F/B centres onto their axis. Its coordinate sees only\n")
cat("which centres are F/B and which are not.\n\n")
cat(sprintf("  %-4s %-6s %-14s %-13s %-14s %s\n",
            "step", "move", "centres home", "faces built", "wings paired",
            "at p1 goal"))

cur <- state
cat(sprintf("  %-4s %-6s %-14d %-13d %-14d %s\n", "-", "-",
            centres_home(cur), faces_built(cur), pairs_made(cur),
            yn(cayleyR:::cube_at_phase_goal_cpp(cur, 1L))))
for (k in seq_along(p1)) {
  cur <- cur[mv[[p1[k]]]]
  cat(sprintf("  %-4d %-6s %-14d %-13d %-14d %s\n", k, p1[k],
              centres_home(cur), faces_built(cur), pairs_made(cur),
              yn(cayleyR:::cube_at_phase_goal_cpp(cur, 1L))))
}

hr("phase 2, move by move")
cat("Phase 2 puts the remaining centres onto their axes and settles wing\n")
cat("parity. Its coordinate sees centres by axis, plus one parity bit.\n\n")
cat(sprintf("  %-4s %-6s %-14s %-13s %-14s %s\n",
            "step", "move", "centres home", "faces built", "wings paired",
            "at p2 goal"))
cat(sprintf("  %-4s %-6s %-14d %-13d %-14d %s\n", "-", "-",
            centres_home(cur), faces_built(cur), pairs_made(cur),
            yn(cayleyR:::cube_at_phase_goal_cpp(cur, 2L))))
for (k in seq_along(p2)) {
  cur <- cur[mv[[p2[k]]]]
  cat(sprintf("  %-4d %-6s %-14d %-13d %-14d %s\n", k, p2[k],
              centres_home(cur), faces_built(cur), pairs_made(cur),
              yn(cayleyR:::cube_at_phase_goal_cpp(cur, 2L))))
}

hr("what phase 3 was handed")

s2 <- cur
p3 <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = node_budget)
cat(sprintf("state after phases 1+2: centres home %d/24, faces built %d/6, wings paired %d/12\n",
            centres_home(s2), faces_built(s2), pairs_made(s2)))
cat(sprintf("phase 3: %s after %s nodes%s\n", p3$outcome,
            format(p3$nodes, scientific = FALSE, big.mark = ","),
            if (isTRUE(p3$found)) sprintf(", %d moves", length(p3$path)) else ""))

hr("the same cube, phases 1+2 against the scramble")

cat(sprintf("scramble            : %2d moves\n", length(word)))
cat(sprintf("phase 1             : %2d moves\n", length(p1)))
cat(sprintf("phase 2             : %2d moves\n", length(p2)))
cat(sprintf("phases 1+2 together : %2d moves\n", length(p1) + length(p2)))

cat("\nCompare the measures at the start with the measures after phases 1+2.\n")
cat("Those phases are meant to be building centres. If the count of centres\n")
cat("home, or of wings paired, is LOWER at the end than at the start, the\n")
cat("phases have taken the cube apart to satisfy a coordinate that does not\n")
cat("track what it is supposed to.\n\n")
cat(sprintf("  centres home : %2d at the start -> %2d after phases 1+2\n",
            centres_home(state), centres_home(s2)))
cat(sprintf("  faces built  : %2d at the start -> %2d after phases 1+2\n",
            faces_built(state), faces_built(s2)))
cat(sprintf("  wings paired : %2d at the start -> %2d after phases 1+2\n",
            pairs_made(state), pairs_made(s2)))
