#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Where the short way back goes missing.
#
# A cube five moves from solved is answered in eighty-five. The five moves
# exist -- they are the scramble read backwards -- so the question is not
# whether a short word is there but why the method never comes near it. This
# script puts the two side by side on the same cube.
#
# The left half walks the short way back, one move at a time, and asks after
# every move what the reduction would notice: how many centre stickers sit
# home, how many of the twelve edges are whole, and whether the cube counts as
# reduced. If the walk passes through a reduced state early, the method could
# in principle have stopped there. If it does not -- if reduced only becomes
# true on the very last move, or never -- then no amount of tuning inside the
# reduction would find this word, because the route it needs does not go
# through the states the reduction steers by.
#
# The right half runs the reduction on the same cube and prints its own
# stages: what each one cost, and what it moved the same three counters to.
# Read against the walk, it shows the method spending moves on counters the
# short way never troubles.
#
# Usage:  Rscript diag_short_path_lost.R [n] [depth] [seed]
#   e.g.  Rscript diag_short_path_lost.R 4 5 10     the 5-into-85 case
#         Rscript diag_short_path_lost.R 4 6 10     its 6-move neighbour
# ---------------------------------------------------------------------------

library(cayleyR)

args  <- commandArgs(trailingOnly = TRUE)
N     <- if (length(args) >= 1) as.integer(args[1]) else 4L
DEPTH <- if (length(args) >= 2) as.integer(args[2]) else 5L
SEED  <- if (length(args) >= 3) as.integer(args[3]) else 10L

g  <- cube_group(N)
id <- group_identity(g)
mv <- cube_moves(N); names(mv) <- cube_move_names(N)
nm <- names(mv)
es <- cube_edge_structure(N)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

invert1 <- function(m)
  if (grepl("'", m, fixed = TRUE)) sub("'", "", m, fixed = TRUE) else paste0(m, "'")
invert <- function(w) rev(vapply(w, invert1, character(1), USE.NAMES = FALSE))

## ---- the three things the reduction steers by ----------------------------
#
# Centres: how many centre stickers show their own face's colour. The
# reduction's first two stages are entirely about driving this to 24 on a
# 4x4x4 (four to a face, six faces).
#
# Edges: how many of the twelve cube edges are whole, which is what the
# pairing stage ranks its candidates by.
#
# Reduced: the two together, as cube_is_reduced states it -- the test the
# solver applies to decide the reduction is done.
CENTRES_TOTAL <- 6L * (N - 2L) * (N - 2L)
cs <- cube_centre_structure(N)
FACE_SIZE <- N * N

# How far along the centres are, counted so that turning the whole cube does
# not change the answer.
#
# The obvious count -- how many centre stickers show their own face's colour --
# is not that. It scores a sticker against the number of the position it sits
# in, so a perfectly built cube that has been rotated reads as broken: the
# faces are all one colour, just not the colours their positions started with.
# Measured: a solved 4x4x4 put through the method's own z' word scores 8 of 24
# that way, having lost nothing at all. The C++ side avoids this by carrying an
# Orient and scoring against the home colour of each position; nothing exported
# to R does, so the measure here is built to not need one.
#
# What is counted instead is how many faces hold four centres of a single
# colour. A rotation moves such a face somewhere else without breaking it, so
# the count is the same before and after -- which is the property a diagnostic
# needs. The size of the largest block on each face gives the same reading one
# step finer, for watching a face fill up.
centre_progress <- function(state) {
  colour <- (state - 1L) %/% FACE_SIZE
  by_face <- split(colour[cs$sticker], (cs$sticker - 1L) %/% FACE_SIZE)
  biggest <- vapply(by_face, function(v) max(tabulate(v + 1L, 6L)), integer(1))
  list(faces = sum(biggest == (N - 2L) * (N - 2L)), home = sum(biggest))
}

counters <- function(state) {
  ec <- cube_edge_counts(state, N, es)
  cp <- centre_progress(state)
  list(faces = cp$faces, home = cp$home, edges = ec$whole,
       reduced = isTRUE(cube_is_reduced(state, N)))
}

fmt <- function(cn) sprintf("centres %d/6 faces (%2d/%d home)   edges %2d/12   reduced %-5s",
                            cn$faces, cn$home, CENTRES_TOTAL,
                            cn$edges, cn$reduced)

## ---- the cube ------------------------------------------------------------
set.seed(SEED)
scramble <- sample(nm, DEPTH, replace = TRUE)
start <- replay(id, scramble)
back  <- invert(scramble)

cat(sprintf("where the short path goes | n=%d, depth %d, seed %d\n\n",
            N, DEPTH, SEED))
cat(sprintf("scrambled by : %s\n", paste(scramble, collapse = " ")))
cat(sprintf("undone by    : %s   (%d moves)\n\n", paste(back, collapse = " "),
            length(back)))

## ---- half one: the short way back, move by move --------------------------
cat("-- the short way back ---------------------------------------------\n")
cat("Each line is the cube after one more move of the inverse scramble.\n")
cat("These are the states a five-move answer passes through; the question\n")
cat("is whether the reduction could recognise any of them.\n\n")

cat(sprintf("  %-4s %-6s  %s\n", "step", "move", fmt(counters(start))))
cat(sprintf("  %-4s %-6s  %s\n", "0", "--", ""))
st <- start
first_reduced <- NA_integer_
for (k in seq_along(back)) {
  st <- st[mv[[back[k]]]]
  cn <- counters(st)
  if (is.na(first_reduced) && cn$reduced) first_reduced <- k
  cat(sprintf("  %-4d %-6s  %s%s\n", k, back[k], fmt(cn),
              if (cn$reduced && k == first_reduced) "   <-- reduced here" else ""))
}
solved_at_end <- identical(as.integer(st), as.integer(id))
cat(sprintf("\n  ends solved  : %s\n", solved_at_end))
if (is.na(first_reduced)) {
  cat("  reduced      : never on this walk\n")
} else {
  cat(sprintf("  reduced      : first at step %d of %d\n",
              first_reduced, length(back)))
}

## ---- half two: what the reduction does to the same cube ------------------
cat("\n-- what the reduction does ----------------------------------------\n")
cat("The same cube, put through the method. Each line is one of its own\n")
cat("stages: what it cost and where it left the three counters.\n\n")

red <- cube_reduce_cpp(start)
if (!isTRUE(red$found)) {
  cat(sprintf("  reduction failed: %s\n", red$failure))
} else {
  words <- attr(red$stages, "moves")
  cat(sprintf("  %-14s %-8s  %s\n", "stage", "moves", fmt(counters(start))))
  cat(sprintf("  %-14s %-8s  %s\n", "(start)", "--", ""))
  total <- 0L
  for (i in seq_len(nrow(red$stages))) {
    s <- as.integer(red$states[[i]])
    total <- total + red$stages$n_moves[i]
    cat(sprintf("  %-14s %-8d  %s\n", red$stages$detail[i],
                red$stages$n_moves[i], fmt(counters(s))))
  }
  cat(sprintf("\n  reduction    : %d moves\n", total))
  cat(sprintf("  the walk     : %d moves\n", length(back)))
  cat(sprintf("  ratio        : %.0fx\n", total / max(1L, length(back))))

  ## ---- the stages, spelled out -------------------------------------------
  cat("\n  stage by stage:\n")
  for (i in seq_len(nrow(red$stages))) {
    w <- words[[i]]
    if (!length(w)) next
    cat(sprintf("    [%s]\n", red$stages$detail[i]))
    for (j in seq(1, length(w), by = 16))
      cat("      ", paste(w[j:min(j + 15, length(w))], collapse = " "), "\n",
          sep = "")
  }
}

## ---- the two read together ------------------------------------------------
cat("\n-- read together --------------------------------------------------\n")
if (isTRUE(red$found)) {
  ec_start <- counters(start)
  cat(sprintf("The cube starts at %s\n", fmt(ec_start)))
  if (is.na(first_reduced)) {
    cat("and the short way back never passes through a reduced state: it\n")
    cat("goes straight to solved without the counters the reduction reads\n")
    cat("ever saying so. A method that stops when reduced is true cannot\n")
    cat("find this word -- not because it searches badly, but because the\n")
    cat("word does not lie along the states it steers by.\n")
  } else if (first_reduced < length(back)) {
    cat(sprintf("and the short way back is reduced by step %d, %d moves before\n",
                first_reduced, length(back) - first_reduced))
    cat("it is solved. The reduction reaches the same condition in ")
    cat(sprintf("%d.\n", sum(red$stages$n_moves)))
    cat("Both end reduced; they simply take different routes to it, and the\n")
    cat("method's route is fixed in advance.\n")
  } else {
    cat(sprintf("and the short way back is reduced only on its last move (%d).\n",
                first_reduced))
    cat("Reduction and solution coincide here, so there is no earlier point\n")
    cat("at which the method could have stopped.\n")
  }
}
cat("\n")
