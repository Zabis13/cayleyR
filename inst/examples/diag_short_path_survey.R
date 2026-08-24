#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# The short way back against the method, over many cubes.
#
# diag_short_path_lost.R takes one cube apart in full. This runs the same two
# measurements over a range of seeds and prints a table, to say whether what
# one cube showed is a property of the method or an accident of that cube.
#
# For each seed it walks the inverse scramble and notes the first step at
# which the cube counts as reduced -- that is how few moves would have done,
# since the reduction stage has nothing left to want after it. Then it runs
# the method on the same cube and takes it apart into three kinds of move:
#
#   turns    whole-cube rotations. On a 4x4x4 each costs four moves (x is
#            "L' 1x 2x R" and its like) and moves no piece relative to any
#            other, so no counter can change across one. They are the method
#            looking around, not working.
#   idle     any stage that left both counters -- centres and edges -- exactly
#            as it found them. Rotations are idle by construction and are
#            counted separately; this is the rest.
#   working  what is left.
#
# The counters are orientation-invariant on purpose: how many faces hold four
# centres of one colour, and how many of the twelve edges are whole. Scoring a
# centre against the number of the position it sits in would read a rotated
# solved cube as broken -- measured, 8 of 24 -- and every rotation would look
# like damage. See diag_short_path_lost.R for that measurement.
#
# Usage:  Rscript diag_short_path_survey.R [n] [depth] [seeds]
#   e.g.  Rscript diag_short_path_survey.R 4 8 10     seeds 1..10 at depth 8
# ---------------------------------------------------------------------------

library(cayleyR)

args  <- commandArgs(trailingOnly = TRUE)
N     <- if (length(args) >= 1) as.integer(args[1]) else 4L
DEPTH <- if (length(args) >= 2) as.integer(args[2]) else 8L
SEEDS <- if (length(args) >= 3) as.integer(args[3]) else 10L

g  <- cube_group(N)
id <- group_identity(g)
mv <- cube_moves(N); names(mv) <- cube_move_names(N)
nm <- names(mv)
es <- cube_edge_structure(N)
cs <- cube_centre_structure(N)
FACE_SIZE <- N * N
PER_FACE  <- (N - 2L) * (N - 2L)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

invert1 <- function(m)
  if (grepl("'", m, fixed = TRUE)) sub("'", "", m, fixed = TRUE) else paste0(m, "'")
invert <- function(w) rev(vapply(w, invert1, character(1), USE.NAMES = FALSE))

## ---- the counters, invariant to turning the cube -------------------------
counters <- function(state) {
  colour <- (state - 1L) %/% FACE_SIZE
  by_face <- split(colour[cs$sticker], (cs$sticker - 1L) %/% FACE_SIZE)
  biggest <- vapply(by_face, function(v) max(tabulate(v + 1L, 6L)), integer(1))
  c(faces = sum(biggest == PER_FACE),
    edges = cube_edge_counts(state, N, es)$whole)
}

## ---- is a stage a whole-cube rotation? -----------------------------------
#
# Asked of the word, not of its name: a rotation is the word that leaves every
# face one colour when applied to a solved cube, which is the definition and
# does not depend on how the stage was labelled. The six are built once.
rotation_words <- local({
  cand <- list(c("L'","1x","2x","R"), c("R'","2x'","1x'","L"),
               c("D'","1y","2y","U"), c("U'","2y'","1y'","D"),
               c("B","1z'","2z'","F'"), c("F","1z","2z","B'"))
  keep <- vapply(cand, function(w) {
    r <- replay(id, w)
    colour <- (r - 1L) %/% FACE_SIZE
    all(vapply(split(colour, (seq_along(r) - 1L) %/% FACE_SIZE),
               function(v) length(unique(v)) == 1L, logical(1)))
  }, logical(1))
  lapply(cand[keep], paste, collapse = " ")
})

is_rotation <- function(w) paste(w, collapse = " ") %in% rotation_words

## ---- one cube ------------------------------------------------------------
one <- function(seed) {
  set.seed(seed)
  scramble <- sample(nm, DEPTH, replace = TRUE)
  start <- replay(id, scramble)
  back  <- invert(scramble)

  # how few moves the short way needed before nothing was left to reduce
  st <- start
  first_reduced <- NA_integer_
  for (k in seq_along(back)) {
    st <- st[mv[[back[k]]]]
    if (isTRUE(cube_is_reduced(st, N))) { first_reduced <- k; break }
  }

  red <- cube_reduce_cpp(start)
  if (!isTRUE(red$found))
    return(list(seed = seed, ok = FALSE, first_reduced = first_reduced))

  # Which phase a stage belongs to, by the label the C++ side gave it. The
  # centre phase is everything cube_reduce_cpp does before it starts pairing:
  # the first centre, the l-slice, the u-slice, the pair swaps, and the cube
  # rotations between them. Anything labelled "edges" is the pairing stage.
  phase_of <- function(nm) if (identical(nm, "edges")) "edges" else "centres"

  words <- attr(red$stages, "moves")
  prev <- counters(start)
  z <- c(turns = 0L, n_rot = 0L, idle = 0L, work = 0L)
  acc <- list(centres = z, edges = z)
  for (i in seq_len(nrow(red$stages))) {
    w <- words[[i]]
    len <- length(w)
    if (!len) next
    ph <- phase_of(red$stages$name[i])
    now <- counters(as.integer(red$states[[i]]))
    if (is_rotation(w)) {
      acc[[ph]][["turns"]] <- acc[[ph]][["turns"]] + len
      acc[[ph]][["n_rot"]] <- acc[[ph]][["n_rot"]] + 1L
    } else if (identical(now, prev)) {
      acc[[ph]][["idle"]] <- acc[[ph]][["idle"]] + len
    } else {
      acc[[ph]][["work"]] <- acc[[ph]][["work"]] + len
    }
    prev <- now
  }

  total <- sum(red$stages$n_moves)
  get <- function(ph, k) as.integer(acc[[ph]][[k]])
  list(seed = seed, ok = TRUE, first_reduced = first_reduced, total = total,
       turns = get("centres", "turns") + get("edges", "turns"),
       n_rot = get("centres", "n_rot") + get("edges", "n_rot"),
       idle  = get("centres", "idle")  + get("edges", "idle"),
       work  = get("centres", "work")  + get("edges", "work"),
       c_turns = get("centres", "turns"), c_idle = get("centres", "idle"),
       c_work  = get("centres", "work"),
       e_turns = get("edges", "turns"),   e_idle = get("edges", "idle"),
       e_work  = get("edges", "work"),
       start_faces = counters(start)[["faces"]],
       start_edges = counters(start)[["edges"]])
}

## ---- the run -------------------------------------------------------------
cat(sprintf("the short way back against the method | n=%d, depth %d, seeds 1..%d\n\n",
            N, DEPTH, SEEDS))

rows <- lapply(seq_len(SEEDS), one)

cat("  For each cube: how few moves of the inverse scramble left nothing to\n")
cat("  reduce, against what the method spent, and where the method's moves\n")
cat("  went. Rotations turn the cube and move nothing relative to anything;\n")
cat("  idle stages left both counters where they found them.\n\n")

cat(sprintf("  %-5s %-7s %-7s %-7s  %-13s %-11s %-11s\n",
            "seed", "start", "short", "method", "rotations", "idle", "working"))
cat(sprintf("  %-5s %-7s %-7s %-7s  %-13s %-11s %-11s\n",
            "", "f/e", "moves", "moves", "moves (n)", "moves", "moves"))
cat("  ", strrep("-", 68), "\n", sep = "")

ok <- Filter(function(r) isTRUE(r$ok), rows)
for (r in rows) {
  if (!isTRUE(r$ok)) {
    cat(sprintf("  %-5d %-7s %-7s  reduction failed\n", r$seed, "", ""))
    next
  }
  cat(sprintf("  %-5d %d/%-5d %-7s %-7d  %3d (%2d)      %-11d %-11d\n",
              r$seed, r$start_faces, r$start_edges,
              if (is.na(r$first_reduced)) "never" else r$first_reduced,
              r$total, r$turns, r$n_rot, r$idle, r$work))
}

if (length(ok)) {
  g <- function(f) vapply(ok, f, numeric(1))
  tot <- g(function(r) r$total)
  sh  <- g(function(r) if (is.na(r$first_reduced)) DEPTH else r$first_reduced)
  cat("  ", strrep("-", 68), "\n", sep = "")
  cat(sprintf("  %-5s %-7s %-7.1f %-7.0f  %3.0f (%4.1f)    %-11.0f %-11.0f\n",
              "mean", "", mean(sh), mean(tot),
              mean(g(function(r) r$turns)), mean(g(function(r) r$n_rot)),
              mean(g(function(r) r$idle)), mean(g(function(r) r$work))))

  cat("\n-- read together --------------------------------------------------\n")
  cat(sprintf("  short way to nothing-left-to-reduce : %.1f moves on average, worst %d\n",
              mean(sh), max(sh)))
  cat(sprintf("  the method                          : %.0f moves on average, worst %d\n",
              mean(tot), max(tot)))
  cat(sprintf("  ratio                               : %.0f:1 on the means\n",
              mean(tot) / max(1, mean(sh))))
  spent <- mean(g(function(r) r$turns)) + mean(g(function(r) r$idle))
  cat(sprintf("\n  of the method's %.0f moves, %.0f move no counter at all:\n",
              mean(tot), spent))
  cat(sprintf("    %.0f turning the cube (%.1f rotations, four moves each)\n",
              mean(g(function(r) r$turns)), mean(g(function(r) r$n_rot))))
  cat(sprintf("    %.0f in stages that left both counters as they found them\n",
              mean(g(function(r) r$idle))))
  cat(sprintf("    %.0f%% of the whole reduction\n", 100 * spent / mean(tot)))
  reduced_early <- sum(vapply(ok, function(r)
    !is.na(r$first_reduced) && r$first_reduced < DEPTH, logical(1)))
  cat(sprintf("\n  cubes where the short way had nothing left to reduce before its\n"))
  cat(sprintf("  last move : %d of %d\n", reduced_early, length(ok)))

  ## ---- the same moves, split by which phase spent them --------------------
  #
  # Whether the wasted moves sit in the centres or in the pairing decides
  # where a fix would have to go, and the two are easy to confuse: a stage can
  # be expensive without being idle. The pairing stage is the clearest case --
  # it moves the edge count nearly every time, so it is barely idle at all,
  # and yet it is where most of the moves go.
  cat("\n-- by phase -------------------------------------------------------\n")
  cat("  The centre phase is everything up to the pairing: the first centre,\n")
  cat("  the l-slice and u-slice, the pair swaps, and the rotations between\n")
  cat("  them. The edge phase is the pairing. Costly and idle are not the\n")
  cat("  same thing, and this separates them.\n\n")

  cat(sprintf("  %-5s  %-25s  %-25s\n", "", "centres", "edges"))
  cat(sprintf("  %-5s  %-7s %-7s %-8s  %-7s %-7s %-8s\n",
              "seed", "rot", "idle", "working", "rot", "idle", "working"))
  cat("  ", strrep("-", 60), "\n", sep = "")
  for (r in ok)
    cat(sprintf("  %-5d  %-7d %-7d %-8d  %-7d %-7d %-8d\n", r$seed,
                r$c_turns, r$c_idle, r$c_work,
                r$e_turns, r$e_idle, r$e_work))
  cat("  ", strrep("-", 60), "\n", sep = "")
  cat(sprintf("  %-5s  %-7.0f %-7.0f %-8.0f  %-7.0f %-7.0f %-8.0f\n", "mean",
              mean(g(function(r) r$c_turns)), mean(g(function(r) r$c_idle)),
              mean(g(function(r) r$c_work)),
              mean(g(function(r) r$e_turns)), mean(g(function(r) r$e_idle)),
              mean(g(function(r) r$e_work))))

  c_tot <- mean(g(function(r) r$c_turns + r$c_idle + r$c_work))
  e_tot <- mean(g(function(r) r$e_turns + r$e_idle + r$e_work))
  c_dead <- mean(g(function(r) r$c_turns + r$c_idle))
  e_dead <- mean(g(function(r) r$e_turns + r$e_idle))
  cat(sprintf("\n  centres : %3.0f moves, %3.0f of them moving no counter (%.0f%%)\n",
              c_tot, c_dead, 100 * c_dead / max(1, c_tot)))
  cat(sprintf("  edges   : %3.0f moves, %3.0f of them moving no counter (%.0f%%)\n",
              e_tot, e_dead, 100 * e_dead / max(1, e_tot)))
}
cat("\n")
