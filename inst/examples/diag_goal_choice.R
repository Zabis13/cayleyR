#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# One cube, twenty-four goals.
#
# The reduction aims at ONE state: every centre tile back on the face it
# started from. But cube_is_reduced() asks for less than that -- each face must
# be one colour, and which colour is nobody's business, because where the
# centres sit is settled later by turning the whole cube. So the goal the
# schedule aims at is one member of a class of twenty-four, and the other
# twenty-three are just as reduced.
#
# The schedule cannot see them. centre_count() in src/cube_centres.h scores a
# tile by whether it matches `o.of(face)` -- the home colour carried in Orient
# -- so a cube whose centres are solid but turned reads as almost nothing
# built, and the full schedule runs. Measured: seven of twenty findings in
# diag_reduced_nearby.R had their nearby reduced state in exactly that shape,
# six faces solid and four of them turned.
#
# This script turns the GOAL rather than the cube and reduces against each of
# the twenty-four in turn. Conjugation makes that the same search: reducing
# (rot . s) against the identity goal is reducing s against the goal turned by
# rot. The rotation moves no piece relative to another, so its moves are NOT
# counted -- they are a change of what counts as home, not part of the answer.
#
# (An earlier measurement DID charge them, 2 x length(rot) per orientation, and
# so measured a different thing -- "which way round is it cheapest to hold the
# cube", where the turning is real work. That came to 83% of the fixed answer.
# Not charging them, which is the question here, comes to 78%.)
#
# What is printed for each goal:
#
#   counters    what the schedule SEES with that goal: centre tiles home out of
#               24, faces finished out of 6. This is the quantity the schedule
#               steers by, so it is the first place a predictor would live.
#   stages      first centre / l-slice / u-slice / pairs / edges / 3x3x3, so a
#               win can be traced to the stage that produced it -- and so that
#               a win in the reduction which merely moves work into the 3x3x3
#               is visible as such.
#   rotation    the word that defines the goal, and which face it sends to the
#               top. If good goals share a geometry, it shows here.
#   path        every move, split by stage.
#
# And two counters of the cube itself, which no goal changes: uniform faces
# (0-6, one colour whatever colour) and paired edges (0-12). They are the same
# for all twenty-four and are printed once.
#
# Usage:  Rscript diag_goal_choice.R [n_cubes] [depth] [seed]
#   e.g.  Rscript diag_goal_choice.R 3 10 1
# ---------------------------------------------------------------------------

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
N_CUBE <- if (length(args) >= 1) as.integer(args[1]) else 3L
DEPTH  <- if (length(args) >= 2) as.integer(args[2]) else 10L
SEED0  <- if (length(args) >= 3) as.integer(args[3]) else 1L

g  <- cube_group(4)
id <- group_identity(g)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
nm <- names(mv)
CS <- cube_centre_structure(4)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

## The twenty-four goals, as the rotations that define them.
ROT <- cayleyR:::.cube4_orientation_words()

## Which face each rotation brings to the top, read off the state rather than
## worked out from the word: rotate the solved cube and see which colour lands
## on U. Colours are 0..5 in face order, so the answer names a face.
FACE_NAME <- c("U", "R", "F", "D", "L", "B")
top_face_of <- function(w) {
  fwd <- if (length(w)) cube_expand_word(w, 4L) else character(0)
  s <- replay(id, fwd)
  (s[1L] - 1L) %/% 16L                      # the colour now sitting on U
}

## Counters of the cube itself. Neither depends on the goal.
uniform_faces <- function(s) {
  col <- (s[CS$sticker] - 1L) %/% 16L
  sum(vapply(0:5, function(f) length(unique(col[CS$face == f])) == 1L,
             logical(1)))
}

## What the schedule sees under one goal. Turning the cube by `fwd` and reading
## the ordinary counters is the same as leaving it still and turning the goal,
## which is the conjugation the whole script rests on.
counters_under <- function(st, fwd) {
  turned <- replay(st, fwd)
  cc <- cube_centre_counts(turned)
  list(centres = cc, c_sum = sum(cc), c_faces = sum(cc == 4L))
}

## ---- one goal ------------------------------------------------------------
solve_against <- function(st, i) {
  w   <- ROT[[i]]
  fwd <- if (length(w)) cube_expand_word(w, 4L) else character(0)
  r <- try(cayleyR:::.cube_solve4_pick(replay(st, fwd), "kociemba", "fixed"),
           silent = TRUE)
  if (inherits(r, "try-error") || !isTRUE(r$found)) return(NULL)

  # The stage words, cut out of the whole path by the stage lengths. Same
  # method diag_scramble_vs_solve.R uses, and checked the same way.
  ends   <- cumsum(r$stages$n_moves)
  starts <- c(1L, head(ends, -1L) + 1L)
  words  <- Map(function(a, b) if (b >= a) r$path[a:b] else character(0),
                starts, ends)

  red <- sum(r$stages$n_moves[r$stages$name != "3x3x3"])
  cn  <- counters_under(st, fwd)

  list(i = i, word = w, nrot = length(w), top = top_face_of(w),
       total = length(r$path), reduction = red, cube3 = length(r$path) - red,
       stages = r$stages, words = words,
       c_sum = cn$c_sum, c_faces = cn$c_faces, centres = cn$centres)
}

## ---- one cube ------------------------------------------------------------
report_cube <- function(seed) {
  set.seed(seed * 1000L + DEPTH)
  scramble <- sample(nm, DEPTH, replace = TRUE)
  st <- replay(id, scramble)

  cat(sprintf("\n== cube seed %d, depth %d =================================\n",
              seed, DEPTH))
  cat(sprintf("  scrambled by : %s\n", paste(scramble, collapse = " ")))
  cat(sprintf("  the cube      : %d of 6 faces one colour, %d of 12 edges paired\n",
              uniform_faces(st), cube_edge_counts(st)$whole))
  cat("                  (neither depends on the goal -- same for all 24)\n")

  runs <- lapply(seq_along(ROT), function(i) solve_against(st, i))
  ok <- Filter(Negate(is.null), runs)
  if (!length(ok)) { cat("  every goal refused this cube\n"); return(invisible(NULL)) }

  tot <- vapply(ok, function(r) r$total, integer(1))
  base <- ok[[1]]                       # goal 1 is the identity: the ordinary run
  best <- ok[[which.min(tot)]]

  cat(sprintf("\n  ordinary (goal 1, no rotation) : %d moves  (%d reduction + %d 3x3x3)\n",
              base$total, base$reduction, base$cube3))
  cat(sprintf("  best of the 24 (goal %d)        : %d moves  (%d reduction + %d 3x3x3)\n",
              best$i, best$total, best$reduction, best$cube3))
  cat(sprintf("  saved                          : %d moves, %.0f%% of the ordinary\n",
              base$total - best$total, 100 * best$total / base$total))
  cat(sprintf("  goals tying with the best      : %d of %d\n",
              sum(tot == min(tot)), length(ok)))

  ## the table over all goals
  cat("\n  every goal, shortest first:\n")
  cat(sprintf("  %4s %-10s %4s %5s %6s %6s %6s %6s %7s\n",
              "goal", "rotation", "top", "turns", "total", "reduce", "3x3x3",
              "c_sum", "c_faces"))
  ord <- order(tot)
  for (r in ok[ord]) {
    mark <- if (r$i == 1L) " <- ordinary" else if (r$total == min(tot)) " <- best" else ""
    cat(sprintf("  %4d %-10s %4s %5d %6d %6d %6d %6d %7d%s\n",
                r$i, if (r$nrot) paste(r$word, collapse = "") else "(none)",
                FACE_NAME[r$top + 1L], r$nrot,
                r$total, r$reduction, r$cube3, r$c_sum, r$c_faces, mark))
  }

  ## stage lengths, which is where a win is made or given back
  cat("\n  by stage, shortest first:\n")
  all_stages <- unique(unlist(lapply(ok, function(r) r$stages$name)))
  cat(sprintf("  %4s", "goal"))
  for (s in all_stages) cat(sprintf(" %12s", substr(s, 1, 12)))
  cat("\n")
  for (r in ok[ord]) {
    cat(sprintf("  %4d", r$i))
    for (s in all_stages) {
      n <- sum(r$stages$n_moves[r$stages$name == s])
      cat(sprintf(" %12d", n))
    }
    cat("\n")
  }


  invisible(list(base = base$total, best = best$total,
                 base_red = base$reduction, best_red = best$reduction,
                 n_tie = sum(tot == min(tot)),
                 best_i = best$i, best_top = best$top,
                 best_csum = best$c_sum, base_csum = base$c_sum))
}

## ---- the run -------------------------------------------------------------
cat(sprintf("goal choice | n=4, %d cube%s at depth %d, from seed %d\n",
            N_CUBE, if (N_CUBE > 1) "s" else "", DEPTH, SEED0))
cat("the goal is turned, not the cube -- rotations cost nothing, they only\n")
cat("change which colour counts as home for each face\n")

t0 <- proc.time()[["elapsed"]]
summ <- list()
for (k in seq_len(N_CUBE)) {
  s <- report_cube(SEED0 + k - 1L)
  if (!is.null(s)) summ[[length(summ) + 1L]] <- s
  flush(stdout())
}
elapsed <- proc.time()[["elapsed"]] - t0

if (length(summ) > 1L) {
  cat("\n== over the cubes =======================================\n\n")
  cat(sprintf("  %6s %8s %8s %7s %8s %6s %6s\n",
              "cube", "ordinary", "best", "of ord", "saved", "ties", "top"))
  for (k in seq_along(summ)) {
    s <- summ[[k]]
    cat(sprintf("  %6d %8d %8d %6.0f%% %8d %6d %6s\n",
                SEED0 + k - 1L, s$base, s$best, 100 * s$best / s$base,
                s$base - s$best, s$n_tie, FACE_NAME[s$best_top + 1L]))
  }
  b <- vapply(summ, function(s) s$base, numeric(1))
  w <- vapply(summ, function(s) s$best, numeric(1))
  br <- vapply(summ, function(s) s$base_red, numeric(1))
  wr <- vapply(summ, function(s) s$best_red, numeric(1))
  cat(sprintf("\n  mean total      %6.1f -> %6.1f   (%.0f%%)\n",
              mean(b), mean(w), 100 * mean(w) / mean(b)))
  cat(sprintf("  mean reduction  %6.1f -> %6.1f   (%.0f%%)\n",
              mean(br), mean(wr), 100 * mean(wr) / mean(br)))
  cat("\n  The two percentages differ because a shorter reduction can hand a\n")
  cat("  harder cube to the 3x3x3 stage. Only the total is the answer.\n")

  ## Does the counter the schedule steers by pick the winner?
  cat("\n  c_sum of the best goal against c_sum of the ordinary one:\n")
  for (k in seq_along(summ)) {
    s <- summ[[k]]
    cat(sprintf("    cube %d: ordinary %2d, best %2d   %s\n",
                SEED0 + k - 1L, s$base_csum, s$best_csum,
                if (s$best_csum > s$base_csum) "higher"
                else if (s$best_csum < s$base_csum) "LOWER -- the counter misleads"
                else "equal"))
  }
  cat("\n  If the best goal were the one the cube already sits closest to, its\n")
  cat("  c_sum would be the highest of the 24. Where it is not, the counter\n")
  cat("  the schedule steers by is not the one that picks the goal.\n")
}

cat(sprintf("\n%.1f s\n", elapsed))
