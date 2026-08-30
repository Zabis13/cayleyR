#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Reduced states the schedule walks straight past.
#
# The reduction does not look for the NEAREST reduced state -- it builds its
# own by a fixed schedule and runs every stage whatever the cube arrived like.
# So a cube can sit a handful of moves from a reduced state and still be
# charged the full two hundred. Measured on seed 1 at depth 9: a reduced state
# lay five moves away, and the schedule spent a hundred and sixty-eight.
#
# This hunts for such cubes and prints what they have in common, so that a
# cheap test for "reduced is near" can be looked for rather than guessed at.
#
# How the distance is known without a search:
#
#   The cube was scrambled by a known word, so the way home is the inverse of
#   that word, and every state along it is a state of known distance. Walking
#   it and asking cube_is_reduced() at each step gives an UPPER BOUND on the
#   distance to reduced -- exact only if no shorter route exists off the walk,
#   which is not claimed. A bound is enough here: the finding is that the
#   schedule spends 168 where 5 would do, and a bound of 5 proves it.
#
#   This is why no BFS appears below. A breadth-first search around each state
#   would give the true distance, but in R it runs at some 2800 nodes a second
#   -- depth 3 in four seconds, depth 4 in eighty-four -- which buys a worse
#   answer than the scramble already hands over for free.
#
# What is measured at the moment of the finding -- the state on the way home
# that is first reduced, and the state the schedule was handed:
#
#   centres     how many of the four tiles of each face's centre are home,
#               as six numbers; and their sum, out of 24
#   edges       how many of the twelve dedges are paired, out of 12
#   scramble    its depth, and how it splits between outer face turns
#               (U R F D L B) and inner slice turns (1x 2x 1y ...)
#   waste       what the schedule spent against what the walk shows possible
#
# Usage:  Rscript diag_reduced_nearby.R [n_found] [max_depth] [seed] [near]
#   e.g.  Rscript diag_reduced_nearby.R 10 14 1 6     ten cubes, reduced <= 6
# ---------------------------------------------------------------------------

library(cayleyR)

args     <- commandArgs(trailingOnly = TRUE)
N_FOUND  <- if (length(args) >= 1) as.integer(args[1]) else 10L
MAXDEPTH <- if (length(args) >= 2) as.integer(args[2]) else 14L
SEED0    <- if (length(args) >= 3) as.integer(args[3]) else 1L
NEAR     <- if (length(args) >= 4) as.integer(args[4]) else 6L

g  <- cube_group(4)
id <- group_identity(g)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
nm <- names(mv)

OUTER <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}
invert1 <- function(m)
  if (grepl("'", m, fixed = TRUE)) sub("'", "", m, fixed = TRUE) else paste0(m, "'")
invert <- function(w) rev(vapply(w, invert1, character(1), USE.NAMES = FALSE))

## ---- the features of one state -------------------------------------------
## Everything here is a counter the package already keeps; nothing is derived
## from the scramble, so a solver could read the same numbers off a cube handed
## to it with no history.
## The stickers of the six centres, and which face each belongs to. Read once
## rather than per state: it is a property of the cube's shape, not of a
## position.
CS <- cube_centre_structure(4)

## How many faces carry a centre of ONE colour, whatever colour that is.
##
## This is not what cube_centre_counts measures, and the difference is the point
## of the whole script. That counter asks whether each tile is on its ORIGINAL
## face, so a cube whose centres are built but turned reads as zero. Yet
## cube_is_reduced asks only for uniformity -- a reduced cube may have every
## centre solid and every one of them in the wrong place, because where they sit
## is the 3x3x3's business, settled later by turning the whole cube.
##
## Measured over 20 findings: the reduced state nearby was always one of two
## shapes by cube_centre_counts -- 24 of 24 (13 cubes) or 8 of 24 (7 cubes),
## never anything between. The second shape is a cube with all six centres
## solid and four of them turned, which the old counter scores as 8 and this one
## scores as 6. So the two shapes are one shape, and only this counter sees it.
uniform_faces <- function(s) {
  col <- (s[CS$sticker] - 1L) %/% 16L
  sum(vapply(0:5, function(f) length(unique(col[CS$face == f])) == 1L,
             logical(1)))
}

features <- function(s) {
  cc <- cube_centre_counts(s)
  ec <- cube_edge_counts(s)
  list(centres = cc, centre_sum = sum(cc),
       faces_done = sum(cc == 4L), faces_empty = sum(cc == 0L),
       centre_max = max(cc),
       uniform = uniform_faces(s),
       edges = ec$whole)
}

## ---- one scramble --------------------------------------------------------
## Returns NULL unless the cube is one of the interesting ones: a reduced state
## within NEAR moves along the way home, while the schedule spends far more.
## The scramble is drawn the way generate_state() draws one -- a walk away from
## the identity, moves taken uniformly from the group's alphabet with
## replacement -- but the WORD is kept, which generate_state does not return.
## The word is the whole method here: the way home is its inverse, and that is
## what gives the distance to reduced without a search.
##
## The seed is per cube rather than per group of cubes. Seeding once and drawing
## several depths from it hands out nested words -- the six-move draw is the
## prefix of the seven-move draw -- so what looks like six findings is one cube
## counted six times, and the columns move in lockstep for that reason and no
## other. An earlier run of this script did exactly that.
probe <- function(seed, depth) {
  set.seed(seed * 1000L + depth)
  scramble <- sample(nm, depth, replace = TRUE)
  st <- replay(id, scramble)

  # Where the walk home first meets a reduced state. Move 0 is the cube as
  # handed over, which is worth testing -- an already reduced cube is the
  # extreme case of "reduced is near".
  if (cube_is_reduced(st)) {
    d_red <- 0L
  } else {
    back <- invert(scramble)
    s <- st; d_red <- NA_integer_
    for (i in seq_along(back)) {
      s <- s[mv[[back[i]]]]
      if (cube_is_reduced(s)) { d_red <- i; break }
    }
  }
  # A reduced state is always found by the end of the walk -- the solved cube
  # is reduced, so walking the whole inverse scramble reaches one every time.
  # That makes "a reduced state exists within k moves" useless as it stands: at
  # depth 9 it is satisfied by the ninth move on every cube there is. Measured
  # over 60 scrambles at each of depths 6, 9, 12, 16 and 20, the distribution
  # of the first reduced state on the way home sits squarely at the end of the
  # walk and SHIFTS with the depth -- pile at 5-6 when the depth is 6, at 11-12
  # when it is 12, at 19-20 when it is 20. What the measure was reporting on
  # most cubes was the arrival home, not a shortcut.
  #
  # So the finding is not that a reduced state is near in absolute terms but
  # that it comes EARLY -- well before the walk would have finished anyway.
  # `early` below is that margin, and it is what separates a genuine shortcut
  # from the cube simply being solved.
  if (is.na(d_red)) return(NULL)
  early <- depth - d_red
  if (d_red > NEAR || early < 3L) return(NULL)

  # What the schedule charges, over all six starting faces, so that the finding
  # is not an artefact of one unlucky choice.
  cost <- vapply(0:5, function(f) {
    r <- try(cube_reduce_cpp(st, f), silent = TRUE)
    if (inherits(r, "try-error") || !isTRUE(r$found)) NA_integer_
    else length(r$path)
  }, integer(1))
  if (all(is.na(cost))) return(NULL)

  best_face <- (0:5)[which.min(cost)]
  best_cost <- min(cost, na.rm = TRUE)

  # Where along the schedule's own path it first becomes reduced. If that is
  # well before the end, the schedule passed through the goal and kept going --
  # a second, independent kind of waste, and one an early exit would fix.
  r <- cube_reduce_cpp(st, best_face)
  s <- st; hit <- NA_integer_
  for (i in seq_along(r$path)) {
    s <- s[mv[[r$path[i]]]]
    if (cube_is_reduced(s)) { hit <- i; break }
  }

  # The reduced state the walk found, and the state the schedule was handed.
  near_state <- if (d_red == 0L) st else replay(st, invert(scramble)[seq_len(d_red)])

  list(seed = seed, depth = depth, scramble = scramble,
       d_red = d_red, early = early,
       cost = cost, best_face = best_face, best_cost = best_cost,
       overspend = best_cost - d_red,
       passes_through = hit,
       n_outer = sum(scramble %in% OUTER),
       n_inner = sum(!(scramble %in% OUTER)),
       f_given = features(st), f_near = features(near_state))
}

## ---- the hunt ------------------------------------------------------------
cat(sprintf("reduced states the schedule walks past | n=4, depths 2..%d, from seed %d\n",
            MAXDEPTH, SEED0))
cat(sprintf("a finding: a reduced state within %d moves, reached at least 3 moves\n",
            NEAR))
cat("           before the walk home would have arrived anyway\n\n")

found <- list()
seen  <- 0L
seed  <- SEED0
t0    <- proc.time()[["elapsed"]]

while (length(found) < N_FOUND && seed < SEED0 + 4000L) {
  for (d in 2:MAXDEPTH) {
    if (length(found) >= N_FOUND) break
    seen <- seen + 1L
    hit <- probe(seed, d)
    # Uninteresting unless the schedule really did overspend: a cube that is
    # three moves from reduced and reduced in four is the method working.
    if (!is.null(hit) && hit$overspend >= 20L) {
      found[[length(found) + 1L]] <- hit
      cat(sprintf("  found %2d/%d  seed %d depth %2d  reduced in %d, schedule %d\n",
                  length(found), N_FOUND, seed, d, hit$d_red, hit$best_cost))
      flush(stdout())
    }
  }
  seed <- seed + 1L
}
elapsed <- proc.time()[["elapsed"]] - t0

cat(sprintf("\n%d found in %d scrambles (%.0f%%), %.1f s\n\n",
            length(found), seen, 100 * length(found) / seen, elapsed))

if (!length(found)) {
  cat("nothing found -- widen with a larger `near` or a deeper `max_depth`\n")
  quit(save = "no")
}

## ---- the cases, one by one -----------------------------------------------
for (h in found) {
  cat(sprintf("== seed %d, depth %d ================================\n",
              h$seed, h$depth))
  cat(sprintf("  scrambled by : %s\n", paste(h$scramble, collapse = " ")))
  cat(sprintf("  outer/inner  : %d outer face turns, %d inner slice turns\n",
              h$n_outer, h$n_inner))
  cat(sprintf("  reduced in   : %d moves along the way home -- %d moves before\n",
              h$d_red, h$early))
  cat(sprintf("                 the walk would have arrived anyway\n"))
  cat(sprintf("  schedule     : %d moves (face %d); by face %s\n",
              h$best_cost, h$best_face,
              paste(ifelse(is.na(h$cost), "--", h$cost), collapse = " ")))
  cat(sprintf("  overspend    : %d moves, %.0fx\n", h$overspend,
              h$best_cost / max(h$d_red, 1L)))
  if (!is.na(h$passes_through) && h$passes_through < h$best_cost) {
    cat(sprintf("  and note     : its own path is reduced by move %d of %d --\n",
                h$passes_through, h$best_cost))
    cat(sprintf("                 %d further moves are spent on a done cube\n",
                h$best_cost - h$passes_through))
  }

  fg <- h$f_given; fn <- h$f_near
  cat("  the cube as handed over:\n")
  cat(sprintf("    centres    %s  = %d of 24, %d faces done, %d empty\n",
              paste(sprintf("%d", fg$centres), collapse = " "),
              fg$centre_sum, fg$faces_done, fg$faces_empty))
  cat(sprintf("    uniform    %d of 6 faces one colour (whatever colour)\n",
              fg$uniform))
  cat(sprintf("    edges      %d of 12 paired\n", fg$edges))
  cat("  the reduced state nearby:\n")
  cat(sprintf("    centres    %s  = %d of 24, %d faces done, %d empty\n",
              paste(sprintf("%d", fn$centres), collapse = " "),
              fn$centre_sum, fn$faces_done, fn$faces_empty))
  cat(sprintf("    uniform    %d of 6 faces one colour (whatever colour)\n",
              fn$uniform))
  cat(sprintf("    edges      %d of 12 paired\n", fn$edges))
  cat("\n")
}

## ---- what they have in common --------------------------------------------
##
## The point of the table. If the near cubes carry a signature the far ones do
## not, a solver can test for it before committing to the schedule. If they do
## not, that is the finding too, and says the distance has to come from a prune
## table rather than from counting pieces.
cat("== what the findings have in common =====================\n\n")

grab <- function(f) vapply(found, f, numeric(1))
tab <- data.frame(
  seed       = grab(function(h) h$seed),
  depth      = grab(function(h) h$depth),
  outer      = grab(function(h) h$n_outer),
  inner      = grab(function(h) h$n_inner),
  d_red      = grab(function(h) h$d_red),
  early      = grab(function(h) h$early),
  schedule   = grab(function(h) h$best_cost),
  overspend  = grab(function(h) h$overspend),
  c_sum      = grab(function(h) h$f_given$centre_sum),
  c_faces    = grab(function(h) h$f_given$faces_done),
  c_max      = grab(function(h) h$f_given$centre_max),
  unif       = grab(function(h) h$f_given$uniform),
  edges      = grab(function(h) h$f_given$edges))
print(tab, row.names = FALSE)

cat("\n  ranges over the findings:\n")
for (col in c("depth", "outer", "inner", "d_red", "early", "schedule",
              "c_sum", "c_faces", "c_max", "unif", "edges")) {
  v <- tab[[col]]
  cat(sprintf("    %-10s %2d .. %-3d  mean %5.1f\n", col, min(v), max(v), mean(v)))
}


## ---- the rule, and how strong it is --------------------------------------
##
## The twenty findings above are the interesting cubes only, which cannot say
## how often a counter fires on cubes that are NOT interesting. That is what
## this pass measures: the same two counters over a plain sweep of random
## cubes, scored against whether a shortcut was there.
##
## Measured over 1500 cubes at depths 6..12:
##
##     base rate                       19% of cubes have a shortcut
##     edges >= 8                      55%, firing on 14% of cubes
##     unif >= 2                       48%, firing on 16%
##     unif >= 2 AND edges >= 8        64%, firing on 11%
##
## The threshold is a step, not a slope. By paired edges alone:
##
##     0..7 paired    7..21%   -- noise around the base rate
##     8 paired          49%   -- the step
##     12 paired         85%
##
## Eight paired out of twelve leaves four unpaired, which is one slice; that
## is repaired in a couple of moves, and a reduced state is correspondingly
## close. Seven or fewer means two slices are scattered, and no short word
## puts them back.
##
## WHAT THIS DOES NOT SHOW, and it matters before anything is built on it:
## `early` is measured along the inverse scramble, which is not an arbitrary
## path. It ends at a solved cube, and a solved cube is reduced -- so the ray
## being searched is one guaranteed to arrive. `d_red` is therefore the first
## reduced state ALONG THAT RAY, not the nearest reduced state in the group,
## and the two are not the same quantity. Symmetry does not close the gap:
## distance to reduced is indeed a property of the position and not of its
## history, but `early = depth - d_red` is not that distance -- it subtracts
## the scramble depth, which is not a property of the position at all.
##
## So what is confirmed here is "a cube with eight edges paired tends to have
## a reduced state early on its way home". What a solver needs is "a cube with
## eight edges paired tends to have a reduced state nearby, full stop" -- it
## sees a position and no history. The second may well follow from the first,
## but it has not been measured, and the honest place to settle it is a search
## outward from the position itself. Until then this is a lead, not a test.
cat("\n== the rule, over plain random cubes =====================\n\n")

RULE_N <- 600L
rows <- vector("list", RULE_N)
for (i in seq_len(RULE_N)) {
  d <- 6L + (i %% 7L)
  set.seed(90000L + i * 1000L + d)
  scramble <- sample(nm, d, replace = TRUE)
  st <- replay(id, scramble)
  back <- invert(scramble)
  s <- st; dr <- NA_integer_
  for (j in seq_along(back)) {
    s <- s[mv[[back[j]]]]
    if (cube_is_reduced(s)) { dr <- j; break }
  }
  if (is.na(dr)) next
  f <- features(st)
  rows[[i]] <- c(unif = f$uniform, edges = f$edges, early = d - dr)
}
rows <- Filter(Negate(is.null), rows)
sweep <- as.data.frame(do.call(rbind, rows))
hit <- sweep$early >= 3L

cat(sprintf("  %d cubes, depths 6..12; %.0f%% have a shortcut at all\n\n",
            nrow(sweep), 100 * mean(hit)))
cat(sprintf("  %-26s %-14s %s\n", "test", "fires on", "of those, shortcuts"))
show_rule <- function(name, sel) {
  if (!any(sel)) { cat(sprintf("  %-26s never fires\n", name)); return(invisible()) }
  cat(sprintf("  %-26s %4d (%4.1f%%)   %3.0f%%\n",
              name, sum(sel), 100 * mean(sel), 100 * mean(hit[sel])))
}
show_rule("edges >= 8", sweep$edges >= 8L)
show_rule("edges >= 6", sweep$edges >= 6L)
show_rule("unif >= 2", sweep$unif >= 2L)
show_rule("unif >= 2 and edges >= 8", sweep$unif >= 2L & sweep$edges >= 8L)
cat(sprintf("  %-26s %4d (%4.1f%%)   %3.0f%%   <- the base rate\n",
            "(no test at all)", nrow(sweep), 100, 100 * mean(hit)))

cat("\n  shortcuts by paired edges, each value:\n")
by_edges <- tapply(hit, sweep$edges, function(v) c(n = length(v), p = mean(v)))
for (k in names(by_edges)) {
  v <- by_edges[[k]]
  cat(sprintf("    %2s paired   n=%4d   %3.0f%%%s\n", k, v[["n"]],
              100 * v[["p"]], if (as.integer(k) == 8L) "   <- the step" else ""))
}

cat("\n  Read this as a lead and not as a test: `early` is measured along the\n")
cat("  inverse scramble, a ray that ends at a solved cube and so is certain to\n")
cat("  meet a reduced one. Whether the same counters predict a reduced state\n")
cat("  in ANY direction -- which is what a solver, seeing only a position,\n")
cat("  would need -- is a different measurement and is not made here.\n")
