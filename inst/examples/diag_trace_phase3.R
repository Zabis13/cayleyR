#!/usr/bin/env Rscript
# Phase 3, move by move, on one cube.
#
# diag_trace_phases.R follows phases 1 and 2 and stops where phase 3 begins,
# reporting only its outcome. This picks the cube up there and walks phase 3
# the same way: after every move, what the cube looks like by the measures that
# phase is supposed to be improving.
#
# Phase 3's job is pairing. It takes a cube whose centres are on their axes and
# has to bring the two halves of each of the twelve dedges together without
# taking the centres apart again. So the two measures to watch are wings paired
# -- which must climb to twelve -- and centres home, which must not fall.
#
# The trace comes from two places and the difference matters:
#
#   path          the moves, expanded to quarter turns, replayed here in R
#                 against the 96 stickers. Sticker measures (centres home,
#                 faces built) can only be computed this way -- the search's
#                 own representation numbers centres by colour and cannot tell
#                 the four centres of a face apart.
#
#   states_perm   the states the search itself passed through, one row per
#                 generator. Exact, but piece-level only.
#
# Replaying in R and comparing the final state against the search's own last
# row is the check worth having: if they disagree, the generator expansion is
# wrong, and every move name this solver has ever reported is suspect.
#
# Run with:  Rscript inst/examples/diag_trace_phase3.R
#            Rscript inst/examples/diag_trace_phase3.R 8      # a seed
#            Rscript inst/examples/diag_trace_phase3.R "U' 1y' 2z 1x' F F"

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

# Both sticker measures are the ones diag_trace_phases.R uses, so the two
# traces can be read against each other without allowing for a change of
# definition.

centres_home <- function(s) {
  n <- 0L
  for (f in 0:5) {
    idx <- f * 16L + c(5L, 6L, 9L, 10L) + 1L
    n <- n + sum((s[idx] - 1L) %/% 16L == f)
  }
  n
}

faces_built <- function(s) {
  sum(vapply(0:5, function(f) {
    idx <- f * 16L + c(5L, 6L, 9L, 10L) + 1L
    length(unique((s[idx] - 1L) %/% 16L)) == 1L
  }, logical(1)))
}

# Pairing, read as pieces rather than stickers. This is the measure from
# diag_stuck_seeds.R: a dedge is paired when the two wings that belong together
# sit in two slots that themselves form a dedge. Both halves come from the
# geometry rather than from the sticker colours, which is what makes it right
# on a cube whose centres have been turned away from home.
.wing_geom <- cayleyR:::cube_wing_geometry_cpp()
n_paired <- function(state) {
  perm <- cayleyR:::cube_to_pieces4_cpp(state)$perm
  # Wings occupy slots 9:32 of the 56-piece vector, and the values there are
  # numbered from the same offset -- 8..31, not 0..23. The geometry vectors are
  # indexed 0..23, so the offset comes off before either is used. Leaving it on
  # runs off the end of `partner` on the very first wing.
  w <- perm[9:32] - 8L
  paired <- 0L
  seen <- logical(24)
  for (slot in seq_len(24)) {
    if (seen[[slot]]) next
    piece <- w[[slot]]
    mate_piece <- .wing_geom$partner[[piece + 1L]]
    mate_slot <- which(w == mate_piece)
    if (!length(mate_slot)) next
    seen[[slot]] <- TRUE
    seen[[mate_slot[[1]]]] <- TRUE
    if (.wing_geom$dedge[[slot]] == .wing_geom$dedge[[mate_slot[[1]]]]) {
      paired <- paired + 1L
    }
  }
  paired
}

# Checked rather than trusted, for the reason recorded in diag_stuck_seeds.R:
# an earlier version of this crossed pieces with stickers and returned 2 for
# every cube, including a solved one.
local({
  chk <- n_paired(cube_identity(N))
  if (chk != 12L) {
    stop("n_paired() reports ", chk, " on a solved cube, expected 12",
         call. = FALSE)
  }
})

# ---- pick a cube ----------------------------------------------------------

scramble_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  list(state = s, word = w)
}

arg <- if (length(args) >= 1L && nzchar(args[[1]])) trimws(args[[1]]) else "8"

if (grepl("^[0-9]+$", arg)) {
  sc <- scramble_state(as.integer(arg))
  word <- sc$word
  state <- sc$state
  origin <- paste0("seed ", arg)
} else {
  word <- strsplit(arg, " +")[[1]]
  state <- replay(cube_identity(N), word)
  origin <- "the scramble given on the command line"
}

hr("the cube")
cat("from     : ", origin, "\n", sep = "")
cat("scramble : ", paste(word, collapse = " "), "  (", length(word),
    " moves)\n", sep = "")

cat(sprintf("at the start: centres home %d/24, faces built %d/6, wings paired %d/12\n",
            centres_home(state), faces_built(state), n_paired(state)))

# ---- phases 1 and 2, to get to phase 3 ------------------------------------

# Phase 3 does not start from the scramble. It starts from wherever phases 1
# and 2 left the cube, so those have to run first even though the trace below
# is not about them.

p1 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 1L,
                                           node_budget = node_budget)
p12 <- cayleyR:::cube_kociemba4_phase12_cpp(state, upto_phase = 2L,
                                            node_budget = node_budget)

hr("phases 1 and 2")
cat(sprintf("phase 1  : %2d moves\n", length(p1)))
cat(sprintf("phase 2  : %2d moves\n", max(0L, length(p12) - length(p1))))

s2 <- replay(state, p12)
cat(sprintf("\nhanded to phase 3: centres home %d/24, faces built %d/6, wings paired %d/12\n",
            centres_home(s2), faces_built(s2), n_paired(s2)))
cat("at phase 2 goal  : ", yn(cayleyR:::cube_at_phase_goal_cpp(s2, 2L)),
    "\n", sep = "")

# If phase 2 did not arrive there is no point reading phase 3's trace: it was
# handed a cube it was never meant to see, and whatever it does with it says
# nothing about phase 3.
if (!isTRUE(cayleyR:::cube_at_phase_goal_cpp(s2, 2L))) {
  cat("\nPhase 2 did not reach its goal, so phase 3 is starting from a cube\n")
  cat("outside its domain. Read the trace below as a description of that,\n")
  cat("not as a verdict on phase 3.\n")
}

# ---- phase 3 --------------------------------------------------------------

hr("phase 3")

r3 <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = node_budget)

cat(sprintf("outcome  : %s after %s nodes\n", r3$outcome,
            format(r3$nodes, scientific = FALSE, big.mark = ",")))
cat(sprintf("prune    : %.1f%% of %s lookups cut, mean bound %.2f\n",
            100 * r3$cut_ratio,
            format(r3$prune_lookups, scientific = FALSE, big.mark = ","),
            r3$mean_bound))

cat(sprintf("best bound: %d   (smallest distance the table gave any node seen)\n",
            r3$best_bound))

if (!isTRUE(r3$found)) {
  co <- cayleyR:::cube_phase3_coord_cpp(s2)
  cat("\nphase 3 coordinate of the state it was handed:\n")
  for (nm in names(co)) {
    v <- co[[nm]]
    if (length(v) == 1L) cat(sprintf("  %-18s %s\n", nm, format(v)))
  }
}

# ---- the trace ------------------------------------------------------------

hr(if (isTRUE(r3$found)) "phase 3, move by move"
   else "phase 3, the closest branch, move by move")

cat("Phase 3 pairs the wings. Wings paired must climb to 12; centres home\n")
cat("must not fall on the way. A move that pairs a dedge by taking centres\n")
cat("off their faces is phase 3 undoing phases 1 and 2.\n")

if (!isTRUE(r3$found)) {
  cat("\nThis run found no solution, so the moves below are not one. They are\n")
  cat("the branch the prune table rated closest of everything the search\n")
  cat("looked at -- where the effort went. The table collides, so a small\n")
  cat("bound is its opinion and not a measured distance. What the trace can\n")
  cat("still show is whether the search was working on the right thing:\n")
  cat("wings climbing while centres hold, or centres being taken apart.\n")
}
cat("\n")

cat(sprintf("  %-4s %-8s %-14s %-13s %-14s\n",
            "step", "move", "centres home", "faces built", "wings paired"))

cur <- s2
cat(sprintf("  %-4s %-8s %-14d %-13d %-14d\n", "-", "-",
            centres_home(cur), faces_built(cur), n_paired(cur)))

for (k in seq_along(r3$path)) {
  cur <- cur[mv[[r3$path[k]]]]
  cat(sprintf("  %-4d %-8s %-14d %-13d %-14d\n", k, r3$path[k],
              centres_home(cur), faces_built(cur), n_paired(cur)))
}

# ---- the states themselves ------------------------------------------------

# The measures above are summaries, and a summary that does not move -- as the
# pairing count does not, on the cubes this phase fails -- says nothing about
# whether the cube moved. These are the states under those summaries, in the
# search's own representation, one row per generator.
#
# Wings are printed as the piece sitting in each of the 24 slots. Centres are
# printed as colours, which is all the search knows them by. Corners are left
# out: phase 3 does not move them.

hr("the states, one row per generator")

n_state_rows <- if (is.null(dim(r3$states_perm))) 0L else nrow(r3$states_perm)
if (n_state_rows > 0L) {
  cat("wings: the piece in each of the 24 wing slots\n\n")
  cat(sprintf("  %-4s %-10s %s\n", "step", "generator", "wing slots 1..24"))
  st <- cayleyR:::cube_to_pieces4_cpp(s2)$perm
  cat(sprintf("  %-4s %-10s %s\n", "-", "-",
              paste(sprintf("%2d", st[9:32] - 8L), collapse = " ")))
  for (r in seq_len(n_state_rows)) {
    cat(sprintf("  %-4d %-10s %s\n", r, r3$generators[r],
                paste(sprintf("%2d", r3$states_perm[r, 9:32] - 8L),
                      collapse = " ")))
  }

  cat("\ncentres: the colour in each of the 24 centre slots\n\n")
  cat(sprintf("  %-4s %-10s %s\n", "step", "generator", "centre slots 1..24"))
  cat(sprintf("  %-4s %-10s %s\n", "-", "-",
              paste(sprintf("%2d", st[33:56] - 32L), collapse = " ")))
  for (r in seq_len(n_state_rows)) {
    cat(sprintf("  %-4d %-10s %s\n", r, r3$generators[r],
                paste(sprintf("%2d", r3$states_perm[r, 33:56] - 32L),
                      collapse = " ")))
  }
} else {
  cat("no states recorded.\n")
}

# ---- the check ------------------------------------------------------------

# Two independent accounts of where phase 3 ended up: the one built here by
# replaying the expanded move names against the stickers, and the one the
# search kept for itself. They have to agree.
#
# They are compared as piece permutations because that is the only
# representation both of them have. `states_perm` carries one row per
# generator, so the last row is the end of the phase however many quarter turns
# that generator expanded into.

hr("replay against the search's own states")

n_gen <- length(r3$generators)
n_row <- if (is.null(dim(r3$states_perm))) 0L else nrow(r3$states_perm)

cat(sprintf("generators : %d\n", n_gen))
cat(sprintf("path       : %d quarter turns\n", length(r3$path)))
cat(sprintf("states     : %d rows\n", n_row))

if (n_row != n_gen) {
  cat("\nMISMATCH: one state per generator was expected. The trace and the\n")
  cat("move names are not describing the same run.\n")
} else if (n_row == 0L) {
  if (isTRUE(r3$found)) {
    cat("\nPhase 3 solved this cube without moving.\n")
  } else {
    cat("\nThe search recorded no branch: no node it reached was rated closer\n")
    cat("than the state it started from.\n")
  }
} else {
  mine <- cayleyR:::cube_to_pieces4_cpp(cur)$perm
  theirs <- as.integer(r3$states_perm[n_row, ])
  if (identical(as.integer(mine), theirs)) {
    cat("\nagreed: replaying `path` in R lands on the state the search ended on.\n")
  } else {
    d <- sum(as.integer(mine) != theirs)
    cat(sprintf("\nDISAGREED: %d of %d pieces differ between the replay and the\n",
                d, length(theirs)))
    cat("search's own final state. Either the generator expansion in\n")
    cat("expand_generator_words() is wrong, or the moves it names do not act\n")
    cat("the way cube_moves(4) acts. Every reported solution depends on this.\n")
  }
}

hr(if (isTRUE(r3$found)) "what phase 3 did to the cube"
   else "where the closest branch led")

cat(sprintf("  centres home : %2d handed over -> %2d at the end\n",
            centres_home(s2), centres_home(cur)))
cat(sprintf("  faces built  : %2d handed over -> %2d at the end\n",
            faces_built(s2), faces_built(cur)))
cat(sprintf("  wings paired : %2d handed over -> %2d at the end\n",
            n_paired(s2), n_paired(cur)))

if (isTRUE(r3$found)) {
  cat(sprintf("\nphase 3 took %d quarter turns.\n", length(r3$path)))
} else {
  cat(sprintf("\nthe branch is %d quarter turns long; the cube itself was left\n",
              length(r3$path)))
  cat("where phase 2 put it, since the phase failed.\n")
}
