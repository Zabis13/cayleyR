#!/usr/bin/env Rscript
# Phase 3 across a sample of cubes: do the ones it fails differ systematically,
# or are they the tail of one distribution?
#
# Two explanations were ruled out before this was written, and neither needs
# measuring again:
#
#   the parity bit     Not it. The twenty-four goals do not share a bit --
#                      twelve carry 0 and twelve carry 1 (diag_goal_parity.R),
#                      because Uw2, Rw2 and Fw2 flip it. Every state has twelve
#                      goals it can reach. The 0-against-1 disagreement that
#                      cube_phase3_coord_cpp used to print came from comparing
#                      against goals3[0] alone.
#
#   an empty goal set  Not it, for the same reason.
#
# What is left is the prune table. On seed 8 it reported a bound of 0 somewhere
# in a search that never solved anything, while the start state's own bound was
# 6 -- a bound of 0 that is not a solved cube is another state's entry in the
# same slot. And the branch it rated closest, nine moves long, did not pair a
# single wing: 0 paired at the start, 0 at the end, while the centres swung
# 10 -> 14 -> 8 -> 14. A heuristic doing its job does not lead the search
# somewhere the phase's own measure never improves.
#
# So this measures, per cube:
#
#   paired_before / paired_after   wings paired handed to phase 3, and at the
#                                  end of the branch it rated closest. On a
#                                  search making progress these differ.
#
#   bound_start                    what the table says the distance is from the
#                                  state phase 3 was handed.
#
#   best_bound                     the smallest bound it saw anywhere. Below
#                                  bound_start is normal -- the search moves
#                                  closer. At 0 without a solution it is a
#                                  collision, and that is the column to count.
#
#   wing_mismatch                  wings out of place against the nearest goal,
#                                  which is the size of the job rather than the
#                                  table's opinion of it.
#
# The question the table answers: on the cubes that fail, is best_bound driven
# to 0 by collisions more often than on the cubes that succeed? If yes, the
# fault is the table's width and the fix is measured in slots. If the failing
# cubes instead show a plainly bigger job -- more wings out of place, further
# to go -- then phases 1 and 2 are handing over worse cubes and the fix is
# upstream.
#
# Run with:  Rscript inst/examples/diag_phase3_seeds.R
#            Rscript inst/examples/diag_phase3_seeds.R 24 1e6   # seeds, budget
#
# It prints a row as each cube finishes, so an interrupted run still leaves a
# usable table.

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)

N        <- 4L
n_seeds  <- if (length(args) >= 1L) as.integer(args[[1]]) else 24L
budget   <- if (length(args) >= 2L) as.numeric(args[[2]]) else 1e6

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# ---- the pairing measure --------------------------------------------------

# From diag_stuck_seeds.R. Wings occupy slots 9:32 of the 56-piece vector and
# are numbered from the same offset, so the 8 comes off before the geometry
# vectors -- which are indexed from zero -- are touched.
.wing_geom <- cayleyR:::cube_wing_geometry_cpp()
n_paired <- function(state) {
  perm <- cayleyR:::cube_to_pieces4_cpp(state)$perm
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

local({
  chk <- n_paired(cube_identity(N))
  if (chk != 12L) {
    stop("n_paired() reports ", chk, " on a solved cube, expected 12",
         call. = FALSE)
  }
})

scramble_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  list(state = s, word = w)
}

# ---- the run --------------------------------------------------------------

cat("seeds  : 1..", n_seeds, "\n", sep = "")
cat("budget : ", format(budget, scientific = FALSE, big.mark = ","),
    " nodes per cube\n\n", sep = "")

cat(sprintf("%4s %-9s %5s %5s %6s %6s %6s %6s %5s %s\n",
            "seed", "outcome", "pair0", "pair1", "bound0", "best", "wmiss",
            "nodes", "p2ok", "moves"))
flush.console()

rows <- list()

for (sd in seq_len(n_seeds)) {
  sc <- scramble_state(sd)

  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(sc$state, upto_phase = 2L,
                                              node_budget = budget)
  s2 <- replay(sc$state, p12)
  p2ok <- isTRUE(cayleyR:::cube_at_phase_goal_cpp(s2, 2L))

  co <- cayleyR:::cube_phase3_coord_cpp(s2)
  r3 <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = budget)

  # Where the branch it rated closest ended up. On success this is the solved
  # cube; on failure it is where the effort went.
  s3 <- replay(s2, r3$path)

  row <- data.frame(
    seed        = sd,
    outcome     = r3$outcome,
    found       = isTRUE(r3$found),
    paired_before = n_paired(s2),
    paired_after  = n_paired(s3),
    bound_start = co$prune_bound,
    best_bound  = r3$best_bound,
    wing_mismatch = co$wing_mismatch,
    nodes       = r3$nodes,
    phase2_ok   = p2ok,
    n_moves     = length(r3$path),
    stringsAsFactors = FALSE)
  rows[[length(rows) + 1L]] <- row

  cat(sprintf("%4d %-9s %5d %5d %6d %6d %6d %6s %5s %d\n",
              sd, r3$outcome, row$paired_before, row$paired_after,
              row$bound_start, row$best_bound, row$wing_mismatch,
              format(r3$nodes, scientific = FALSE, big.mark = ","),
              if (p2ok) "yes" else "NO", row$n_moves))
  flush.console()
}

tab <- do.call(rbind, rows)

# ---- what the table says --------------------------------------------------

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

hr("solved and unsolved")

ok <- tab$found
cat(sprintf("solved   : %d of %d\n", sum(ok), nrow(tab)))
cat(sprintf("failed   : %d  (seeds %s)\n", sum(!ok),
            paste(tab$seed[!ok], collapse = ", ")))

if (any(!tab$phase2_ok)) {
  cat(sprintf("\nphase 2 did not arrive on seeds %s -- those rows say nothing\n",
              paste(tab$seed[!tab$phase2_ok], collapse = ", ")))
  cat("about phase 3, since it was handed a cube outside its domain.\n")
}

if (sum(ok) > 0L && sum(!ok) > 0L) {
  a <- tab[ok, ]
  b <- tab[!ok, ]

  hr("solved against failed")

  cmp <- function(nm, va, vb) {
    cat(sprintf("  %-16s %8.2f %8.2f\n", nm, mean(va), mean(vb)))
  }
  cat(sprintf("  %-16s %8s %8s\n", "", "solved", "failed"))
  cmp("wings paired in", a$paired_before, b$paired_before)
  cmp("wings paired out", a$paired_after, b$paired_after)
  cmp("wing mismatch", a$wing_mismatch, b$wing_mismatch)
  cmp("bound at start", a$bound_start, b$bound_start)
  cmp("best bound", a$best_bound, b$best_bound)

  hr("the collision count")

  # A bound of 0 on a cube that was never solved cannot be this state's own
  # distance. It is another state's entry, read out of the same slot.
  ph <- sum(b$best_bound == 0L)
  cat(sprintf("failed cubes whose best bound reached 0 : %d of %d\n",
              ph, nrow(b)))
  cat("A bound of 0 without a solution is another state's entry in the same\n")
  cat("slot -- the table's width, not the cube's distance.\n")

  hr("did the search pair anything")

  moved <- b$paired_after != b$paired_before
  cat(sprintf("failed cubes where the closest branch changed the pairing count: %d of %d\n",
              sum(moved), nrow(b)))
  cat("\nIf this is near zero the table is not steering towards pairing at\n")
  cat("all, and a wider table is the thing to measure next. If the branches\n")
  cat("do pair wings and still fail, the phase is running out of depth\n")
  cat("rather than being misled, and the budget is the thing to measure.\n")
}

out <- file.path(tempdir(), "diag_phase3_seeds.csv")
write.csv(tab, out, row.names = FALSE)
cat("\nrows written to ", out, "\n", sep = "")
