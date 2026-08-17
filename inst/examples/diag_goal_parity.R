#!/usr/bin/env Rscript
# The parity bit of phase 3's twenty-four goals.
#
# diag_trace_phase3.R prints a state's parity bit next to the goals' parity
# bit, and on the cubes phase 3 fails they disagree -- 0 against 1. Read
# plainly that says the state can never reach a goal, which would explain the
# failure completely.
#
# It says that only if all twenty-four goals carry the same bit. The number it
# is compared against comes from cube_phase3_coord_cpp, which reads
#
#     S.d3.derive(S.goals3[0], g);
#
# -- the FIRST goal, not the nearest one and not all of them. The goals are the
# solved cube in each of its twenty-four orientations, and whole-cube rotations
# are not among phase 3's generators, so nothing so far guarantees they share a
# bit. If some goal carries 0, a state with bit 0 has a goal to aim at and the
# reported disagreement is an artefact of which goal got printed.
#
# So the question this answers is narrow and comes before any measurement of
# which seeds fail: do the twenty-four goals all carry the same parity bit?
#
#   all 1     the disagreement is real. A state with bit 0 must spend a move on
#             Uw2, Rw2 or Fw2 -- measured, those three flip the bit and the
#             other fourteen do not -- before any goal is reachable at all.
#
#   mixed     the disagreement is an artefact. goal_parity_bit is comparing
#             against the wrong goal, and the reason seeds 8 and 12 fail is
#             somewhere else.
#
# Run with:  Rscript inst/examples/diag_goal_parity.R

library(cayleyR)

N <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

bit_of <- function(s) cayleyR:::cube_phase3_coord_cpp(s)$parity_bit

# The goals, built the way build_spec4() builds them: close the solved cube
# under whole-cube rotations. Closing it here rather than reaching into the
# solver keeps this script independent of the thing it is checking -- if the
# closure does not come out at twenty-four, that is itself worth knowing.
#
# A whole-cube rotation is all three layers of an axis turning together, and
# these are the two the solver uses, copied from build_spec4():
#
#     rot_x = L' 1x 2x R        rot_y = D' 1y 2y U
#
# Not "1x" on its own. That is a single inner layer, and closing the cube under
# the twelve single-layer turns generates the puzzle group rather than the
# twenty-four orientations -- 13, 127, 1195, 11014 and climbing by a factor of
# nine a round, which is how the mistake announced itself.
rot_x <- strsplit("L' 1x 2x R", " +")[[1]]
rot_y <- strsplit("D' 1y 2y U", " +")[[1]]
rots <- list(x = rot_x, y = rot_y)

key <- function(s) paste(s, collapse = ",")

solved <- cube_identity(N)
seen <- list(solved)
keys <- key(solved)
frontier <- list(solved)

cat("closing the solved cube under whole-cube rotations\n")
cat("  x: ", paste(rot_x, collapse = " "),
    "    y: ", paste(rot_y, collapse = " "), "\n", sep = "")
flush.console()

round <- 0L
repeat {
  round <- round + 1L
  nxt <- list()
  for (s in frontier) {
    for (r in rots) {
      t <- s
      for (m in r) t <- t[mv[[m]]]
      k <- key(t)
      if (!(k %in% keys)) {
        keys <- c(keys, k)
        seen[[length(seen) + 1L]] <- t
        nxt[[length(nxt) + 1L]] <- t
      }
    }
  }
  cat(sprintf("  round %d: %d new, %d total\n", round, length(nxt),
              length(seen)))
  flush.console()
  if (!length(nxt)) break
  # Twenty-four is the whole orbit; anything past it means the moves being
  # closed over are not rotations, and the run should stop saying so rather
  # than growing until it fills memory.
  if (length(seen) > 24L) {
    stop("the closure passed 24 states -- these are not whole-cube rotations",
         call. = FALSE)
  }
  frontier <- nxt
}

cat("orientations reached : ", length(seen), "\n", sep = "")
if (length(seen) != 24L) {
  cat("\nNOTE: the closure did not come out at 24. build_spec4() throws if it\n")
  cat("does not, so either the rotation names picked out above are not the\n")
  cat("whole set or something is wrong at a level below this question.\n")
}

# One call into the solver per goal, and the first one pays for S.init() --
# building the specs and the goal set. That is the slow part of this script, so
# it says where it is rather than sitting silent through it.
cat("\nreading the parity bit of each goal (the first call builds the solver)\n")
flush.console()
bits <- integer(length(seen))
for (i in seq_along(seen)) {
  bits[[i]] <- bit_of(seen[[i]])
  cat(sprintf("  goal %2d/%d: bit %d\n", i, length(seen), bits[[i]]))
  flush.console()
}

cat("goals with bit 0     : ", sum(bits == 0L), "\n", sep = "")
cat("goals with bit 1     : ", sum(bits == 1L), "\n", sep = "")
cat("solved cube's bit    : ", bit_of(solved), "\n", sep = "")

cat("\n")
if (length(unique(bits)) == 1L) {
  cat("All goals carry the same parity bit.\n\n")
  cat("So the disagreement diag_trace_phase3.R reports is real: a state whose\n")
  cat("bit differs from this one cannot reach ANY goal without first flipping\n")
  cat("it, and only Uw2, Rw2 and Fw2 do that. Phase 3 is being handed states\n")
  cat("that owe a move before they can begin, which is one more level of\n")
  cat("depth against a prune table that is already weak.\n")
} else {
  cat("The goals carry BOTH parity bits.\n\n")
  cat("So the disagreement diag_trace_phase3.R reports is an artefact: it\n")
  cat("compares against goals3[0] alone, and a state with the other bit has\n")
  cat("goals of its own to reach. cube_phase3_coord_cpp should report the bit\n")
  cat("of the NEAREST goal -- the one `nearest_goal` already names -- and the\n")
  cat("reason seeds 8 and 12 fail has to be looked for elsewhere.\n")
}
