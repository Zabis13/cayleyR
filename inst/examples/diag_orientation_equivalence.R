# Are the four orientations the same problem wearing four hats?
#
# The cascade tries phase 3 from four whole-cube rotations -- (-), 1y, 1x, 1z --
# and on a measured cube spent 95 of 127 seconds proving that two of them were
# not the one. That cost is only unavoidable if the four are genuinely different
# problems. If they are the same problem rotated, then a canonical form exists,
# all four collapse to one search, and the 95 seconds are an artefact of not
# having written it.
#
# The question has a sharp test, and it is not a correlation: take a rotation
# whose phase 3 succeeded, carry its solution across to another rotation by
# conjugation, and see whether it solves there too. If it does, the orientations
# are one problem and canonicalisation is possible. If it does not, they are
# four problems, no canonical form can exist, and the sweep cannot be removed --
# only ordered better.
#
# There is a reason to expect the second answer, which is worth stating before
# the measurement rather than after, so this script cannot be accused of finding
# what it went looking for. Phase 3 does not start from the rotated cube. It
# starts from `apply_path(turned, p12)` -- the cube after phases 1 and 2 have
# been re-run on the rotated state. Those phases are searches; they return their
# own first solution, and for a rotated cube that solution is a different word,
# not the rotation of the original one. So the four states handed to phase 3 are
# four different positions rather than four views of one, and their phase-3
# distances have no reason to agree. The measured log agrees: on one cube (-)
# and 1y did not finish at depth 14 while 1x finished at 13.
#
# This script separates the two candidate causes, which the cascade conflates:
#
#   A. the ROTATED cube, before phases 1 and 2 -- four views of one position,
#      trivially equivalent, and this is checked as a control. If this leg ever
#      fails, the rotation words themselves are wrong and nothing else in the
#      script means anything.
#
#   B. the HANDED state, after phases 1 and 2 -- what phase 3 actually searches.
#      This is the leg the answer depends on.
#
# Usage:
#   Rscript inst/examples/diag_orientation_equivalence.R [n_cubes] [budget]

suppressMessages(library(cayleyR))

args <- commandArgs(trailingOnly = TRUE)
n_cubes <- if (length(args) >= 1) as.integer(args[[1]]) else 4L
budget <- if (length(args) >= 2) as.numeric(args[[2]]) else 5e7
scramble_len <- 20L
orientations <- c("", "1y", "1x", "1z")
table_dir <- "/mnt/Data2/DS_projects/phase3"

hr <- function(t) cat(sprintf("\n== %s %s\n", t,
                              strrep("-", max(0, 58 - nchar(t)))))
fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

moves <- cube_moves(4)
names(moves) <- cube_move_names(4)
replay <- function(s, p) { for (m in p) s <- s[moves[[m]]]; s }

# The cascade's own inverter, so this script cannot disagree with it about what
# undoing a rotation means.
inv <- cayleyR:::.cube4_invert_moves

hr("setup")
cat(sprintf("cubes        : %d, scrambled %d moves\n", n_cubes, scramble_len))
cat(sprintf("budget       : %s nodes\n", fmt(budget)))
cat(sprintf("orientations : %s\n",
            paste(ifelse(nzchar(orientations), orientations, "(-)"),
                  collapse = ", ")))

set.seed(20260816)
states <- lapply(seq_len(n_cubes), function(i) {
  replay(cube_identity(4), sample(names(moves), scramble_len, TRUE))
})

hr("the tables")
for (ph in 1:3) {
  cand <- Sys.glob(file.path(table_dir, sprintf("phase%d_d*.bin", ph)))
  if (length(cand) == 0) {
    cat(sprintf("  phase %d: no file in %s -- fills lazily\n", ph, table_dir))
    next
  }
  d <- as.integer(sub("^.*_d([0-9]+)\\.bin$", "\\1", cand))
  cand <- cand[order(d, decreasing = TRUE)]
  ld <- cayleyR:::cube_kociemba4_load_phase_cpp(cand[1], ph)
  if (isTRUE(ld$ok)) {
    cat(sprintf("  phase %d: %s, depth %d, %s entries\n",
                ph, basename(cand[1]), ld$built_depth, fmt(ld$n_writes)))
  } else {
    cat(sprintf("  phase %d: %s refused (%s)\n", ph, basename(cand[1]),
                ld$reason))
  }
}

# ---- Leg A: the control ----------------------------------------------------
#
# Rotate a cube, undo the rotation, and the cube must be back. This is not the
# question -- it is the check that the rotation words mean what the rest of the
# script assumes, and it costs microseconds. A failure here would make every
# later "not equivalent" verdict meaningless, since it would be measuring a
# broken rotation rather than a real difference between positions.
hr("leg A: are the rotation words honest rotations")
okA <- TRUE
for (i in seq_len(n_cubes)) {
  for (rot in orientations) {
    if (!nzchar(rot)) next
    w <- cube_expand_word(rot, 4L)
    back <- replay(replay(states[[i]], w), inv(w))
    if (!identical(back, states[[i]])) {
      cat(sprintf("  cube %d rot %-3s : rotation does not undo -- STOP\n", i, rot))
      okA <- FALSE
    }
  }
}
cat(if (okA) "  all rotations undo cleanly\n" else
    "  BROKEN -- later results cannot be trusted\n")

# ---- What phase 3 is handed, per orientation -------------------------------
prep_one <- function(state, rot) {
  rot_word <- if (nzchar(rot)) cube_expand_word(rot, 4L) else character(0)
  turned <- replay(state, rot_word)
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(turned, upto_phase = 2L,
                                              node_budget = budget)
  if (!length(p12)) return(NULL)
  handed <- replay(turned, p12)
  if (!isTRUE(cayleyR:::cube_at_phase_goal_cpp(handed, 2L))) return(NULL)
  list(rot = rot, rot_word = rot_word, p12 = p12, handed = handed)
}

solve3 <- function(handed) {
  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(handed, node_budget = budget,
                                           use_exact_centres = TRUE,
                                           progress_every = 0)
  # The solution comes back in `path`, not `moves`. Read once from the C++ side
  # rather than assumed: a wrong name here does not error, it yields an empty
  # word, and an empty word transfers to nothing and "proves" the orientations
  # are different problems -- the exact conclusion this script exists to test.
  # So a found solution with no moves is treated as a fault and stops the run.
  sol <- r$path
  if (isTRUE(r$found) && !length(sol)) {
    stop("phase 3 reported found with an empty path -- the field name is wrong",
         call. = FALSE)
  }
  list(found = isTRUE(r$found), nodes = r$nodes, sol = sol,
       depth = if (isTRUE(r$found)) length(sol) else NA_integer_,
       secs = proc.time()[["elapsed"]] - t0)
}

# ---- Leg B: the question ---------------------------------------------------
#
# Two readings, and they answer different halves of it.
#
#   B1. Do the four handed states differ at all? If phases 1 and 2 happened to
#       return rotations of one word, the handed states would be rotations of
#       one position and the whole issue would be notational. Compared by
#       carrying each handed state back through its own rotation, which puts all
#       four in the same frame where they can be compared at all.
#
#   B2. Does a working solution transfer? The direct test. Conjugate the winner's
#       phase-3 solution into another orientation's frame and check whether it
#       reaches that orientation's goal.
hr("leg B: what phase 3 is handed")

rows <- list()
for (i in seq_len(n_cubes)) {
  cat(sprintf("\n  cube %d\n", i))
  preps <- list()
  for (rot in orientations) {
    p <- prep_one(states[[i]], rot)
    lbl <- if (nzchar(rot)) rot else "(-)"
    if (is.null(p)) {
      cat(sprintf("    %-4s phase 2 did not hand over\n", lbl))
      next
    }
    preps[[lbl]] <- p
  }
  if (!length(preps)) next

  # B1: are the handed states the same position seen from four sides?
  # Each is carried back through its own rotation into the unrotated frame.
  in_common_frame <- lapply(preps, function(p) replay(p$handed, inv(p$rot_word)))
  same <- vapply(in_common_frame, function(s) identical(s, in_common_frame[[1]]),
                 logical(1))
  cat(sprintf("    handed states, compared in one frame : %s\n",
              if (all(same)) "IDENTICAL -- one position"
              else sprintf("%d distinct of %d -- different positions",
                           length(unique(lapply(in_common_frame, paste,
                                                collapse = ","))),
                           length(in_common_frame))))
  cat(sprintf("    phase 1+2 word lengths               : %s\n",
              paste(sprintf("%s=%d", names(preps),
                            vapply(preps, function(p) length(p$p12), 1L)),
                    collapse = "  ")))

  # The truth for each, which is what the transfer test needs a winner from.
  cat(sprintf("    %-5s %10s %14s %7s\n", "rot", "outcome", "nodes", "depth"))
  sols <- list()
  for (lbl in names(preps)) {
    tr <- solve3(preps[[lbl]]$handed)
    sols[[lbl]] <- tr
    cat(sprintf("    %-5s %10s %14s %7s\n", lbl,
                if (tr$found) "found" else "exhausted", fmt(tr$nodes),
                if (tr$found) as.character(tr$depth) else "-"))
    flush.console()
    rows[[length(rows) + 1L]] <- data.frame(
      cube = i, rot = lbl, found = tr$found, nodes = tr$nodes,
      depth = tr$depth, secs = tr$secs, stringsAsFactors = FALSE)
  }

  # B2: transfer. Take a rotation that finished and try its solution elsewhere.
  winners <- names(sols)[vapply(sols, function(s) isTRUE(s$found), logical(1))]
  if (!length(winners)) {
    cat("    no orientation finished -- nothing to transfer\n")
    next
  }
  w <- winners[[1]]
  cat(sprintf("    transferring %s's %d-move solution:\n", w, sols[[w]]$depth))
  for (lbl in names(preps)) {
    if (identical(lbl, w)) next
    # Carry the solution into `lbl`'s frame: undo the winner's rotation, apply
    # the winner's moves, redo `lbl`'s rotation. If the orientations were one
    # problem, this word solves phase 3 there as well.
    conj <- c(inv(preps[[w]]$rot_word), sols[[w]]$sol, preps[[lbl]]$rot_word)
    reached <- replay(preps[[lbl]]$handed, conj)
    ok <- isTRUE(cayleyR:::cube_at_phase_goal_cpp(reached, 3L))
    cat(sprintf("      -> %-4s %s\n", lbl,
                if (ok) "SOLVES -- same problem" else "does not solve"))
  }
}

df <- do.call(rbind, rows)

hr("the verdict")
if (is.null(df) || !nrow(df)) {
  cat("  nothing measured\n")
} else {
  fin <- df[df$found, ]
  cat(sprintf("  orientations that finished : %d of %d\n", nrow(fin), nrow(df)))
  if (nrow(fin)) {
    cat("\n  phase-3 depth per cube (a canonical form would make these equal):\n")
    for (i in unique(df$cube)) {
      sub <- df[df$cube == i, ]
      cat(sprintf("    cube %d : %s\n", i,
                  paste(sprintf("%s=%s", sub$rot,
                                ifelse(is.na(sub$depth), ">budget",
                                       as.character(sub$depth))),
                        collapse = "  ")))
    }
  }
}

hr("how to read this")
cat("
  If leg B says the handed states are IDENTICAL in one frame and the transfer
  SOLVES, then the four orientations are one problem, a canonical form exists,
  and the sweep should be replaced by canonicalisation -- the 95 seconds are
  removable in full.

  If the handed states are different positions and the transfer does not solve,
  then phases 1 and 2 have already sent the four rotations to genuinely
  different places. No canonical form can collapse them, because there is
  nothing common left to canonicalise: the sweep is searching four distinct
  problems and taking the first that yields. Then the only gains available are
  ordering ones -- interleaving by depth, or a predictor -- and the depth table
  above says how much there is to win.

  Note which way the depths fall. Equal depths across orientations would be the
  signature of one problem; a spread of several moves is the signature of four.
")
