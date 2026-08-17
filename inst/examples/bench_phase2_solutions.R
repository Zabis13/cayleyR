# Several phase-2 solutions in one orientation, against four orientations.
#
# The cascade gives phase 3 another chance by rotating the whole cube. Measured
# (diag_orientation_equivalence.R), that is dear: the four rotations re-run
# phases 1 and 2 and land in four unrelated positions -- distinct when carried
# back into one frame, and a phase-3 solution found in one solved in none of the
# others, 12 transfers with 0 successes. Three of the four attempts are usually
# paid for whole, at forty seconds each.
#
# Phase 2 costs a tenth of a second. If its second, third and fourth solutions
# hand phase 3 starting points that differ as much as the rotations' do, then
# the same second chance is available at a four-hundredth of the price.
#
# That "if" is the whole question, and it can fail in a way worth naming before
# the run rather than after. Phase 2's solutions come out of one search in
# increasing depth, so consecutive ones may differ by a move or two at the end
# and leave phase 3 in near-identical positions -- in which case they are not
# four chances but one chance measured four times. The distinctness column below
# is what tells the two apart, and it is reported whether or not it flatters the
# idea.
#
# Usage:
#   Rscript inst/examples/bench_phase2_solutions.R [n_cubes] [n_sol] [budget]

suppressMessages(library(cayleyR))

args <- commandArgs(trailingOnly = TRUE)
n_cubes <- if (length(args) >= 1) as.integer(args[[1]]) else 4L
n_sol <- if (length(args) >= 2) as.integer(args[[2]]) else 4L
budget <- if (length(args) >= 3) as.numeric(args[[3]]) else 5e7
scramble_len <- 20L
table_dir <- "/mnt/Data2/DS_projects/phase3"

hr <- function(t) cat(sprintf("\n== %s %s\n", t,
                              strrep("-", max(0, 58 - nchar(t)))))
fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

moves <- cube_moves(4)
names(moves) <- cube_move_names(4)
replay <- function(s, p) { for (m in p) s <- s[moves[[m]]]; s }

hr("setup")
cat(sprintf("cubes        : %d, scrambled %d moves\n", n_cubes, scramble_len))
cat(sprintf("solutions    : up to %d from phase 2, one orientation\n", n_sol))
cat(sprintf("budget       : %s nodes\n", fmt(budget)))

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
  }
}

solve3 <- function(handed) {
  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(handed, node_budget = budget,
                                           use_exact_centres = TRUE,
                                           progress_every = 0)
  list(found = isTRUE(r$found), nodes = r$nodes,
       depth = if (isTRUE(r$found)) length(r$path) else NA_integer_,
       secs = proc.time()[["elapsed"]] - t0)
}

hr("phase 2's solutions, and what phase 3 makes of them")
rows <- list()
for (i in seq_len(n_cubes)) {
  cat(sprintf("\n  cube %d\n", i))

  t0 <- proc.time()[["elapsed"]]
  res <- cayleyR:::cube_kociemba4_phase2_solutions_cpp(states[[i]],
                                                       n_solutions = n_sol,
                                                       node_budget = budget)
  secs12 <- proc.time()[["elapsed"]] - t0

  if (!length(res$solutions)) {
    cat("    phase 2 handed over nothing\n")
    next
  }
  cat(sprintf("    phase 1+2 for all %d solutions : %.2f s\n",
              length(res$solutions), secs12))

  # Each solution's handover, and whether they are really different states.
  handed <- lapply(res$solutions, function(w) {
    replay(replay(states[[i]], res$phase1), w)
  })
  keys <- vapply(handed, function(s) paste(s, collapse = ","), character(1))
  cat(sprintf("    distinct handovers            : %d of %d\n",
              length(unique(keys)), length(keys)))
  cat(sprintf("    phase-2 word lengths          : %s\n",
              paste(vapply(res$solutions, length, 1L), collapse = " ")))

  # Every handover must actually be at phase 2's goal. A collected solution that
  # is not there would send phase 3 searching from a position it cannot pair
  # from, and the node counts below would be measuring nothing.
  atgoal <- vapply(handed, function(s)
    isTRUE(cayleyR:::cube_at_phase_goal_cpp(s, 2L)), logical(1))
  if (!all(atgoal)) {
    cat(sprintf("    NOT AT PHASE-2 GOAL           : %d of %d -- collector is wrong\n",
                sum(!atgoal), length(atgoal)))
  }

  cat(sprintf("    %-4s %10s %14s %7s %8s\n",
              "sol", "outcome", "nodes", "depth", "secs"))
  for (k in seq_along(handed)) {
    if (!atgoal[k]) next
    tr <- solve3(handed[[k]])
    cat(sprintf("    %-4d %10s %14s %7s %8.1f\n", k,
                if (tr$found) "found" else "exhausted", fmt(tr$nodes),
                if (tr$found) as.character(tr$depth) else "-", tr$secs))
    flush.console()
    rows[[length(rows) + 1L]] <- data.frame(
      cube = i, sol = k, found = tr$found, nodes = tr$nodes,
      depth = tr$depth, secs = tr$secs, secs12 = secs12,
      distinct = length(unique(keys)), n = length(keys),
      stringsAsFactors = FALSE)
  }
}

df <- do.call(rbind, rows)

hr("the verdict")
if (is.null(df) || !nrow(df)) {
  cat("  nothing measured\n")
} else {
  cat(sprintf("  solutions that phase 3 finished : %d of %d\n",
              sum(df$found), nrow(df)))
  cat(sprintf("  cubes with at least one finish  : %d of %d\n",
              length(unique(df$cube[df$found])), n_cubes))

  cat("\n  per cube: what a sweep over phase-2 solutions would cost\n")
  cat(sprintf("  %-6s %10s %12s %12s\n",
              "cube", "distinct", "first win", "cheapest"))
  for (i in unique(df$cube)) {
    sub <- df[df$cube == i, ]
    win <- which(sub$found)
    cat(sprintf("  %-6d %6d/%-3d %12s %12s\n", i, sub$distinct[1], sub$n[1],
                if (length(win)) sprintf("%.1f s", sum(sub$secs[1:win[1]]))
                else "none",
                if (length(win)) sprintf("%.1f s", min(sub$secs[win]))
                else "none"))
  }

  cat(sprintf("\n  phase 1+2 for %d solutions : %.2f s median\n",
              n_sol, median(unique(df$secs12))))
  cat("  (four orientations cost four separate phase-1+2 runs, and measured\n")
  cat("   40 s per failed phase 3 -- compare the first-win column against that)\n")
}

hr("how to read this")
cat("
  The distinct column comes first. If phase 2's solutions collapse to one or
  two distinct handovers, this buys less than it appears to and the number of
  solutions asked for is doing nothing -- the search is returning the same state
  by different routes.

  If the handovers are distinct and some are cheap where others exhaust, then
  phase-2 solutions separate the way rotations do, at a four-hundredth of the
  cost, and the cascade should sweep them before it ever rotates the cube.

  If every solution exhausts on a cube where a rotation would have succeeded,
  the two are not interchangeable: rotations reach positions phase-2 solutions
  in one orientation cannot, and the cascade needs both -- solutions first
  because they are cheap, rotations after.
")
