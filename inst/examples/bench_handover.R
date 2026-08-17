#!/usr/bin/env Rscript
# A cube six moves from solved arrives at phase 3 twenty-three moves from its
# goal. Is that forced, or is it the particular route phases 1 and 2 chose?
#
# Measured on seed 5: the scramble is 6 moves, phases 1 and 2 spend 6 more, and
# phase 3 is then 23 moves out (bench_phase3_true_distance.R). Twenty-nine moves
# of work on a puzzle six moves deep.
#
# The obvious reading -- that phases 1 and 2 took a needlessly long route -- is
# wrong, and worth stating so nobody spends a day on it. Their search is IDA*
# over increasing limits, so the first solution it returns is a shortest one for
# that phase's goal. Six moves is the minimum to reach phase 2's target from
# this cube; there is no shorter route being missed.
#
# What is not constrained is which shortest route. Phase 1's coordinate sees
# centres. Phase 2's sees centres by axis and a parity bit. Neither sees the
# wings at all, apart from the filter phase 2 applies at the end. So among the
# routes that reach the goal in the same number of moves, some leave the wings
# in a state phase 3 can finish quickly and others do not, and nothing in the
# search prefers the first kind. It returns whichever the move ordering reaches
# first.
#
# That is what this measures. For one cube, enumerate many distinct phase 1-2
# solutions rather than the single one the search happens to return, and for
# each ask how far phase 3 then has to go. What comes back is a distribution:
#
#   narrow, all near 23   the handover is forced. Phase 3 has to deal with far
#                         states, and no reordering of earlier phases helps --
#                         the work belongs in phase 3's coordinate.
#   wide, some near 8     the handover is a choice being made blindly. Then the
#                         cheap repair is upstream: have phase 2 rank its equal
#                         length solutions by what they leave for phase 3, the
#                         way it already rejects some of them with a filter.
#
# Phase 3 distance is measured by solving, with a budget large enough to finish
# on the near states. States that exhaust the budget are reported as ">budget"
# and counted separately -- they are the far ones, which is the answer either
# way.
#
# Run with:  Rscript inst/examples/bench_handover.R
#            Rscript inst/examples/bench_handover.R 5 40    # seed 5, 40 routes

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

seed     <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 5L
n_routes <- if (!worker && length(args) >= 2L) as.integer(args[[2]]) else 24L

N          <- 4L
p12_budget <- 2e6
p3_budget  <- 5e6
fill_depth <- 6L
timeout_s  <- 900L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

scramble_of <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  w
}

# Different routes to the same place, obtained by rotating the cube before the
# phases run and rotating back after. A whole-cube rotation does not change how
# far anything is from anywhere -- it renames the axes -- so every route this
# produces is a genuine phase 1-2 solution of the same length class, reached
# through a different move ordering. That is exactly the freedom the search
# leaves unspecified.
rotations <- function() {
  base <- c("", "x", "x x", "x'", "y", "y y", "y'", "z", "z z", "z'",
            "x y", "x y y", "x y'", "x' y", "x' y'", "y x", "y x'",
            "y y x", "z y", "z y'", "z' y", "x z", "y z", "y' z")
  base
}

if (worker) {
  sd    <- as.integer(args[[2]])
  ridx  <- as.integer(args[[3]])
  out   <- args[[4]]

  w <- scramble_of(sd)
  rots <- rotations()
  rot <- rots[[ridx]]

  st <- replay(cube_identity(N), w)
  if (nzchar(rot)) st <- st[cube_wide_word(rot, N)]

  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(st, upto_phase = 2L,
                                              node_budget = p12_budget)
  handed <- replay(st, p12)

  # The same table for every route, built before the search, so routes are
  # compared against one heuristic rather than against whatever each one's
  # search happened to build.
  cayleyR:::cube_kociemba4_fill_phase3_cpp(fill_depth, 2^25)

  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(handed, node_budget = p3_budget)
  secs <- proc.time()[["elapsed"]] - t0

  writeLines(sprintf("RESULT\t%d\t%s\t%d\t%d\t%.0f\t%.1f\t%.4f",
                     ridx, r$outcome, length(p12),
                     if (r$found) length(r$path) else -1L,
                     r$nodes, secs, r$cut_ratio), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

rots <- rotations()
n_routes <- min(n_routes, length(rots))

run_one <- function(ridx) {
  res <- tempfile("hand", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, ridx, shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = timeout_s)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    return(data.frame(route = ridx, rot = rots[[ridx]], outcome = "timeout",
                      p12 = NA_integer_, p3 = NA_integer_, nodes = NA_real_,
                      seconds = NA_real_, cut_ratio = NA_real_,
                      stringsAsFactors = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  p3 <- as.integer(f[[5]])
  data.frame(route = ridx, rot = rots[[ridx]], outcome = f[[3]],
             p12 = as.integer(f[[4]]),
             p3 = if (p3 < 0L) NA_integer_ else p3,
             nodes = as.numeric(f[[6]]), seconds = as.numeric(f[[7]]),
             cut_ratio = as.numeric(f[[8]]), stringsAsFactors = FALSE)
}

hr("setup")
cat("seed         : ", seed, " (scramble ", paste(scramble_of(seed), collapse = " "),
    ")\n", sep = "")
cat("routes       : ", n_routes, " whole-cube rotations before the phases run\n",
    sep = "")
cat("phase 3 table: depth ", fill_depth, ", width 2^25, same for every route\n",
    sep = "")
cat("\nEvery route is a shortest phase 1-2 solution -- IDA* guarantees that.\n")
cat("What differs is which shortest one, and the phases have no preference\n")
cat("because their coordinates do not see the wings.\n\n")

rows <- list()
for (i in seq_len(n_routes)) {
  cat(sprintf("  route %2d (%-6s) ... ", i, if (nzchar(rots[[i]])) rots[[i]] else "-"))
  flush.console()
  r <- run_one(i)
  cat(sprintf("p12 %2s  ->  phase 3 %s  (%s nodes, %.0fs)\n",
              r$p12,
              if (is.na(r$p3)) ">budget" else sprintf("%2d moves", r$p3),
              format(r$nodes, big.mark = ",", scientific = FALSE),
              r$seconds))
  rows[[length(rows) + 1L]] <- r
}
tab <- do.call(rbind, rows)

hr("what the handover left for phase 3")
done <- tab[!is.na(tab$p3), ]
cat(sprintf("%d of %d routes finished inside the budget.\n", nrow(done), nrow(tab)))
if (nrow(done)) {
  cat(sprintf("phase 3 length: min %d, median %.0f, max %d\n",
              min(done$p3), median(done$p3), max(done$p3)))
  print(table(done$p3))
}

hr("phase 1-2 length against what it left behind")
if (nrow(done)) {
  print(do.call(rbind, lapply(split(done, done$p12), function(d) data.frame(
    p12 = d$p12[1], routes = nrow(d),
    p3_min = min(d$p3), p3_median = median(d$p3), p3_max = max(d$p3),
    stringsAsFactors = FALSE))), row.names = FALSE)
}

hr("verdict")
if (!nrow(done)) {
  cat("No route finished. Every shortest phase 1-2 solution leaves this cube\n")
  cat("far from phase 3's goal, which is the forced-handover answer: the work\n")
  cat("is in phase 3, not upstream. Raise the budget to put a number on it.\n")
} else {
  spread <- max(done$p3) - min(done$p3)
  cat(sprintf("spread across routes: %d moves (min %d, max %d)\n",
              spread, min(done$p3), max(done$p3)))
  cat(sprintf("routes that exhausted the budget: %d of %d\n",
              sum(is.na(tab$p3)), nrow(tab)))
  cat("\n")
  if (spread >= 6) {
    cat("The handover is a choice, not a constraint. Equal-length phase 1-2\n")
    cat("solutions leave phase 3 anywhere in this range, and the search picks\n")
    cat("among them by move ordering alone. The cheap repair is upstream:\n")
    cat("phase 2 already rejects some solutions with a filter, so it can rank\n")
    cat("the rest by what they leave for phase 3 instead of taking the first.\n")
  } else {
    cat("The handover is forced. Every shortest route leaves phase 3 about\n")
    cat("the same distance out, so no amount of choosing between them helps,\n")
    cat("and the distance phase 3 faces is a property of the cube rather than\n")
    cat("of the route taken to it. Then the work really is phase 3's\n")
    cat("coordinate -- a table of depth 6 cannot prune a state 23 moves out,\n")
    cat("whatever its width, because its deepest honest answer is 7.\n")
  }
}
