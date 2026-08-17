#!/usr/bin/env Rscript
# Does prune_depth_bonus work now that the table's width is allowed to grow?
#
# It was measured once and lost badly -- 11x the time for no change in nodes --
# but that measurement was taken under a condition that no longer holds. The
# table started at 1<<24 and grow_to() only ever grows, so no level's estimate
# ever exceeded the starting size and n_grows was 0 on every run: the width was
# frozen. Filling one level deeper into a frozen table means filling 12.5x more
# entries into the same slots, which is not a deeper table but a fuller one.
# Fixed 2026-08-13 by starting phase 3 at 1<<20; n_grows is now 1 or 2.
#
# What the width does not fix is the depth, and the depth is what stalls:
#
#     int fill_to = limit / 2 + limits.prune_depth_bonus;   kociemba_core.h:888
#
# `limit` only advances when a level is searched to completion. A budget that
# runs out at limit 9 never reaches limit 10, so fill_to stays at 9/2 = 4 for
# the rest of the run. Measured on seed 5, whose true distance is 23:
#
#     budget 1e5    width 2^20   depth 4   exhausted
#     budget 2e5    width 2^21   depth 4   exhausted
#     budget 8e5    width 2^21   depth 4   exhausted
#     budget 1.6e6  width 2^24   depth 5   FOUND
#
# The width climbed the whole way and changed nothing. The solve arrived with
# the depth. That is the circle prune_depth_bonus exists to break: it sets the
# fill depth independently of how far the search has got, so the table can be
# deep before the search has earned it.
#
# Held fixed here, so that depth is the only thing varying:
#
#   the budget, at a value where the search stalls at depth 4 without help
#   one process per (seed, bonus), since the table is a singleton
#   the same seeds throughout, taken from bench_phase3_select.R
#
# Reading it. If bonus buys the depth the budget could not, the exhausted runs
# turn into solves and nodes drop by something like the branching factor per
# level. If nodes fall but the outcomes do not change, the heuristic improved
# and the budget is now what binds. If neither moves, depth is not what these
# states are short of, and the next thing to look at is the handover: the
# failing seeds are the ones phases 1 and 2 hand over after 9+ moves.
#
# Run with:  Rscript inst/examples/bench_phase3_bonus2.R
#            Rscript inst/examples/bench_phase3_bonus2.R 6 3      # 6 seeds, 0..3
#            Rscript inst/examples/bench_phase3_bonus2.R 6 3 5e5  # + budget

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

n_states  <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 4L
max_bonus <- if (!worker && length(args) >= 2L) as.integer(args[[2]]) else 3L
# Deliberately a budget that fails without help: at 2e5 seed 5 stalls at table
# depth 4 and exhausts, at 1.6e6 it reaches depth 5 and solves. Measuring bonus
# at a budget that already solves would hide the effect being looked for.
budget    <- if (!worker && length(args) >= 3L) as.numeric(args[[3]]) else 2e5

N              <- 4L
p12_budget     <- 2e6
worker_timeout <- 600L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

handed_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = p12_budget)
  list(state = replay(s, p12), word = w, p12_moves = length(p12))
}

if (worker) {
  seed  <- as.integer(args[[2]])
  bonus <- as.integer(args[[3]])
  out   <- args[[4]]
  budget <- as.numeric(args[[5]])

  h <- handed_state(seed)
  depth_in <- cayleyR:::cube_kociemba4_tables_cpp()$phase3$built_depth

  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(h$state, node_budget = budget,
                                           prune_depth_bonus = bonus)
  secs <- proc.time()[["elapsed"]] - t0

  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3

  writeLines(sprintf("RESULT\t%s\t%.0f\t%.2f\t%d\t%d\t%d\t%.0f\t%.0f\t%.0f\t%d",
                     r$outcome, r$nodes, secs, length(r$path),
                     depth_in, tb$built_depth, tb$size, tb$n_grows,
                     tb$filled, h$p12_moves), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

select_file <- file.path(dirname(tempdir()), "cayleyR_phase3_select.rds")
if (!file.exists(select_file)) {
  stop("run inst/examples/bench_phase3_select.R first", call. = FALSE)
}
sel <- readRDS(select_file)
seeds <- head(sel$seed[!sel$solved], n_states)
if (!length(seeds)) stop("no failing states in the selection", call. = FALSE)

run_one <- function(seed, bonus) {
  cat(sprintf("    bonus %d ... ", bonus))
  flush.console()

  res <- tempfile("p3b2", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, bonus, shQuote(res), budget),
          stdout = NULL, stderr = NULL, timeout = worker_timeout)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    cat(sprintf("no result (over %d s)\n", worker_timeout))
    return(data.frame(seed = seed, bonus = bonus, outcome = "timeout",
                      nodes = NA_real_, seconds = as.numeric(worker_timeout),
                      moves = 0L, depth_in = NA_integer_,
                      depth = NA_integer_, size = NA_real_,
                      n_grows = NA_real_, filled = NA_real_,
                      p12_moves = NA_integer_, solved = FALSE,
                      stringsAsFactors = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  out <- data.frame(seed = seed, bonus = bonus, outcome = f[[2]],
                    nodes = as.numeric(f[[3]]), seconds = as.numeric(f[[4]]),
                    moves = as.integer(f[[5]]), depth_in = as.integer(f[[6]]),
                    depth = as.integer(f[[7]]), size = as.numeric(f[[8]]),
                    n_grows = as.numeric(f[[9]]), filled = as.numeric(f[[10]]),
                    p12_moves = as.integer(f[[11]]),
                    solved = f[[2]] == "found", stringsAsFactors = FALSE)
  cat(sprintf("%-10s  depth %d  width 2^%.0f (x%.0f)  %9s nodes  %.1f s\n",
              out$outcome, out$depth, log2(out$size), out$n_grows,
              format(out$nodes, big.mark = ",", scientific = FALSE),
              out$seconds))
  out
}

hr("setup")
cat("seeds        : ", paste(seeds, collapse = ", "), "\n", sep = "")
cat("bonuses      : ", paste(seq(0L, max_bonus), collapse = ", "), "\n", sep = "")
cat("node budget  : ", format(budget, scientific = FALSE, big.mark = ","),
    "\n", sep = "")
cat("\nfill depth is limit/2 + bonus, and limit only advances when a level\n")
cat("finishes. At this budget it stalls at 4, so bonus is the only way the\n")
cat("table gets deeper. Width now grows on its own -- watch it separately.\n")

rows <- list()
for (i in seq_along(seeds)) {
  sd <- seeds[[i]]
  h <- handed_state(sd)
  hr(paste0("seed ", sd, " (p12 ", h$p12_moves, " moves)"))
  cat("  scramble: ", paste(h$word, collapse = " "), "\n", sep = "")
  for (b in seq(0L, max_bonus)) rows[[length(rows) + 1L]] <- run_one(sd, b)
}
tab <- do.call(rbind, rows)

hr("isolation check")
d_in <- unique(tab$depth_in[!is.na(tab$depth_in)])
if (length(d_in) == 1L) {
  cat(sprintf("every worker entered with built_depth = %d. Comparable.\n", d_in))
} else {
  cat("WORKERS DID NOT START ALIKE -- depth_in was ",
      paste(sort(d_in), collapse = ", "), ". Fix this before reading on.\n",
      sep = "")
}

hr("every run")
print(tab[, c("seed", "bonus", "outcome", "depth", "n_grows", "nodes",
              "seconds")], row.names = FALSE)

hr("by bonus")
print(do.call(rbind, lapply(split(tab, tab$bonus), function(d) data.frame(
  bonus = d$bonus[1],
  solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
  mean_depth = round(mean(d$depth, na.rm = TRUE), 1),
  mean_nodes = round(mean(d$nodes, na.rm = TRUE)),
  mean_secs = round(mean(d$seconds, na.rm = TRUE), 1),
  stringsAsFactors = FALSE))), row.names = FALSE)

hr("what one more level of table costs and buys")
# The trade, stated per level rather than in totals: filling a level costs
# about the branching factor, and should save about the branching factor.
for (i in seq_along(seeds)) {
  d <- tab[tab$seed == seeds[[i]], ]
  d <- d[order(d$bonus), ]
  cat(sprintf("\n  seed %d\n", seeds[[i]]))
  for (j in seq_len(nrow(d))) {
    prev <- if (j > 1L) d[j - 1L, ] else NULL
    fn <- if (is.null(prev) || !isTRUE(prev$nodes > 0)) NA
          else d$nodes[j] / prev$nodes
    fs <- if (is.null(prev) || !isTRUE(prev$seconds > 0)) NA
          else d$seconds[j] / prev$seconds
    cat(sprintf("    bonus %d  depth %2d  %-10s  nodes x%-6s  secs x%s\n",
                d$bonus[j], d$depth[j], d$outcome[j],
                if (is.na(fn)) "-" else sprintf("%.2f", fn),
                if (is.na(fs)) "-" else sprintf("%.1f", fs)))
  }
}

hr("two mechanisms, kept apart")
# There are two separate reasons a state fails here, and a run that reports
# only "solved / not solved" merges them.
#
#   the stalled table   fill_to = limit/2 and limit does not advance, so the
#                       table stays at depth 4 however long the search runs.
#                       A bonus fixes this by setting the depth directly.
#   collisions          the coordinate folded into the table's width. Measured
#                       at 1<<24 and depth 6: 146M of 150M writes lost, 78% of
#                       occupied slots holding the single value 6, and seeds
#                       23 and 26 moves out told they were 6 moves out. A
#                       deeper table does not help; it makes it worse.
#
# The prediction that separates them: a state failing only for the first reason
# solves once the bonus deepens the table. One failing for the second does not,
# because the entries it reads were never its own.
cat(sprintf("  %-6s %-5s %-9s %s\n", "seed", "p12", "bonus 0", "best bonus"))
for (sd in seeds) {
  d <- tab[tab$seed == sd, ]
  b0 <- d[d$bonus == 0L, ]
  best <- d[d$solved, ]
  cat(sprintf("  %-6d %-5d %-9s %s\n", sd, d$p12_moves[1],
              if (nrow(b0) && b0$solved[1]) "solved" else "NO",
              if (nrow(best)) sprintf("solved at bonus %d", min(best$bonus))
              else "never solved"))
}
cat("\n")
cat("Seeds that turn from NO to solved are the stalled-table half. Seeds\n")
cat("that never solve at any bonus are the ones to take to the coordinate\n")
cat("work -- deepening cannot reach them.\n")

hr("verdict")
base <- tab[tab$bonus == 0L, ]
top  <- tab[tab$bonus == max(tab$bonus), ]

cat(sprintf("bonus 0: %d of %d solved, table depth %.1f\n",
            sum(base$solved), nrow(base), mean(base$depth, na.rm = TRUE)))
cat(sprintf("bonus %d: %d of %d solved, table depth %.1f\n",
            max(tab$bonus), sum(top$solved), nrow(top),
            mean(top$depth, na.rm = TRUE)))
cat("\n")

if (sum(top$solved) > sum(base$solved)) {
  cat("The bonus buys the depth the budget could not, and the states solve.\n")
  cat("That closes the circle this run was written for: fill_to = limit/2\n")
  cat("cannot deepen a table on a search that never finishes a level, and\n")
  cat("setting the depth directly is what breaks it. The question that\n")
  cat("follows is whether to keep the bonus as a dial or to fill to a fixed\n")
  cat("depth at the start of the phase and stop tying it to limit at all.\n")
} else if (all(top$depth <= base$depth, na.rm = TRUE)) {
  cat("The bonus did not deepen the table -- depth is the same at every\n")
  cat("bonus. Then the dial is not reaching the fill, and the place to look\n")
  cat("is the clamp on the line below it: fill_to is capped at limit, so a\n")
  cat("bonus cannot take the table deeper than the level being searched.\n")
} else {
  cat("The table got deeper and the outcomes did not change. Depth is not\n")
  cat("what these states are short of. They are the seeds phases 1 and 2\n")
  cat("hand over after 9 or more moves, and that handover is the next thing\n")
  cat("to measure -- a cube six moves from solved should not be arriving at\n")
  cat("phase 3 twelve moves deep.\n")
}
