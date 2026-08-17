#!/usr/bin/env Rscript
# Which states handed over by phases 1 and 2 does phase 3 actually fail on?
#
# This exists because the obvious ways to ask the question are both broken.
#
# Reading the prune table's bound does not work. cube_phase3_coord_cpp() reads
# the table but never builds it, so on a fresh process built_depth is 0 and
# get() returns built_depth + 1 = 1 for every state that is not already at a
# goal. Measured: seeds 1..12 all score 0 or 1. A selection rule of
# "bound >= 5" cannot fire at all, which is why bench_phase3_bonus.R found no
# states to measure.
#
# Running the searches in one process does not work either. The phase 3 table
# is a singleton and each search grows it, so a seed is measured on whatever
# depth its predecessors left behind. Measured, seeds 1..6 in one process:
#
#     seed 1  found      depth 0->0
#     seed 2  found      depth 0->1
#     seed 3  found      depth 1->1
#     seed 4  exhausted  depth 1->4      <- grew the table three levels
#     seed 5  exhausted  depth 4->4
#     seed 6  found      depth 4->4      <- searched a table seed 1 never saw
#
# Seed 6 is not comparable to seed 1: it inherited four levels of table that
# seed 1 paid nothing for. Selecting on the outcome rather than on the bound
# does not fix this -- it moves the same order-dependence from the number being
# read to the search being run.
#
# So: one process per seed, every one of them starting from an empty table and
# the same node budget. The outcome then says something about the state instead
# of something about its position in the queue. built_depth on entry is
# reported for each worker so the identical starting point is visible in the
# output rather than assumed.
#
# Run with:  Rscript inst/examples/bench_phase3_select.R
#            Rscript inst/examples/bench_phase3_select.R 200        # seeds
#            Rscript inst/examples/bench_phase3_select.R 200 200000 # + budget

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)

worker <- length(args) >= 1L && args[[1]] == "--run"

n_seeds <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 60L
# 2e6, not the 2e5 this started at. The prune table grows from the estimated
# cost of the level about to be searched, and at 2e5 the search dies before any
# level's estimate exceeds the starting size: measured on seed 5, n_grows is 0
# at 2e5 and 2 at 2e6, and the seed goes from exhausted to solved with it. A
# budget that stops the table growing measures the table not growing.
budget  <- if (!worker && length(args) >= 2L) as.numeric(args[[2]]) else 2e6

N              <- 4L
p12_budget     <- 2e6
worker_timeout <- 120L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# Built from the seed, not passed in, so the worker reproduces the same cube
# without anything being serialised across the process boundary.
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
  seed <- as.integer(args[[2]])
  out  <- args[[3]]

  h <- handed_state(seed)

  # The starting point, reported rather than trusted: every worker must enter
  # at the same depth for the outcomes to be comparable.
  depth_in <- cayleyR:::cube_kociemba4_tables_cpp()$phase3$built_depth

  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(h$state, node_budget = budget)
  secs <- proc.time()[["elapsed"]] - t0

  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3

  writeLines(sprintf("RESULT\t%s\t%.0f\t%.4f\t%d\t%d\t%d\t%d\t%s\t%.0f\t%.0f",
                     r$outcome, r$nodes, secs, length(r$path),
                     depth_in, tb$built_depth, h$p12_moves,
                     paste(h$word, collapse = " "), tb$size, tb$n_grows), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
rscript <- file.path(R.home("bin"), "Rscript")

run_one <- function(seed) {
  res <- tempfile("p3sel", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(rscript, c(shQuote(this_file), "--run", seed, shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = worker_timeout)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    return(data.frame(seed = seed, outcome = "timeout", nodes = NA_real_,
                      seconds = as.numeric(worker_timeout), moves = 0L,
                      depth_in = NA_integer_, depth_out = NA_integer_,
                      p12_moves = NA_integer_, word = NA_character_,
                      size = NA_real_, n_grows = NA_real_,
                      solved = FALSE, stringsAsFactors = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  data.frame(seed = seed, outcome = f[[2]], nodes = as.numeric(f[[3]]),
             seconds = as.numeric(f[[4]]), moves = as.integer(f[[5]]),
             depth_in = as.integer(f[[6]]), depth_out = as.integer(f[[7]]),
             p12_moves = as.integer(f[[8]]), word = f[[9]],
             size = as.numeric(f[[10]]), n_grows = as.numeric(f[[11]]),
             solved = f[[2]] == "found", stringsAsFactors = FALSE)
}

hr("setup")
cat("seeds        : ", n_seeds, "\n", sep = "")
cat("node budget  : ", format(budget, scientific = FALSE, big.mark = ","),
    "\n", sep = "")
cat("one process per seed, each starting from an empty phase 3 table.\n")
cat("depth_in is that starting depth, printed so it can be checked.\n\n")

rows <- list()
for (sd in seq_len(n_seeds)) {
  r <- run_one(sd)
  rows[[length(rows) + 1L]] <- r
  cat(sprintf("  seed %3d  %-10s  %10s nodes  depth %s->%s  table %5s (x%s)  p12 %2s  %.1f s\n",
              sd, r$outcome, format(r$nodes, scientific = FALSE, big.mark = ","),
              r$depth_in, r$depth_out,
              if (is.na(r$size)) "?" else sprintf("2^%.0f", log2(r$size)),
              if (is.na(r$n_grows)) "?" else as.character(r$n_grows),
              r$p12_moves, r$seconds))
  flush.console()
}
tab <- do.call(rbind, rows)

hr("isolation check")
d <- unique(tab$depth_in[!is.na(tab$depth_in)])
if (length(d) == 1L) {
  cat(sprintf("every worker entered with built_depth = %d. Comparable.\n", d))
} else {
  cat("WORKERS DID NOT START ALIKE -- depth_in took values ",
      paste(sort(d), collapse = ", "), ".\n", sep = "")
  cat("The outcomes below are not comparable to each other. Fix this first.\n")
}

hr("did the table grow?")
# The table is sized from the estimated cost of the level about to be searched,
# so a run that never reaches a costly level never grows -- and then this whole
# measurement is about a table stuck at its starting size rather than about the
# cubes. n_grows = 0 everywhere means the budget is too small to be measuring
# what it looks like it is measuring.
if (any(!is.na(tab$n_grows))) {
  cat(sprintf("grew in %d of %d runs. sizes reached: %s\n",
              sum(tab$n_grows > 0, na.rm = TRUE), nrow(tab),
              paste(sprintf("2^%.0f", sort(unique(log2(tab$size[!is.na(tab$size)])))),
                    collapse = ", ")))
  g <- do.call(rbind, lapply(split(tab, tab$n_grows), function(d) data.frame(
    n_grows = d$n_grows[1], runs = nrow(d),
    solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
    stringsAsFactors = FALSE)))
  print(g, row.names = FALSE)
}

hr("outcome")
print(table(tab$outcome), row.names = FALSE)
cat(sprintf("\nsolved %d of %d (%.0f%%)\n", sum(tab$solved), nrow(tab),
            100 * mean(tab$solved)))

hr("failing seeds")
bad <- tab[!tab$solved, ]
if (!nrow(bad)) {
  cat("none failed at this budget. Raise the seed count or lower the budget.\n")
} else {
  cat("These are the states to measure the prune depth against.\n\n")
  print(bad[, c("seed", "outcome", "nodes", "seconds", "p12_moves", "word")],
        row.names = FALSE)
  cat("\nseeds: ", paste(bad$seed, collapse = ", "), "\n", sep = "")
}

hr("phases 1 and 2 length against phase 3 solving")
# The open hypothesis in TODO.md: long phase 1-2 output predicts phase 3
# failure. Now measurable without the order-dependence that clouded it before.
if (any(!is.na(tab$p12_moves))) {
  bands <- cut(tab$p12_moves, breaks = c(-1, 4, 8, 12, Inf),
               labels = c("<=4", "5-8", "9-12", ">12"))
  print(do.call(rbind, lapply(split(tab, bands), function(d) {
    if (!nrow(d)) return(NULL)
    data.frame(p12_moves = as.character(bands[match(d$seed[1], tab$seed)]),
               cubes = nrow(d),
               solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
               mean_nodes = round(mean(d$nodes, na.rm = TRUE)),
               stringsAsFactors = FALSE)
  })), row.names = FALSE)
}

# Not tempdir(): that is per-process and goes away when this one exits, so
# bench_phase3_bonus.R would never find it and would redo the selection every
# time. A fixed path under the system temp directory outlives the process and
# is the same one the other script looks in.
out_file <- file.path(dirname(tempdir()), "cayleyR_phase3_select.rds")
saveRDS(tab, out_file)
cat("\ntable saved to ", out_file, "\n", sep = "")
cat("bench_phase3_bonus.R reads it from there; delete it to reselect.\n")
