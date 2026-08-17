#!/usr/bin/env Rscript
# Does the heuristic fire at all, and does a wide enough table make it fire?
#
# bench_phase3_bonus2.R produced the sharpest result of the investigation and
# the one hardest to explain away. Filling the table from depth 4 to 5 to 6, on
# four different cubes:
#
#     bonus 0   depth 4   200,001 nodes     0.6 s
#     bonus 1   depth 5   200,001 nodes    11.8 s
#     bonus 2   depth 6   200,001 nodes   146.1 s
#
# Not "fewer nodes but not enough" -- the same node count to the last digit, at
# three different table depths, on cubes with nothing else in common. A search
# whose heuristic improved even slightly would walk a different tree and stop at
# a different number. This one walks the identical tree, so the added levels
# prune exactly nothing.
#
# The pruning code is not the problem; it was read (kociemba_core.h, in
# recurse): the bound is fetched at every node and tested twice, `bound >
# remaining` to cut the branch and `bound > remaining + 1` to drop the whole
# move class. Both tests are there and both are correct.
#
# Which leaves the bound itself. If it is always small, neither test ever
# passes, and the table is consulted at every node and prunes at none. That is
# invisible in a node count and obvious in a counter, so the counters now exist:
# `prune_lookups`, `prune_cuts`, `cut_ratio`, `mean_bound` come back with the
# search.
#
# Two candidate causes, and this run separates them by holding the width fixed
# rather than letting grow_to choose it:
#
#   width lags depth   grow_to sizes from the estimated cost of the next search
#                      level, extend_prune_table fills to a depth chosen
#                      separately. Measured: filling to depth 6 needs ~3.7M
#                      entries and the width sat at 2^21 = 2M, overfull twice
#                      over before the level even started, with the 2^28
#                      ceiling untouched. Fix the width and the bounds should
#                      become real.
#   the fold itself    ~1e11 coordinates over any addressable table. Then even
#                      a correct width leaves cut_ratio near zero, and the work
#                      is the coordinate, not the table.
#
# What to read, in order: cut_ratio first (is it pruning at all), then
# mean_bound against the depth being searched (can it prune in principle), then
# nodes. Nodes last, because nodes are what could not tell these apart.
#
# Run with:  Rscript inst/examples/bench_phase3_width.R
#            Rscript inst/examples/bench_phase3_width.R 3 6    # 3 seeds, depth 6

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

n_states <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 3L
depth    <- if (!worker && length(args) >= 2L) as.integer(args[[2]]) else 6L

N          <- 4L
p12_budget <- 2e6
budget     <- 2e5
timeout_s  <- 900L

# The widths to try, against a fill of ~3.7M entries at depth 6. 2^21 is what
# the search chose on its own and is overfull by a factor of two; 2^28 is the
# ceiling the phase is already entitled to and never reached.
widths <- c(2^21, 2^23, 2^25, 2^28)

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
  width <- as.numeric(args[[3]])
  out   <- args[[4]]

  h <- handed_state(seed)

  # Width first, then depth, then search. Doing it in this order is the point
  # of the run: the search would otherwise pick the width itself from a
  # quantity unrelated to how many entries the fill is about to produce.
  t0 <- proc.time()[["elapsed"]]
  cayleyR:::cube_kociemba4_fill_phase3_cpp(depth, width)
  fill_secs <- proc.time()[["elapsed"]] - t0
  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3
  bound <- cayleyR:::cube_phase3_coord_cpp(h$state)$prune_bound

  t1 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(h$state, node_budget = budget)
  secs <- proc.time()[["elapsed"]] - t1

  writeLines(sprintf(
    "RESULT\t%s\t%.0f\t%.2f\t%.2f\t%.0f\t%.0f\t%.0f\t%d\t%.0f\t%.0f\t%.4f\t%.2f\t%d",
    r$outcome, r$nodes, secs, fill_secs, tb$size, tb$filled, tb$n_visits,
    tb$built_depth, bound, r$prune_lookups, r$cut_ratio, r$mean_bound,
    h$p12_moves), out)
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

run_one <- function(seed, width) {
  cat(sprintf("    2^%-2.0f ... ", log2(width)))
  flush.console()

  res <- tempfile("p3w", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, format(width, scientific = FALSE),
            shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = timeout_s)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    cat(sprintf("no result (over %d s)\n", timeout_s))
    return(data.frame(seed = seed, width = width, outcome = "timeout",
                      nodes = NA_real_, seconds = NA_real_,
                      fill_secs = NA_real_, size = NA_real_,
                      filled = NA_real_, visits = NA_real_,
                      depth = NA_integer_, bound = NA_real_,
                      lookups = NA_real_, cut_ratio = NA_real_,
                      mean_bound = NA_real_, p12 = NA_integer_,
                      solved = FALSE, stringsAsFactors = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  o <- data.frame(seed = seed, width = width, outcome = f[[2]],
                  nodes = as.numeric(f[[3]]), seconds = as.numeric(f[[4]]),
                  fill_secs = as.numeric(f[[5]]), size = as.numeric(f[[6]]),
                  filled = as.numeric(f[[7]]), visits = as.numeric(f[[8]]),
                  depth = as.integer(f[[9]]), bound = as.numeric(f[[10]]),
                  lookups = as.numeric(f[[11]]),
                  cut_ratio = as.numeric(f[[12]]),
                  mean_bound = as.numeric(f[[13]]),
                  p12 = as.integer(f[[14]]),
                  solved = f[[2]] == "found", stringsAsFactors = FALSE)
  cat(sprintf("%-10s  fill %5.1f%%  bound %2.0f  cuts %6.2f%%  mean %.2f  %9s nodes  (%.0fs fill)\n",
              o$outcome, 100 * o$filled / o$size, o$bound,
              100 * o$cut_ratio, o$mean_bound,
              format(o$nodes, big.mark = ",", scientific = FALSE),
              o$fill_secs))
  o
}

hr("setup")
cat("seeds        : ", paste(seeds, collapse = ", "), "\n", sep = "")
cat("fill depth   : ", depth, " (fixed; the width is what varies)\n", sep = "")
cat("widths       : ", paste(sprintf("2^%.0f", log2(widths)), collapse = ", "),
    "\n", sep = "")
cat("node budget  : ", format(budget, scientific = FALSE, big.mark = ","),
    "\n", sep = "")
cat("\ncut_ratio is the share of nodes where the table pruned. It is the\n")
cat("number this run exists for: node counts identical to the digit across\n")
cat("three table depths say the heuristic never fires, and only a counter\n")
cat("on the pruning test itself can confirm that.\n")

rows <- list()
for (sd in seeds) {
  h <- handed_state(sd)
  hr(paste0("seed ", sd, " (p12 ", h$p12_moves, " moves)"))
  for (w in widths) rows[[length(rows) + 1L]] <- run_one(sd, w)
}
tab <- do.call(rbind, rows)

hr("does the heuristic fire?")
print(do.call(rbind, lapply(split(tab, tab$width), function(d) data.frame(
  width = sprintf("2^%.0f", log2(d$width[1])),
  fill_pct = round(100 * mean(d$filled / d$size, na.rm = TRUE), 2),
  mean_bound = round(mean(d$mean_bound, na.rm = TRUE), 2),
  cut_pct = round(100 * mean(d$cut_ratio, na.rm = TRUE), 3),
  solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
  nodes = round(mean(d$nodes, na.rm = TRUE)),
  fill_secs = round(mean(d$fill_secs, na.rm = TRUE), 1),
  stringsAsFactors = FALSE))), row.names = FALSE)

hr("cost of the width")
# Stated separately because a confirmed hypothesis with an unaffordable price
# is a different answer from a confirmed one with a cheap price.
for (w in widths) {
  d <- tab[tab$width == w, ]
  if (!nrow(d)) next
  cat(sprintf("  2^%-2.0f  %6.0f MB  fill %5.1f s  %s entries into %s slots\n",
              log2(w), w / 2^20, mean(d$fill_secs, na.rm = TRUE),
              format(round(mean(d$filled, na.rm = TRUE)), big.mark = ","),
              format(w, big.mark = ",", scientific = FALSE)))
}

hr("verdict")
best <- tab[tab$width == max(tab$width) & !is.na(tab$cut_ratio), ]
worst <- tab[tab$width == min(tab$width) & !is.na(tab$cut_ratio), ]

if (!nrow(best) || !nrow(worst)) {
  cat("Not enough finished runs to compare. Raise the timeout.\n")
} else {
  cat(sprintf("narrowest 2^%.0f: cuts %.3f%%, mean bound %.2f, %d solved\n",
              log2(min(tab$width)), 100 * mean(worst$cut_ratio),
              mean(worst$mean_bound), sum(worst$solved)))
  cat(sprintf("widest    2^%.0f: cuts %.3f%%, mean bound %.2f, %d solved\n",
              log2(max(tab$width)), 100 * mean(best$cut_ratio),
              mean(best$mean_bound), sum(best$solved)))
  cat("\n")
  if (mean(best$cut_ratio) > 10 * max(mean(worst$cut_ratio), 1e-9)) {
    cat("The width was the constraint. A table wide enough to hold the level\n")
    cat("it is filled to returns real bounds, and the heuristic starts\n")
    cat("pruning. The repair is to size the table from how many entries the\n")
    cat("fill will produce, not from the estimated cost of the search level --\n")
    cat("those are different quantities and grow_to uses the second.\n")
  } else if (mean(best$cut_ratio) < 0.01) {
    cat("Even at the full width the table almost never prunes. Then the fold\n")
    cat("is the limit, not the size of what it folds into: about 1e11\n")
    cat("coordinates do not fit any table, and widening trades one collision\n")
    cat("rate for a slightly lower one. The work is the coordinate -- what\n")
    cat("phase 3 needs to know about a state, rather than where every piece\n")
    cat("in it sits.\n")
  } else {
    cat("Widening helped without settling it. Compare mean_bound against the\n")
    cat("depths being searched: if the bound stays far below the remaining\n")
    cat("depth, the table is honest and weak rather than broken, and no width\n")
    cat("repairs that.\n")
  }
}
