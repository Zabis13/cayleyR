#!/usr/bin/env Rscript
# Does reducing the cube in all 24 orientations beat reducing it in the one it
# arrives in?
#
# Phases 1 and 2 search for centres and centres-by-axis. Their coordinates do
# not see the wings, so among the many shortest routes to their own goal, some
# leave the wings where phase 3 finishes at once and others leave them far out
# -- and nothing in those phases prefers the first kind. They return whichever
# the move ordering reached first.
#
# Measured on one cube six moves from solved (bench_handover.R), reducing it in
# each orientation and asking what phase 3 then faced:
#
#     x x, z z, z'    phase 3 finished in 0 moves
#     y y            16
#     y', x y'       18
#     (no rotation)  23   <- what the solver took
#     z              28
#     x, x'          phase 3 could not finish at all
#
# Rotating is sound: phase 3's goal set already holds all 24 rotations of the
# solved cube (kociemba4.h), so turning the cube does not change the question it
# is asked -- only which of the equally short answers phases 1 and 2 hand it.
# The rotation and its inverse go into the returned path, so the answer applies
# to the cube as it was given.
#
# ---- What this run has to be careful about ----------------------------------
#
# Two other things changed today, and a run that only compares against today's
# numbers cannot say what any single change did:
#
#   the table's width  now sized from what the fill writes, not only from the
#                      estimated cost of the search level
#   the node budget    2e5 -> 2e6 in the selection script
#
# So each seed is measured four ways: {one orientation, 24} x {old budget, new}.
# The one-orientation column at the old budget is the state of things before
# today; the 24-orientation column at the new budget is where it stands now;
# and the difference between the two columns at a fixed budget is the effect of
# this change alone.
#
# Cost is reported as well as outcome. Sweeping orientations means up to 24
# reductions instead of one -- there is an early exit when a rotation reduces
# the cube outright, but no guarantee it fires -- and a fix that solves more
# cubes while taking twenty times as long is a different answer from one that
# does not.
#
# Run with:  Rscript inst/examples/bench_orientations.R
#            Rscript inst/examples/bench_orientations.R 12    # 12 seeds

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

n_seeds <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 12L

N         <- 4L
budgets   <- c(2e5, 2e6)
timeout_s <- 1200L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

scramble_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  list(state = s, word = w)
}

if (worker) {
  seed   <- as.integer(args[[2]])
  sweep  <- args[[3]] == "24"
  budget <- as.numeric(args[[4]])
  out    <- args[[5]]

  sc <- scramble_state(seed)
  ors <- if (sweep) cayleyR:::.cube4_orientations else ""

  t0 <- proc.time()[["elapsed"]]
  path <- cube_kociemba4_reduce(sc$state, node_budget = budget,
                                orientations = ors)$path
  secs <- proc.time()[["elapsed"]] - t0

  # Reduced or not is decided on the cube, not on whether a path came back:
  # an already-reduced cube legitimately returns nothing.
  final <- replay(sc$state, path)
  ok <- cube_is_reduced(final)

  writeLines(sprintf("RESULT\t%d\t%.1f\t%s", length(path), secs, ok), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

run_one <- function(seed, sweep, budget) {
  res <- tempfile("orient", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, if (sweep) "24" else "1",
            format(budget, scientific = FALSE), shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = timeout_s)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    return(data.frame(seed = seed, sweep = sweep, budget = budget,
                      moves = NA_integer_, seconds = NA_real_,
                      reduced = FALSE, stringsAsFactors = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  data.frame(seed = seed, sweep = sweep, budget = budget,
             moves = as.integer(f[[2]]), seconds = as.numeric(f[[3]]),
             reduced = identical(f[[4]], "TRUE"), stringsAsFactors = FALSE)
}

hr("setup")
cat("seeds     : 1 .. ", n_seeds, "\n", sep = "")
cat("budgets   : ", paste(format(budgets, scientific = FALSE, big.mark = ","),
                          collapse = ", "), "\n", sep = "")
cat("modes     : one orientation, all 24\n")
cat("\nfour runs per seed, one process each, so the orientation sweep can be\n")
cat("read apart from the budget rather than through it.\n\n")

rows <- list()
for (sd in seq_len(n_seeds)) {
  sc <- scramble_state(sd)
  cat(sprintf("  seed %2d (%s)\n", sd, paste(sc$word, collapse = " ")))
  for (b in budgets) {
    for (sw in c(FALSE, TRUE)) {
      r <- run_one(sd, sw, b)
      cat(sprintf("    %-3s orient  budget %-9s  %-10s %4s moves  %6.1f s\n",
                  if (sw) "24" else "1",
                  format(b, scientific = FALSE, big.mark = ","),
                  if (r$reduced) "reduced" else "NOT reduced",
                  if (is.na(r$moves)) "?" else r$moves, r$seconds))
      flush.console()
      rows[[length(rows) + 1L]] <- r
    }
  }
}
tab <- do.call(rbind, rows)

hr("the four cells")
print(do.call(rbind, lapply(split(tab, list(tab$sweep, tab$budget)),
  function(d) if (!nrow(d)) NULL else data.frame(
    orientations = if (d$sweep[1]) "24" else "1",
    budget = format(d$budget[1], scientific = FALSE, big.mark = ","),
    reduced = sprintf("%d/%d", sum(d$reduced), nrow(d)),
    mean_moves = round(mean(d$moves[d$reduced], na.rm = TRUE), 1),
    mean_secs = round(mean(d$seconds, na.rm = TRUE), 1),
    stringsAsFactors = FALSE))), row.names = FALSE)

hr("the sweep on its own, budget held fixed")
for (b in budgets) {
  one <- tab[!tab$sweep & tab$budget == b, ]
  all <- tab[tab$sweep & tab$budget == b, ]
  cat(sprintf("  budget %-9s : %d/%d -> %d/%d reduced",
              format(b, scientific = FALSE, big.mark = ","),
              sum(one$reduced), nrow(one), sum(all$reduced), nrow(all)))
  gained <- setdiff(all$seed[all$reduced], one$seed[one$reduced])
  lost <- setdiff(one$seed[one$reduced], all$seed[all$reduced])
  if (length(gained)) cat("   gained: ", paste(gained, collapse = ", "), sep = "")
  if (length(lost)) cat("   LOST: ", paste(lost, collapse = ", "), sep = "")
  cat("\n")
}

hr("shorter, or just more often?")
# Solving more cubes and solving them in fewer moves are separate wins, and
# the sweep is picking the shortest of 24 so it should show both.
both <- merge(tab[!tab$sweep, c("seed", "budget", "moves", "reduced", "seconds")],
              tab[tab$sweep, c("seed", "budget", "moves", "reduced", "seconds")],
              by = c("seed", "budget"), suffixes = c("_one", "_24"))
both <- both[both$reduced_one & both$reduced_24, ]
if (nrow(both)) {
  cat(sprintf("on the %d cubes both modes reduced:\n", nrow(both)))
  cat(sprintf("  moves   %.1f -> %.1f\n",
              mean(both$moves_one), mean(both$moves_24)))
  cat(sprintf("  seconds %.1f -> %.1f (x%.1f)\n",
              mean(both$seconds_one), mean(both$seconds_24),
              mean(both$seconds_24) / max(mean(both$seconds_one), 1e-9)))
}

hr("verdict")
old <- tab[!tab$sweep & tab$budget == min(budgets), ]
new <- tab[tab$sweep & tab$budget == max(budgets), ]
cat(sprintf("before today (1 orientation, budget %s): %d of %d reduced\n",
            format(min(budgets), scientific = FALSE, big.mark = ","),
            sum(old$reduced), nrow(old)))
cat(sprintf("now (24 orientations, budget %s): %d of %d reduced\n",
            format(max(budgets), scientific = FALSE, big.mark = ","),
            sum(new$reduced), nrow(new)))
cat("\n")

fixed_b <- tab[tab$budget == max(budgets), ]
one_b <- fixed_b[!fixed_b$sweep, ]; all_b <- fixed_b[fixed_b$sweep, ]
if (sum(all_b$reduced) > sum(one_b$reduced)) {
  cat("At a fixed budget the sweep reduces more cubes, so the gain is the\n")
  cat("sweep's and not the budget's. What it costs is the time column: the\n")
  cat("sweep runs the reduction up to 24 times, and if that is too much, the\n")
  cat("same idea belongs inside phase 2 -- rank its equal-length solutions by\n")
  cat("what they leave the wings in, rather than reducing the whole cube over\n")
  cat("and over from outside.\n")
} else if (sum(all_b$reduced) == sum(one_b$reduced)) {
  cat("The sweep changed nothing at a fixed budget. Either these seeds are\n")
  cat("not the ones it helps -- bench_handover.R found the effect on seed 5\n")
  cat("specifically -- or the orientation it picks is not reaching phase 3,\n")
  cat("which would mean the reduction is choosing a rotation on a criterion\n")
  cat("other than what phase 3 then has to do. Check that the shortest total\n")
  cat("path is the right thing to be minimising here.\n")
} else {
  cat("The sweep reduced FEWER cubes, which should not happen: the unrotated\n")
  cat("orientation is among the 24, so the sweep can only pick something at\n")
  cat("least as short. A cube lost by the sweep means the rotation handling\n")
  cat("is wrong -- check that the inverse rotation really undoes the forward\n")
  cat("one on the same cube, not just on a solved one.\n")
}
