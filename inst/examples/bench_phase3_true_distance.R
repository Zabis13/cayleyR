#!/usr/bin/env Rscript
# Is the bound of 6 these states get at table depth 6 their real distance, or a
# different state's entry that hashed into the same slot?
#
# bench_phase3_saturation.R measured, for the six failing seeds:
#
#     table depth 5   stub is 6   bounds  6 6 6 6 6 6
#     table depth 6   stub is 7   bounds  6 6 6 6 6 6
#
# and its verdict read the second row as the states finally entering the table.
# The reasoning holds up only so far. At depth 5 the 6 is the stub -- computed
# as built_depth + 1 because the slot was empty, not read from it. At depth 6
# the stub is 7, so a 6 must have come out of a slot holding v - 1 = 6, written
# by the depth 6 fill, since set_if_empty leaves an occupied slot alone and the
# slot was empty one level earlier.
#
# What that does not settle is whose entry it is. The fill walks 150 million
# states into 16.7 million slots at depth 6, so a slot may well hold a different
# state that hashed alike. First-writer-wins keeps the bound admissible -- the
# value is some state's true distance, so it cannot overestimate -- but an
# admissible bound belonging to another state is not information about this one.
# A byte of depth carries no signature, so the table cannot be asked.
#
# The one external check is to solve the state for real: run phase 3 with a
# budget large enough to finish and compare the solution's length with 6.
#
#   length == 6   the bound was this state's own distance. The heuristic was
#                 telling the truth and the earlier reading was right.
#   length  > 6   the bound came from somewhere else. Admissible, useless, and
#                 the claim that depth alone explains the failure comes off the
#                 table -- along with any tuning of prune_depth_bonus.
#
# Note what the second outcome would NOT mean. A bound of 6 against a true
# distance of, say, 10 is exactly what a correct-but-weak heuristic looks like
# too. Collision is one way to get there; a coordinate too coarse to separate
# near states from far ones is another, and this test does not tell them apart.
# It only settles whether 6 is this cube's number.
#
# Run with:  Rscript inst/examples/bench_phase3_true_distance.R
#            Rscript inst/examples/bench_phase3_true_distance.R 3 2e8

library(cayleyR)

args   <- commandArgs(trailingOnly = TRUE)
worker <- length(args) >= 1L && args[[1]] == "--run"

n_seeds <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 6L
budget  <- if (!worker && length(args) >= 2L) as.numeric(args[[2]]) else 5e7

N          <- 4L
p12_budget <- 2e6
timeout_s  <- 2400L
fill_depth <- 6L

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
  replay(s, p12)
}

if (worker) {
  seed <- as.integer(args[[2]])
  out  <- args[[3]]

  st <- handed_state(seed)

  # Fill to the same depth the saturation run used, so the bound compared
  # against the solution is the same number that run reported.
  cayleyR:::cube_kociemba4_fill_phase3_cpp(fill_depth)
  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3
  bound <- cayleyR:::cube_phase3_coord_cpp(st)$prune_bound

  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(st, node_budget = budget)
  secs <- proc.time()[["elapsed"]] - t0

  # The solver reports quarter turns; the phase counts generators, and a half
  # turn is one generator. Both are printed because the comparison against the
  # bound has to be in the phase's own units, not the printed ones.
  gens <- if (r$found) length(r$path) else NA_integer_

  writeLines(sprintf("RESULT\t%s\t%d\t%d\t%.0f\t%.1f\t%d\t%s",
                     r$outcome, bound, tb$built_depth, r$nodes, secs,
                     if (is.na(gens)) -1L else gens,
                     if (r$found) paste(r$path, collapse = " ") else ""), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

select_file <- file.path(dirname(tempdir()), "cayleyR_phase3_select.rds")
if (!file.exists(select_file)) {
  stop("run inst/examples/bench_phase3_select.R first", call. = FALSE)
}
sel <- readRDS(select_file)
seeds <- head(sel$seed[!sel$solved], n_seeds)

hr("setup")
cat("seeds        : ", paste(seeds, collapse = ", "), "\n", sep = "")
cat("table depth  : ", fill_depth, " (same as the saturation run)\n", sep = "")
cat("node budget  : ", format(budget, scientific = FALSE, big.mark = ","),
    "\n", sep = "")
cat("\nthe question: is the bound of 6 this state's own distance, or an\n")
cat("entry left by another state that hashed into the same slot?\n")

rows <- list()
for (sd in seeds) {
  cat(sprintf("\n  seed %d ... ", sd))
  flush.console()

  res <- tempfile("p3true", fileext = ".tsv")
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", sd, shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = timeout_s)
  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  unlink(res)

  if (length(line) != 1L) {
    cat(sprintf("no result (over %d s)\n", timeout_s))
    rows[[length(rows) + 1L]] <- data.frame(
      seed = sd, outcome = "timeout", bound = NA_integer_,
      built = NA_integer_, nodes = NA_real_, seconds = as.numeric(timeout_s),
      moves = NA_integer_, path = NA_character_, stringsAsFactors = FALSE)
    next
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  r <- data.frame(seed = sd, outcome = f[[2]], bound = as.integer(f[[3]]),
                  built = as.integer(f[[4]]), nodes = as.numeric(f[[5]]),
                  seconds = as.numeric(f[[6]]),
                  moves = as.integer(f[[7]]),
                  path = if (length(f) >= 8L) f[[8]] else NA_character_,
                  stringsAsFactors = FALSE)
  if (r$moves < 0L) r$moves <- NA_integer_
  cat(sprintf("%-10s  bound %d  %s moves  %s nodes  %.0f s\n",
              r$outcome, r$bound,
              if (is.na(r$moves)) "?" else as.character(r$moves),
              format(r$nodes, big.mark = ",", scientific = FALSE), r$seconds))
  rows[[length(rows) + 1L]] <- r
}

tab <- do.call(rbind, rows)

hr("bound against the real distance")
print(tab[, c("seed", "outcome", "bound", "moves", "nodes", "seconds")],
      row.names = FALSE)

hr("verdict")
done <- tab[!is.na(tab$moves), ]
if (!nrow(done)) {
  cat("None finished inside the budget. The distance is beyond what this\n")
  cat("budget reaches, which is itself worth knowing: whatever the bound of\n")
  cat("6 refers to, it is not stopping the search from having to walk very\n")
  cat("deep. Raise the budget or the timeout and run it again.\n")
} else {
  exact <- sum(done$moves == done$bound)
  cat(sprintf("%d of %d finished. bound equalled the solution length in %d.\n",
              nrow(done), nrow(tab), exact))
  cat(sprintf("mean solution %.1f moves against a bound of %.1f\n",
              mean(done$moves), mean(done$bound)))
  cat("\n")
  if (exact == nrow(done)) {
    cat("The bound was each state's own distance. The table was right, and\n")
    cat("what remains is the cost of filling deep enough to say so.\n")
  } else {
    cat("The bound understates the real distance, by ")
    cat(sprintf("%.1f moves on average.\n", mean(done$moves - done$bound)))
    cat("So a bound of 6 was not these cubes' distance, and the reading that\n")
    cat("depth alone explains the failure does not stand. Two things produce\n")
    cat("this and this run does not separate them: a slot holding another\n")
    cat("state's entry, or a coordinate too coarse to tell near from far.\n")
    cat("Both point away from prune_depth_bonus and towards the addressing.\n")
  }
}
