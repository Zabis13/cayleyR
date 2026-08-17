#!/usr/bin/env Rscript
# Does filling the phase 3 prune table one level deeper rescue the states it
# cannot currently solve?
#
# The states to measure it on come from bench_phase3_select.R, which runs one
# process per seed from an empty table. That indirection is not fussiness. This
# script used to pick its own states by reading the prune bound and keeping
# those scoring >= 5, and that rule can never fire: cube_phase3_coord_cpp()
# reads the table without building it, so on a fresh process built_depth is 0
# and get() returns 1 for everything not already at a goal. Measured, seeds
# 1..12: every bound was 0 or 1. The run stopped with "no failing states found".
#
# The same reading also undoes what diag_phase3_coord.R appeared to show. Its
# bounds of 6 were not a property of those cubes -- they were the depth the
# singleton table happened to have reached by then, growing as each search ran.
# "bound decides the outcome" was a measurement of run order.
#
# What survives is the mechanism itself, and bench_phase3_select.R gives it
# direct support: all 23 failing seeds grow the table to depth 4 and still
# exhaust the budget, while the ones that solve mostly finish at depth 0-3. The
# table is being built; depth 4 is not enough against a branching factor near
# twelve.
#
# The depth is set in kociemba_core.h:
#
#     int fill_to = limit / 2 + limits.prune_depth_bonus;
#
# so prune_depth_bonus is the dial, and it reaches phase 3 alone.
#
# This was measured once before and the deeper table lost -- but for a reason
# that no longer holds. Phase 2's table had no ceiling then (max_size = 0 reads
# as "no limit"), so it grew to 268M slots and was rebuilt from scratch on
# every growth; the runs died on that, not on phase 3. Phases 1 and 2 have
# ceilings now and phase 3's table grows once. The old result does not carry
# over.
#
# What is measured, per state and per bonus:
#
#   bound_before   the table's verdict on the state before the search runs
#   bound_after    its verdict once the search has grown and filled the table
#   outcome        found, exhausted or no_solution
#   nodes          what the search actually spent
#
# The causal test is bound_before against outcome on the SAME state: if a state
# that scored the stub at bonus 0 scores a real distance at bonus 1 and then
# solves in thousands of nodes rather than millions, the mechanism is confirmed
# directly. If the bound stays at the stub however deep the table is filled,
# that is a second finding -- the deepening is not covering this part of the
# coordinate -- and not a repeat of the old result.
#
# Run with:  Rscript inst/examples/bench_phase3_bonus.R
#            Rscript inst/examples/bench_phase3_bonus.R 6      # 6 states
#            Rscript inst/examples/bench_phase3_bonus.R 6 3    # bonuses 0..3

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)

# Re-invoked as a worker: "--run <seed> <bonus> <outfile>". One (state, bonus)
# per process. The prune table is a singleton that survives between calls and
# keeps whatever depth the last run left it at, so two bonuses timed in one
# session would measure each other's leftovers.
worker <- length(args) >= 1 && args[[1]] == "--run"

n_states  <- if (!worker && length(args) >= 1) as.integer(args[[1]]) else 4L
max_bonus <- if (!worker && length(args) >= 2) as.integer(args[[2]]) else 2L

N           <- 4L
node_budget <- 2e6
bonuses     <- seq(0L, max_bonus)
worker_timeout <- 300L

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# A state phases 1 and 2 hand over, built from a seed so that a worker can
# reproduce it exactly rather than have it serialised across.
handed_state <- function(seed) {
  set.seed(seed)
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = node_budget)
  list(state = replay(s, p12), word = w)
}

if (worker) {
  seed  <- as.integer(args[[2]])
  bonus <- as.integer(args[[3]])
  out   <- args[[4]]

  h <- handed_state(seed)

  # The depth the worker starts at, reported rather than trusted: bonuses are
  # only comparable to each other if none of them inherited a table.
  depth_in <- cayleyR:::cube_kociemba4_tables_cpp()$phase3$built_depth

  # Before the search: what the table already knows about this state. On a
  # fresh process this is the stub for anything off a goal -- the coordinate
  # call reads the table, it does not build it -- so it is bound_after that
  # carries the information here.
  before <- cayleyR:::cube_phase3_coord_cpp(h$state)

  t0 <- proc.time()[["elapsed"]]
  r <- cayleyR:::cube_kociemba4_phase3_cpp(h$state, node_budget = node_budget,
                                           prune_depth_bonus = bonus)
  secs <- proc.time()[["elapsed"]] - t0

  # After: the search has grown and filled the table, so the same state may
  # score differently now. This is the number that says whether deepening
  # reached the part of the coordinate this state lives in.
  after <- cayleyR:::cube_phase3_coord_cpp(h$state)
  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3

  writeLines(sprintf("RESULT\t%s\t%.0f\t%.4f\t%d\t%d\t%d\t%d\t%.0f\t%d\t%d",
                     r$outcome, r$nodes, secs, length(r$path),
                     before$prune_bound, after$prune_bound,
                     tb$built_depth, tb$size, tb$n_grows, depth_in), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

run_one <- function(seed, bonus) {
  cat(sprintf("    bonus %d ... ", bonus))
  flush.console()

  res <- tempfile("p3bonus", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", seed, bonus, shQuote(res)),
          stdout = NULL, stderr = NULL, timeout = worker_timeout)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) {
    cat(sprintf("no result (over %d s)\n", worker_timeout))
    return(list(outcome = "timeout", nodes = NA_real_,
                seconds = as.numeric(worker_timeout), moves = 0L,
                bound_before = NA_integer_, bound_after = NA_integer_,
                built_depth = NA_integer_, size = NA_real_,
                n_grows = NA_integer_, depth_in = NA_integer_,
                solved = FALSE))
  }
  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  out <- list(outcome = f[[2]], nodes = as.numeric(f[[3]]),
              seconds = as.numeric(f[[4]]), moves = as.integer(f[[5]]),
              bound_before = as.integer(f[[6]]),
              bound_after = as.integer(f[[7]]),
              built_depth = as.integer(f[[8]]),
              size = as.numeric(f[[9]]), n_grows = as.integer(f[[10]]),
              depth_in = as.integer(f[[11]]),
              solved = f[[2]] == "found")
  cat(sprintf("%-11s  bound %d->%d  depth %d  %s nodes  %.1f s\n",
              out$outcome, out$bound_before, out$bound_after, out$built_depth,
              format(out$nodes, scientific = FALSE, big.mark = ","),
              out$seconds))
  out
}

hr("setup")
cat("states       : ", n_states, " handed over by phases 1 and 2\n", sep = "")
cat("bonuses      : ", paste(bonuses, collapse = ", "), "\n", sep = "")
cat("node budget  : ", format(node_budget, scientific = FALSE, big.mark = ","),
    "\n", sep = "")
cat("\nbound is what the prune table says the distance is. built_depth + 1\n")
cat("means the state is not in the table at all -- the stub, not a distance.\n")
cat("Each (state, bonus) runs in its own process: the table is a singleton\n")
cat("and would otherwise carry its depth over from the run before.\n")

rows <- list()

# The states worth measuring are the ones that fail today, and most do not.
# bench_phase3_select.R finds them, one process per seed; its table is reused
# if it is there and the selection is rerun if it is not, so this script never
# measures a list whose provenance it cannot state.
hr("states that fail at bonus 0")

select_file <- file.path(dirname(tempdir()), "cayleyR_phase3_select.rds")
select_script <- file.path(dirname(this_file), "bench_phase3_select.R")

sel <- NULL
if (file.exists(select_file)) {
  sel <- readRDS(select_file)
  cat("read from ", select_file, "\n", sep = "")
} else if (file.exists(select_script)) {
  cat("no selection on disk; running bench_phase3_select.R first.\n")
  cat("(one process per seed, a few minutes)\n")
  system2(file.path(R.home("bin"), "Rscript"), shQuote(select_script),
          stdout = NULL, stderr = NULL)
  if (file.exists(select_file)) sel <- readRDS(select_file)
}
if (is.null(sel)) {
  stop("no failing states: run inst/examples/bench_phase3_select.R first",
       call. = FALSE)
}

# The selection is only comparable across seeds if every worker started from
# the same table. bench_phase3_select.R records that; check it rather than
# assume it, since a mixed selection would put the old confound back.
if ("depth_in" %in% names(sel)) {
  d_in <- unique(sel$depth_in[!is.na(sel$depth_in)])
  if (length(d_in) != 1L) {
    stop("selection is not comparable: workers entered at built_depth ",
         paste(sort(d_in), collapse = ", "), call. = FALSE)
  }
  cat(sprintf("selection ran with every worker at built_depth = %d.\n", d_in))
}

seeds <- head(sel$seed[!sel$solved], n_states)
if (!length(seeds)) stop("no failing states in the selection", call. = FALSE)

for (sd in seeds) {
  h <- handed_state(sd)
  cat(sprintf("  seed %3d  p12 %2d  scramble %s\n", sd,
              sel$p12_moves[match(sd, sel$seed)], paste(h$word, collapse = " ")))
}

for (i in seq_along(seeds)) {
  sd <- seeds[[i]]
  h <- handed_state(sd)
  hr(paste0("state ", i, " (seed ", sd, ")"))
  cat("  scramble: ", paste(h$word, collapse = " "), "\n", sep = "")

  for (b in bonuses) {
    r <- run_one(sd, b)
    rows[[length(rows) + 1L]] <- data.frame(
      state = i, seed = sd, bonus = b, outcome = r$outcome,
      solved = r$solved, nodes = r$nodes, seconds = round(r$seconds, 2),
      moves = r$moves, bound_before = r$bound_before,
      bound_after = r$bound_after, built_depth = r$built_depth,
      n_grows = r$n_grows, depth_in = r$depth_in,
      stringsAsFactors = FALSE)
  }
}

tab <- do.call(rbind, rows)

hr("isolation check")
d_in <- unique(tab$depth_in[!is.na(tab$depth_in)])
if (length(d_in) == 1L) {
  cat(sprintf("every worker entered with built_depth = %d. Bonuses are\n", d_in))
  cat("comparable to each other.\n")
} else {
  cat("WORKERS DID NOT START ALIKE -- depth_in took values ",
      paste(sort(d_in), collapse = ", "), ".\n", sep = "")
  cat("Nothing below can be read as an effect of the bonus. Fix this first.\n")
}

hr("every run")
print(tab[, c("state", "bonus", "outcome", "bound_before", "bound_after",
              "built_depth", "nodes", "seconds")], row.names = FALSE)

hr("the causal test")

# The comparison this run exists for, state by state. Did the bound stop being
# the stub, and did the state then solve?
cat("For each state: what the bound was before the search, and what happened.\n\n")
cat(sprintf("  %-6s %-6s %-13s %-12s %-9s %s\n",
            "state", "bonus", "bound before", "bound after", "solved",
            "nodes"))
for (i in seq_len(nrow(tab))) {
  cat(sprintf("  %-6d %-6d %-13d %-12d %-9s %s\n",
              tab$state[i], tab$bonus[i], tab$bound_before[i],
              tab$bound_after[i],
              if (tab$solved[i]) "yes" else "NO",
              format(tab$nodes[i], scientific = FALSE, big.mark = ",")))
}

hr("by bonus")
by_bonus <- do.call(rbind, lapply(split(tab, tab$bonus), function(d) {
  data.frame(bonus = d$bonus[1],
             solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
             mean_bound_before = round(mean(d$bound_before, na.rm = TRUE), 1),
             mean_depth = round(mean(d$built_depth, na.rm = TRUE), 1),
             mean_nodes = round(mean(d$nodes, na.rm = TRUE)),
             mean_secs = round(mean(d$seconds, na.rm = TRUE), 1),
             stringsAsFactors = FALSE)
}))
print(by_bonus, row.names = FALSE)

hr("verdict")

base <- tab[tab$bonus == 0L, ]
cat(sprintf("at bonus 0: %d of %d solved, mean bound before the search %.1f\n",
            sum(base$solved), nrow(base),
            mean(base$bound_before, na.rm = TRUE)))

for (b in bonuses[-1]) {
  d <- tab[tab$bonus == b, ]
  cat(sprintf("at bonus %d: %d of %d solved, mean bound %.1f, mean depth %.1f\n",
              b, sum(d$solved), nrow(d), mean(d$bound_before, na.rm = TRUE),
              mean(d$built_depth, na.rm = TRUE)))
}

top <- tab[tab$bonus == max(bonuses), ]
cat("\n")
if (sum(top$solved) > sum(base$solved)) {
  cat("The deeper table rescues states the shallow one could not solve, and\n")
  cat("the bound column says why: states that scored the stub now score a\n")
  cat("real distance, so the search has a gradient again. The mechanism is\n")
  cat("confirmed -- fill_to = limit/2 is too shallow for phase 3, whose\n")
  cat("branching factor is near twelve.\n")
} else if (all(top$bound_before >= top$built_depth, na.rm = TRUE)) {
  cat("Deepening did not move the bound: these states still score the stub\n")
  cat("however far the table is filled. That is a finding of its own, not a\n")
  cat("repeat of the earlier one -- the levels being added do not reach the\n")
  cat("part of the coordinate these states live in. Look at which states the\n")
  cat("fill actually visits: extend_prune_table walks forward from the goals\n")
  cat("with the canonical FSM, and if that walk cannot reach these states,\n")
  cat("no depth will put them in the table.\n")
} else {
  cat("The deeper table did not rescue them, though the bound did move.\n")
  cat("Compare nodes at each bonus: if they fell without the outcome\n")
  cat("changing, the heuristic improved but not enough, and the budget is\n")
  cat("what to raise next.\n")
}
