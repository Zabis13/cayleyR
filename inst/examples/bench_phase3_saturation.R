#!/usr/bin/env Rscript
# Is the phase 3 prune table full, and does that explain why deepening it does
# nothing?
#
# bench_phase3_bonus.R measured, on seed 4:
#
#     bonus 0   table to depth 5   2,000,001 nodes    7.0 s
#     bonus 1   table to depth 6   2,000,001 nodes   80.1 s
#
# Eleven times the fill cost, the same search, and the bound stayed at the stub
# (built_depth + 1) both times. A level was walked and the state being scored
# learned nothing from it.
#
# The table is addressed as table[h & mask] -- a 64-bit hash of the derived
# state truncated to the table's width. That is not Kociemba's addressing: his
# coordinate is composite and gives every state in the phase its own slot. Here
# phase 3's coordinate space, on the order of 1e11, is being folded into 2^24
# slots, about ten thousand states per slot. set_if_empty keeps the first value
# written, which keeps the bound admissible -- the value kept was reached at a
# shallower depth, so it can only underestimate -- but the arriving state's own
# distance is not approximated, it is discarded.
#
# Two causes produce the same stub, and the outcome so far cannot separate them:
#
#   the fill never reached this state
#   the fill reached it and the slot was taken by a state that hashed alike
#
# so this measures the write path rather than the read path. Per level:
#
#   filled / size    how full the table is
#   n_writes         entries the level actually recorded
#   waste_ratio      the share of the walk that landed on a taken slot
#   depth_counts     where the recorded mass sits
#
# The distinguishing prediction. If the table is saturating, n_writes flattens
# while n_visits keeps climbing at the branching factor, and waste_ratio goes to
# one: each new level costs 11.5x and records almost nothing. If instead the
# table is far from full and the failing states still score the stub, the fill
# is not reaching them, and the FSM or the goal set is where to look next.
#
# Run with:  Rscript inst/examples/bench_phase3_saturation.R
#            Rscript inst/examples/bench_phase3_saturation.R 7   # depths 1..7

library(cayleyR)

args      <- commandArgs(trailingOnly = TRUE)
worker    <- length(args) >= 1L && args[[1]] == "--run"
max_depth <- if (!worker && length(args) >= 1L) as.integer(args[[1]]) else 6L

N          <- 4L
p12_budget <- 2e6
timeout_s  <- 1800L

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

# One depth per process. The table is a singleton and each fill leaves it
# deeper, so depths measured in one session would each report the leftovers of
# the one before -- the same confound that made the earlier bound readings
# meaningless.
if (worker) {
  depth <- as.integer(args[[2]])
  out   <- args[[3]]
  seeds <- as.integer(strsplit(args[[4]], ",", fixed = TRUE)[[1]])

  t0 <- proc.time()[["elapsed"]]
  cayleyR:::cube_kociemba4_fill_phase3_cpp(depth)
  secs <- proc.time()[["elapsed"]] - t0

  tb <- cayleyR:::cube_kociemba4_tables_cpp()$phase3

  # What the table says about the states that fail, now that it is this deep.
  # A stub here while the table is far from full means the fill is not reaching
  # them; a stub while it is full means their slot was taken.
  bounds <- vapply(seeds, function(sd) {
    as.integer(cayleyR:::cube_phase3_coord_cpp(handed_state(sd))$prune_bound)
  }, integer(1))

  writeLines(sprintf("RESULT\t%d\t%.2f\t%.0f\t%.0f\t%.0f\t%.0f\t%.0f\t%d\t%s\t%s",
                     depth, secs, tb$size, tb$filled, tb$n_visits,
                     tb$n_writes, tb$n_collisions, tb$built_depth,
                     paste(tb$depth_counts, collapse = ","),
                     paste(bounds, collapse = ",")), out)
  quit(save = "no")
}

this_file <- sub("^--file=", "",
                 grep("^--file=", commandArgs(FALSE), value = TRUE)[1])

select_file <- file.path(dirname(tempdir()), "cayleyR_phase3_select.rds")
if (!file.exists(select_file)) {
  stop("run inst/examples/bench_phase3_select.R first", call. = FALSE)
}
sel <- readRDS(select_file)
fail_seeds <- head(sel$seed[!sel$solved], 6L)

run_depth <- function(depth) {
  res <- tempfile("p3sat", fileext = ".tsv")
  on.exit(unlink(res), add = TRUE)
  system2(file.path(R.home("bin"), "Rscript"),
          c(shQuote(this_file), "--run", depth, shQuote(res),
            paste(fail_seeds, collapse = ",")),
          stdout = NULL, stderr = NULL, timeout = timeout_s)

  line <- if (file.exists(res))
            grep("^RESULT\t", readLines(res, warn = FALSE), value = TRUE)
          else character(0)
  if (length(line) != 1L) return(NULL)

  f <- strsplit(line[[1]], "\t", fixed = TRUE)[[1]]
  list(depth = as.integer(f[[2]]), seconds = as.numeric(f[[3]]),
       size = as.numeric(f[[4]]), filled = as.numeric(f[[5]]),
       visits = as.numeric(f[[6]]), writes = as.numeric(f[[7]]),
       collisions = as.numeric(f[[8]]), built = as.integer(f[[9]]),
       counts = as.numeric(strsplit(f[[10]], ",", fixed = TRUE)[[1]]),
       bounds = as.integer(strsplit(f[[11]], ",", fixed = TRUE)[[1]]))
}

hr("setup")
cat("depths       : 1 .. ", max_depth, ", one process each\n", sep = "")
cat("failing seeds: ", paste(fail_seeds, collapse = ", "), "\n", sep = "")
cat("\nthe table is addressed as table[h & mask]: a hash folded into the\n")
cat("table's width, not a composite coordinate. Phase 3's space is около\n")
cat("1e11 states against 2^24 slots.\n")

rows <- list()
for (d in seq_len(max_depth)) {
  cat(sprintf("\n  depth %d ... ", d))
  flush.console()
  r <- run_depth(d)
  if (is.null(r)) {
    cat(sprintf("no result (over %d s)\n", timeout_s))
    break
  }
  cat(sprintf("%.1f s  filled %s of %s (%.1f%%)  waste %.3f\n",
              r$seconds,
              format(r$filled, big.mark = ",", scientific = FALSE),
              format(r$size, big.mark = ",", scientific = FALSE),
              100 * r$filled / r$size,
              if (r$visits) r$collisions / r$visits else 0))
  rows[[length(rows) + 1L]] <- r
}
if (!length(rows)) stop("nothing measured", call. = FALSE)

hr("per level")
tab <- do.call(rbind, lapply(rows, function(r) data.frame(
  depth = r$depth, seconds = round(r$seconds, 1),
  visits = r$visits, writes = r$writes,
  fill_pct = round(100 * r$filled / r$size, 2),
  waste = round(if (r$visits) r$collisions / r$visits else 0, 3),
  stringsAsFactors = FALSE)))
print(tab, row.names = FALSE)

hr("what each level added")
# The question a level has to answer: did it record anything the level before
# did not already have?
cat(sprintf("  %-6s %-14s %-14s %-10s %s\n",
            "depth", "new entries", "x visits", "x seconds", "waste"))
for (i in seq_along(rows)) {
  prev <- if (i > 1L) rows[[i - 1L]] else NULL
  new_entries <- rows[[i]]$filled - (if (is.null(prev)) 0 else prev$filled)
  fv <- if (is.null(prev) || !prev$visits) NA else rows[[i]]$visits / prev$visits
  fs <- if (is.null(prev) || !prev$seconds) NA else rows[[i]]$seconds / prev$seconds
  cat(sprintf("  %-6d %-14s %-14s %-10s %.3f\n",
              rows[[i]]$depth,
              format(new_entries, big.mark = ",", scientific = FALSE),
              if (is.na(fv)) "-" else sprintf("%.1f", fv),
              if (is.na(fs)) "-" else sprintf("%.1f", fs),
              if (rows[[i]]$visits) rows[[i]]$collisions / rows[[i]]$visits else 0))
}

hr("the failing states, level by level")
cat("bound at each depth. built_depth + 1 is the stub -- not a distance.\n\n")
cat(sprintf("  %-6s %-8s %s\n", "depth", "stub is", "bounds"))
for (r in rows) {
  cat(sprintf("  %-6d %-8d %s\n", r$depth, r$built + 1L,
              paste(sprintf("%2d", r$bounds), collapse = " ")))
}

hr("verdict")
last <- rows[[length(rows)]]
fill_pct <- 100 * last$filled / last$size
still_stub <- sum(last$bounds >= last$built + 1L)

cat(sprintf("at depth %d: table %.1f%% full, waste %.3f, %d of %d failing\n",
            last$depth, fill_pct,
            if (last$visits) last$collisions / last$visits else 0,
            still_stub, length(last$bounds)))
cat("states still scoring the stub.\n\n")

if (fill_pct > 80) {
  cat("The table is saturated. Deeper levels land on slots that are already\n")
  cat("taken, so they cost the full branching factor in time and record\n")
  cat("almost nothing -- which is exactly the 11x for nothing that the bonus\n")
  cat("sweep measured. This is not a depth that can be tuned: with 1e11\n")
  cat("states over 2^24 slots the hash is the limit, and raising max_size to\n")
  cat("2^28 buys at most sixteen times against a shortfall of four orders.\n")
  cat("What it points at is the addressing -- a composite coordinate that\n")
  cat("gives each state its own slot, or separate tables per orbit combined\n")
  cat("with max, rather than one hash over everything.\n")
} else if (still_stub > 0) {
  cat("The table is NOT full and these states still score the stub, so the\n")
  cat("fill is not reaching them. Saturation is not the cause. Look at what\n")
  cat("the walk covers: extend_prune_table goes forward from the goals under\n")
  cat("the canonical FSM, and if the goal set or the FSM keeps it out of the\n")
  cat("region these states live in, no depth will put them in the table.\n")
} else {
  cat("The failing states are in the table now and score real distances.\n")
  cat("Then depth was the issue after all, and what remains is the cost of\n")
  cat("filling to it -- compare the seconds column against the search time\n")
  cat("it saves.\n")
}
