# Benchmark: the 4x4x4 reduction, against a recorded baseline.
#
# This is a measurement, not a test -- it asserts nothing and never fails. It
# prints what the three reduction phases cost today beside what they cost at a
# known point in the past, so that a change to the phases can be judged by
# numbers rather than by whether the test suite still passes.
#
# Why node counts and not just wall time: the phases are searches, and the
# thing a change to a generator set or a goal set alters is how many nodes get
# visited. Time follows from that, but it also follows from what else the
# machine is doing. A change that leaves the node counts identical did not
# alter the search, whatever the clock says -- that is the check this script
# exists to make possible.
#
# Run with:
#   Rscript inst/examples/bench_kociemba4.R

library(cayleyR)

# ---- The baseline ---------------------------------------------------------
#
# Recorded 2026-08-12, on phase 3 as it stood before the twips port: twenty
# generators (all six inner half turns) against a single goal. Seed 1 is the
# scramble that spends the whole budget -- it is kept in deliberately, because
# a change that makes it finish is exactly the kind of change worth seeing.
#
# Update these numbers only together with a note in TODO.md saying what moved
# them and why; a silently edited baseline measures nothing.

BASELINE <- data.frame(
  seed    = 7001:7008,
  len     = c(0, 7, 5, 29, 25, 3, 13, 4),
  nodes   = c(51348346, 777, 827, 4120324, 34757, 47, 514, 56),
  reduced = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

SCRAMBLE_MOVES <- 5

# Nodes between progress lines. Two million is roughly a line a second on the
# hard scramble and no lines at all on the easy ones, which is what is wanted:
# the point is to show that a long search is alive, not to narrate a short one.
PROGRESS_EVERY <- 2e6

# ---- The measurement ------------------------------------------------------

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)

apply_path <- function(state, path) {
  for (mv in path) state <- state[moves4[[mv]]]
  state
}

measure_one <- function(seed) {
  set.seed(seed)
  state <- generate_state(group = cube_group(4), n_moves = SCRAMBLE_MOVES)

  # Announced before the search rather than after it: a scramble that spends
  # the whole budget takes the best part of a minute, and a line that appears
  # only on success leaves the screen looking hung during exactly the case
  # worth watching.
  cat(sprintf("seed %d\n", seed))
  flush.console()

  started <- Sys.time()
  path <- cube_kociemba4_reduce(state, progress_every = PROGRESS_EVERY)
  elapsed <- as.numeric(Sys.time() - started, units = "secs")

  report <- cube_kociemba4_report()
  reduced <- if (length(path)) cube_is_reduced(apply_path(state, path))
             else cube_is_reduced(state)

  # Per phase, because that is where the diagnosis lives. A total node count
  # says the search was expensive; it does not say which phase was expensive,
  # and the three differ in what would fix them.
  cat(sprintf("  -> %6.2fs  len %2d  reduced %-5s   [p1 %s %d | p2 %s %d | p3 %s %d]\n\n",
              elapsed, length(path), reduced,
              report$phase1, report$phase1_nodes,
              report$phase2, report$phase2_nodes,
              report$phase3, report$phase3_nodes))
  flush.console()

  data.frame(
    seed    = seed,
    len     = length(path),
    nodes   = report$phase1_nodes + report$phase2_nodes + report$phase3_nodes,
    n1      = report$phase1_nodes,
    n2      = report$phase2_nodes,
    n3      = report$phase3_nodes,
    ph1     = report$phase1,
    ph2     = report$phase2,
    ph3     = report$phase3,
    # An empty path means the search gave up; the state is reduced only if it
    # already was. Calling apply_path() with nothing would say TRUE for a
    # failure, which is the one answer that must not be given here.
    reduced = reduced,
    sec     = round(elapsed, 2),
    stringsAsFactors = FALSE
  )
}

now <- do.call(rbind, lapply(BASELINE$seed, measure_one))
cat("\n")

# ---- The comparison -------------------------------------------------------

cmp <- data.frame(
  seed       = now$seed,
  len_base   = BASELINE$len,
  len_now    = now$len,
  nodes_base = BASELINE$nodes,
  nodes_now  = now$nodes,
  sec        = now$sec,
  reduced    = now$reduced
)
print(cmp, row.names = FALSE)

ratio <- function(a, b) if (b == 0) NA_real_ else round(100 * a / b, 1)

cat("\n")
cat(sprintf("reduced   baseline %d of %d, now %d of %d\n",
            sum(BASELINE$reduced), nrow(BASELINE),
            sum(now$reduced), nrow(now)))
cat(sprintf("nodes     baseline %d, now %d (%s%%)\n",
            sum(BASELINE$nodes), sum(now$nodes),
            format(ratio(sum(now$nodes), sum(BASELINE$nodes)))))
cat(sprintf("path len  baseline %d, now %d\n", sum(BASELINE$len), sum(now$len)))
cat(sprintf("seconds   %.2f total\n", sum(now$sec)))

# Where the nodes went. The baseline is a total only, so there is nothing to
# compare these against -- they say which phase to look at, not whether it got
# better.
cat(sprintf("\nby phase  1: %d   2: %d   3: %d\n",
            sum(now$n1), sum(now$n2), sum(now$n3)))
exhausted <- now[now$ph1 != "found" | now$ph2 != "found" | now$ph3 != "found", ]
if (nrow(exhausted)) {
  cat("\nphases that did not finish:\n")
  for (i in seq_len(nrow(exhausted))) {
    cat(sprintf("  seed %d: phase1 %s, phase2 %s, phase3 %s\n",
                exhausted$seed[i], exhausted$ph1[i],
                exhausted$ph2[i], exhausted$ph3[i]))
  }
}

# Worth saying out loud rather than leaving to be spotted in the table: a
# scramble that used to reduce and no longer does is a regression whatever the
# node counts did.
lost <- which(BASELINE$reduced & !now$reduced)
if (length(lost)) {
  cat(sprintf("\nREGRESSION: seed(s) %s reduced at baseline and do not now\n",
              paste(BASELINE$seed[lost], collapse = ", ")))
}
gained <- which(!BASELINE$reduced & now$reduced)
if (length(gained)) {
  cat(sprintf("\nseed(s) %s now reduce and did not at baseline\n",
              paste(BASELINE$seed[gained], collapse = ", ")))
}
