# A ladder of node budgets, swept across a list of attempts. Shared by
# cube_solve4_cascade() and cube_kociemba4_reduce().
#
# Phase 3 of a 4x4x4 reduction either succeeds cheaply or fails expensively, and
# `exhausted` is a statement about the leash rather than about the cube -- so
# calling an orientation hopeless means letting it spend the whole budget, while
# one that works usually says so early. Measured at 5e7 on one cube:
#
#     orientation (-)    exhausted  50,000,000 nodes   43.3 s
#     orientation 1y     exhausted  50,000,000 nodes   41.0 s
#     orientation 1x     found      32,806,505 nodes   25.9 s
#
# So every attempt runs at a small budget before any runs at a large one, and
# nothing is thrown away -- the search restarts from scratch at each rung.
# Rungs of 2e6/1e7/5e7 were worse than no ladder for a cube that succeeds at 2.4
# million nodes: the first rung fell just short and the solve went 6.2 -> 12.7 s.
# The rungs have to clear the cheap successes they exist to catch.

# Turn fractions into node counts: smallest first, no repeats. Fractions close
# enough to round to the same node count at a given budget become one rung
# rather than two identical ones, and a fraction small enough to round to zero
# becomes a rung of one node rather than a search that cannot begin.
.budget_rungs <- function(node_budget, budget_steps) {
  sort(unique(pmax(1, round(as.numeric(node_budget) * as.numeric(budget_steps)))))
}

# Sweep `attempts` at each rung in turn, stopping at the first success.
#
#   attempts   a list; each element is whatever `try_one` understands
#   try_one    function(attempt, budget) -> list(ok, value)
#              `ok` decides whether the sweep stops. `value` is handed back
#              untouched, so the caller decides what a result is.
#   on_result  optional function(attempt, budget, res), called after every
#              attempt including the failures. For logging and for `attempts`
#              records; its return value is ignored.
#
# Returns the first result whose `ok` was TRUE, or NULL when no rung produced
# one -- the caller decides what to do with that, since falling back is a
# policy and not a property of the sweep.
.budget_sweep <- function(attempts, rungs, try_one, on_result = NULL,
                          workers = 1L) {
  for (budget in rungs) {
    if (workers > 1L) {
      hit <- .budget_rung_parallel(attempts, budget, try_one, on_result,
                                   workers)
      if (!is.null(hit)) return(hit)
      next
    }
    for (a in attempts) {
      res <- try_one(a, budget)
      if (!is.null(on_result)) on_result(a, budget, res)
      if (isTRUE(res$ok)) return(res$value)
    }
  }
  NULL
}

# One rung, its attempts run several at a time. They are independent -- each a
# phase-3 search from its own state -- and on a slow cube almost all of them
# fail: measured, the fifteen candidates before the winner cost 280 s together.
# The saving is bounded by where the winner sits, not by the worker count.
#
# Forking rather than threads because of the prune table: a process-wide 256MB
# singleton, shared copy-on-write, so N workers cost one copy as long as nobody
# writes. A search that grows its own table does write, and then N workers cost
# N tables -- so `workers` is the caller's decision, not a guess made here.
# Windows has no fork and mclapply degrades to sequential.
.budget_rung_parallel <- function(attempts, budget, try_one, on_result,
                                  workers) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    for (a in attempts) {
      res <- try_one(a, budget)
      if (!is.null(on_result)) on_result(a, budget, res)
      if (isTRUE(res$ok)) return(res$value)
    }
    return(NULL)
  }

  # Chunked rather than all at once. A rung of sixteen attempts run sixteen
  # abreast on six cores finishes no sooner than the slowest of them, and pays
  # for every one of the sixteen even when the second was a winner. A chunk of
  # `workers` pays at most one chunk of waste past the answer.
  n <- length(attempts)
  idx <- seq_len(n)
  for (from in seq(1L, n, by = workers)) {
    to <- min(from + workers - 1L, n)
    chunk <- idx[from:to]
    res_list <- parallel::mclapply(attempts[chunk], try_one, budget,
                                   mc.cores = min(workers, length(chunk)),
                                   mc.preschedule = FALSE)
    for (k in seq_along(chunk)) {
      res <- res_list[[k]]
      # A worker that died takes back an error rather than a result. Treated as
      # a failed attempt rather than allowed to abort the sweep: the remaining
      # candidates are unaffected by whatever went wrong in that one, and the
      # cascade's fallback is there for the case where none of them work.
      if (inherits(res, "try-error") || !is.list(res)) {
        res <- list(ok = FALSE, r3 = list(outcome = "worker-error", nodes = 0))
      }
      if (!is.null(on_result)) on_result(attempts[[chunk[k]]], budget, res)
      if (isTRUE(res$ok)) return(res$value)
    }
  }
  NULL
}
