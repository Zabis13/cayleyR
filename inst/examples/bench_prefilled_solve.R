# Solving a batch of cubes on one prefilled table.
#
# The scenario: a thousand 4x4x4 cubes, one after another, target about ten
# seconds each, with everything built once at the start and shared after.
#
# That works because kociemba4::solver4() is a singleton whose init() begins
# `if (ready) return` -- a table filled once stays filled for the life of the
# process. What DOES reset it is grow_to(), which reallocates and discards the
# contents, so the fill happens at 2^28, the ceiling phase 3 is entitled to.
#
# Fill cost, measured in bench_phase3_prefill.R:
#
#     depth 5      1.4 s       909,754 entries
#     depth 6     11.1 s     8,266,804
#     depth 7     87.8 s    47,572,611     17.7% of the table
#
# Depth 8 is not worth asking for: its writes would be 200 to 270 million
# against 268 million slots, and a table that full learns nothing more.
#
# There is no unfilled baseline here -- every cube runs on the same table, which
# is the batch scenario itself. bench_reduce_budget.R has the contrast.

suppressMessages(library(cayleyR))

args      <- commandArgs(trailingOnly = TRUE)
n_states  <- if (length(args) >= 1) as.integer(args[[1]]) else 10L
fill_depth <- if (length(args) >= 2) as.integer(args[[2]]) else 7L
n_moves   <- 20L

# On by default: the per-cube line prints only after the solve returns, so a
# cube that takes two minutes is otherwise indistinguishable from a hang. The
# third argument turns it off for a collected run; the fourth sets the node
# interval, and at ~1.2M nodes a second 2e6 is a line every couple of seconds.
verbose        <- if (length(args) >= 3) !identical(args[[3]], "quiet") else TRUE
progress_every <- if (length(args) >= 4) as.numeric(args[[4]]) else 2e6
if (!verbose) progress_every <- 0

# The fifth argument runs the candidates several at a time, which is safe here
# because the tables are loaded before the first cube: a search that never grows
# its table never writes to it, so the forks share one 256MB copy. The `grows`
# column at the end is where that assumption is checked. Progress lines go off
# above one worker -- several searches printing into one terminal interleave.
workers <- if (length(args) >= 5) as.integer(args[[5]]) else 1L
if (workers > 1L) progress_every <- 0

# The sixth argument caps the candidates before the cube goes to the algorithmic
# solver; the seventh shuffles their order. Together they are the dial between a
# short answer and a quick one. Shuffling matters because of the cap: with one,
# the order changes WHICH candidates the cube ever sees, not just their order.
max_candidates <- if (length(args) >= 6 && !identical(args[[6]], "all"))
                    as.integer(args[[6]]) else NA_integer_
shuffle <- if (length(args) >= 7) !identical(args[[7]], "no") else FALSE

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

# Defined once and used both by the header below and by profile_one(), so the
# printed setup cannot drift from what the run actually asks for.
NODE_BUDGET  <- 5e7
PREP_BUDGET  <- 5e6
BUDGET_STEPS <- c(0.1, 0.3)

hr("setup")
cat(sprintf("cubes      : %d, scrambled %d moves from solved\n",
            n_states, n_moves))
cat("tables     : whatever build_phase_tables.R left in the table dir\n")
cat("table size : 268,435,456 (2^28) -- the ceiling, so no search regrows it\n")
# The rungs as well as the ceiling: printing "5e7" alone described a run whose
# searches never asked for more than 1.5e7.
cat(sprintf("phase 3    : %s nodes ceiling, rungs %s\n",
            format(NODE_BUDGET, big.mark = ",", scientific = FALSE),
            paste(format(cayleyR:::.budget_rungs(NODE_BUDGET, BUDGET_STEPS),
                         big.mark = ",", scientific = FALSE),
                  collapse = ", ")))
cat(sprintf("phases 1-2 : %s nodes, no ladder\n",
            format(PREP_BUDGET, big.mark = ",", scientific = FALSE)))
cat(sprintf("workers    : %d %s\n", workers,
            if (workers > 1L) "candidates at a time (progress lines off)"
            else "-- one candidate at a time"))
cat(sprintf("candidates : %s%s\n",
            if (is.na(max_candidates)) "all 16, then fall back"
            else sprintf("%d of 16, then cube_solve4", max_candidates),
            if (shuffle) ", shuffled" else ", in build order"))

set.seed(2026)
states <- lapply(seq_len(n_states),
                 function(i) generate_state(group = g, n_moves = n_moves))

# One solve, with the phase breakdown read out of the solver afterwards.
#
# The timing lives in the cascade because that is the only place that sees every
# attempt. Two earlier versions did it here and were wrong: timing the calls
# separately priced phase 1 twice under different depth limits, and reading
# cube_kociemba4_last_cpp() saw only the most recent attempt -- on an eighth-try
# cube it priced the eighth and called the other seven "rest", reporting 31 s of
# phases against 95 s of remainder that was phase 3 seven times over.
#
# The gap between the two budgets is deliberate: node_budget is phase 3's ladder
# top, prep_budget is what phases 1 and 2 spend whole. Handing 5e7 to both let
# phase 2 burn 30 million nodes on the batch's slowest cube.
profile_one <- function(s, node_budget = NODE_BUDGET,
                        prep_budget = PREP_BUDGET) {
  t_all <- proc.time()[["elapsed"]]

  r <- cube_solve4_cascade(s, node_budget = node_budget,
                           prep_budget = prep_budget,
                           workers = workers,
                           max_candidates = max_candidates,
                           shuffle_candidates = shuffle,
                           verbose = verbose, progress_every = progress_every)

  el <- proc.time()[["elapsed"]] - t_all

  # One row per operation, summed across the attempts that ran it. `calls` is
  # what separates an operation that is slow from one that is merely frequent:
  # four cheap phase-1 runs and one expensive phase-3 run are different findings
  # and add up the same way.
  ops <- r$ops
  agg <- data.frame(
    stage = names(tapply(ops$secs, ops$op, sum)),
    secs = as.numeric(tapply(ops$secs, ops$op, sum)),
    calls = as.integer(tapply(ops$secs, ops$op, length)),
    nodes = as.numeric(tapply(ops$nodes, ops$op,
                              function(v) sum(v, na.rm = TRUE))),
    stringsAsFactors = FALSE)
  agg <- agg[order(-agg$secs), , drop = FALSE]
  agg$moves <- NA_real_
  agg$outcome <- vapply(agg$stage, function(nm) {
    o <- ops$outcome[ops$op == nm]
    o <- o[!is.na(o)]
    if (!length(o)) "" else paste(names(table(o)), table(o), sep = " x",
                                  collapse = ", ")
  }, character(1))
  rownames(agg) <- NULL

  list(ok = isTRUE(r$solved) && cube_is_colour_solved(replay(s, r$path)),
       moves = if (isTRUE(r$solved)) length(r$path) else NA_integer_,
       secs = el, method = r$method,
       orientation = if (nzchar(r$orientation)) r$orientation else "(-)",
       # Which of the orientation's phase-2 solutions carried the cube. With
       # several per orientation the rotation alone no longer identifies the
       # attempt that worked, and identifying it is the point: on one measured
       # cube the winner was the sixteenth candidate of sixteen, which is the
       # difference between a six-second solve and a six-minute one.
       candidate = if (!is.null(r$candidate) && nzchar(r$candidate))
                     r$candidate else NA_character_,
       stages = agg[, c("stage", "secs", "moves", "nodes", "outcome")],
       ops = ops)
}

solve_one <- function(s) {
  r <- try(profile_one(s), silent = TRUE)
  if (inherits(r, "try-error")) {
    cat(sprintf("        error: %s", as.character(r)))
    return(list(ok = FALSE, moves = NA_integer_, secs = NA_real_,
                method = "error", orientation = NA_character_,
                candidate = NA_character_, stages = NULL))
  }
  r
}

# ---- The tables, once -------------------------------------------------------
hr("the tables")

# All three phases, not phase 3 alone: profiling one 36.2 s cube showed phase 3
# taking 5.5 s with its table loaded while phases 1 and 2 took 30.6 with theirs
# filled lazily and discarded by grow_to(). build_phase_tables.R writes the
# files, this only loads them, and a missing file just falls back to the lazy
# fill. Loading also pins min_size and max_size, which is what stops a search
# from regrowing the table and throwing the contents away.
table_dir <- "/mnt/Data2/DS_projects/phase3"
fill_secs <- 0
loaded <- rep(FALSE, 3)

for (ph in 1:3) {
  # The depth in the name is the depth reached, which is not always the depth
  # asked for: phase 2's fill stops at 1. So the newest matching file wins
  # rather than a name built from fill_depth.
  cand <- Sys.glob(file.path(table_dir, sprintf("phase%d_d*.bin", ph)))
  if (length(cand) == 0) {
    cat(sprintf("  phase %d: no file in %s -- fills lazily during the search\n",
                ph, table_dir))
    next
  }
  # Deepest first: d10 sorts before d7 alphabetically, so the depth is parsed
  # rather than sorted on.
  d <- as.integer(sub("^.*_d([0-9]+)\\.bin$", "\\1", cand))
  cand <- cand[order(d, decreasing = TRUE)]

  t0 <- proc.time()[["elapsed"]]
  ld <- cayleyR:::cube_kociemba4_load_phase_cpp(cand[1], ph)
  el <- proc.time()[["elapsed"]] - t0
  fill_secs <- fill_secs + el

  if (isTRUE(ld$ok)) {
    loaded[ph] <- TRUE
    cat(sprintf("  phase %d: %s, depth %d, %s entries, %.1f%% full (%.1f s)\n",
                ph, basename(cand[1]), ld$built_depth, fmt(ld$n_writes),
                100 * ld$n_writes / ld$size, el))
  } else {
    cat(sprintf("  phase %d: %s refused (%s) -- fills lazily instead\n",
                ph, basename(cand[1]), ld$reason))
  }
}

# A table one level deep bounds nothing: a miss returns built_depth + 1, so at
# depth 1 every unknown state is called "2 moves away" and nothing is pruned.
# Worth saying out loud, because the line above reads like a success.
cat(sprintf("\n  loaded %d of 3 phases\n", sum(loaded)))

# What the three tables hold before any cube is solved, so the same reading
# after the solves says whether they survived it. A loaded table that comes
# back shallower, or with n_grows above zero, was reallocated -- and grow_to()
# discards the contents when it reallocates, which is the whole reason the
# files exist.
tables_before <- cayleyR:::cube_kociemba4_tables_cpp()
cat(sprintf("\n  %5s %6s %14s %14s %8s %8s\n",
            "phase", "depth", "filled", "slots", "full", "grows"))
for (i in 1:3) {
  t <- tables_before[[i]]
  cat(sprintf("  %5d %6d %14s %14s %7.1f%% %8d\n", i, t$built_depth,
              fmt(t$filled), fmt(t$size), 100 * t$filled / t$size, t$n_grows))
}

# ---- Every cube, on that table ---------------------------------------------
hr(sprintf("%d cubes on the prefilled table", n_states))

cat(sprintf("  %5s %9s %14s %8s %10s\n",
            "cube", "result", "method", "moves", "secs"))
rows <- list()
all_stages <- list()
all_ops <- list()
for (i in seq_len(n_states)) {
  # Announced before it runs. The line below reports the cube after the fact,
  # which says nothing at all while the cube is the one being worked on.
  if (verbose) {
    cat(sprintf("  %5d  solving (%d of %d)...\n", i, i, n_states))
    flush.console()
  }
  r <- solve_one(states[[i]])
  cat(sprintf("  %5d %9s %14s %8s %10.1f\n", i,
              if (r$ok) "solved" else "FAILED", r$method,
              if (is.na(r$moves)) "-" else r$moves, r$secs))

  # Where those seconds went. Printed per cube rather than collected for the
  # end, because a run that is stopped early should still have said something.
  if (!is.null(r$stages)) {
    st <- r$stages
    cat(sprintf("        orientation %s%s\n", r$orientation,
                if (!is.na(r$candidate) && !identical(r$candidate, r$orientation))
                  sprintf("   candidate %s", r$candidate) else ""))
    cat(sprintf("        %-13s %8s %7s %13s  %s\n",
                "stage", "secs", "moves", "nodes", "outcome"))
    for (k in seq_len(nrow(st))) {
      cat(sprintf("        %-13s %8.1f %7s %13s  %s\n",
                  st$stage[k], st$secs[k],
                  if (is.na(st$moves[k])) "-" else st$moves[k],
                  if (is.na(st$nodes[k])) "-" else fmt(st$nodes[k]),
                  st$outcome[k]))
    }

    # And the same seconds unaggregated, one line per operation in the order it
    # ran. The sums above say which operation is expensive; this says whether it
    # was expensive once or cheap many times, and on which orientation -- which
    # is the difference between a table to deepen and a ladder to reorder.
    if (!is.null(r$ops) && nrow(r$ops) > 0) {
      op <- r$ops[r$ops$secs >= 0.05, , drop = FALSE]
      if (nrow(op) > 0) {
        cat(sprintf("\n        %-13s %8s %-22s %13s  %s\n",
                    "operation", "secs", "detail", "nodes", "outcome"))
        for (k in seq_len(nrow(op))) {
          cat(sprintf("        %-13s %8.1f %-22s %13s  %s\n",
                      op$op[k], op$secs[k],
                      if (is.na(op$detail[k])) "" else op$detail[k],
                      if (is.na(op$nodes[k])) "-" else fmt(op$nodes[k]),
                      if (is.na(op$outcome[k])) "" else op$outcome[k]))
        }
        cat("\n")
      }
    }
  }
  flush.console()
  rows[[i]] <- data.frame(cube = i, ok = r$ok, method = r$method,
                          moves = r$moves, secs = r$secs,
                          candidate = if (is.null(r$candidate)) NA_character_
                                      else r$candidate,
                          stringsAsFactors = FALSE)
  all_stages[[i]] <- r$stages
  if (!is.null(r$ops)) {
    all_ops[[length(all_ops) + 1L]] <- cbind(cube = i, r$ops)
  }
}
tab <- do.call(rbind, rows)

# ---- What it means for a batch ---------------------------------------------
hr("what this says")

ok <- tab[tab$ok, ]
cat(sprintf("  solved              : %d of %d\n", nrow(ok), nrow(tab)))

# Which road each cube took. "reduce" means phase 3 finished inside the budget
# and the prefilled table did its job; "solve4" means it did not and the
# fallback carried the cube instead. A high solved count on a low reduce count
# is the cascade working around phase 3, not phase 3 working.
for (m in names(sort(table(tab$method), decreasing = TRUE))) {
  cat(sprintf("  method %-13s: %d\n", m, sum(tab$method == m)))
}

# The tables again, against the reading taken before the first cube: one that
# lost depth or grew was rebuilt mid-search, discarding what the file paid for.
hr("the tables after the solves")

tables_after <- cayleyR:::cube_kociemba4_tables_cpp()
cat(sprintf("  %5s %13s %13s %10s %10s\n",
            "phase", "depth", "filled", "grows", "verdict"))
for (i in 1:3) {
  b <- tables_before[[i]]
  a <- tables_after[[i]]
  verdict <- if (a$n_grows > b$n_grows) "REGROWN"
             else if (a$built_depth < b$built_depth) "SHALLOWER"
             else "intact"
  cat(sprintf("  %5d %6d->%-6d %13s %10d %10s\n", i,
              b$built_depth, a$built_depth, fmt(a$filled),
              a$n_grows, verdict))
}

# Every operation of every cube, unaggregated -- so `calls` below counts what
# actually ran rather than how many cubes had a row for it. A stage summed per
# cube and then counted per row reports "2 calls" for a phase that ran sixteen
# times, which is the number that mattered.
op_all <- do.call(rbind, all_ops)
st_all <- do.call(rbind, all_stages)
if (!is.null(op_all) && nrow(op_all) > 0) {
  cat("\n  where the seconds went, all cubes:\n")
  by_stage <- tapply(op_all$secs, op_all$op, sum)
  n_calls <- tapply(op_all$secs, op_all$op, length)
  total <- sum(op_all$secs)
  for (nm in names(sort(by_stage, decreasing = TRUE))) {
    cat(sprintf("    %-13s %8.1f s  (%4.1f%%, %d calls, %5.1f s each)\n",
                nm, by_stage[[nm]], 100 * by_stage[[nm]] / total,
                n_calls[[nm]], by_stage[[nm]] / n_calls[[nm]]))
  }
  cat(sprintf("    %-13s %8.1f s\n", "total", total))

  mv <- st_all[!is.na(st_all$moves), ]
  if (nrow(mv) > 0) {
    cat("\n  moves contributed, median per successful stage:\n")
    by_mv <- tapply(mv$moves, mv$stage, median)
    for (nm in names(by_mv)) {
      cat(sprintf("    %-13s %6.0f\n", nm, by_mv[[nm]]))
    }
  }
}

if (nrow(ok) > 0) {
  cat(sprintf("  seconds per cube    : median %.1f, mean %.1f, worst %.1f\n",
              median(ok$secs), mean(ok$secs), max(ok$secs)))
  cat(sprintf("  moves               : median %.0f, worst %.0f\n",
              median(ok$moves), max(ok$moves)))
  cat(sprintf("  under 10 s          : %d of %d\n",
              sum(ok$secs <= 10), nrow(ok)))

  # Which candidate carried each cube, and how far down the sweep it sat. Every
  # candidate before the winner was paid in full, so this is both where a slow
  # cube's seconds went and what running them in parallel is worth.
  cw <- ok$candidate[!is.na(ok$candidate)]
  if (length(cw)) {
    cat("\n  winning candidate   :\n")
    for (nm in names(sort(table(cw), decreasing = TRUE))) {
      cat(sprintf("    %-8s %d\n", nm, sum(cw == nm)))
    }
    # How far down the sweep the winner sat is only recoverable from the label
    # when the sweep ran in build order -- orientations outer, solutions inner.
    # Shuffled, the label says which candidate won and nothing about when it was
    # reached, and computing a position from it would report a confident number
    # for a quantity that was not measured. So it is reported only when it means
    # something, and the cap is stated either way since it bounds the wait.
    if (!shuffle) {
      rot_of <- sub("#.*$", "", cw)
      sol_of <- suppressWarnings(as.integer(sub("^.*#", "", cw)))
      sol_of[is.na(sol_of)] <- 1L
      rots <- unique(sub("#.*$", "", ok$candidate[!is.na(ok$candidate)]))
      ord <- match(rot_of, c("(-)", "1y", "1x", "1z"))
      ord[is.na(ord)] <- match(rot_of[is.na(ord)], rots)
      pos <- (ord - 1L) * max(sol_of) + sol_of
      cat(sprintf("  candidates tried    : median %.0f, worst %d of %d\n",
                  median(pos), max(pos), length(rots) * max(sol_of)))
    } else {
      cat(sprintf("  candidates tried    : up to %s per cube, shuffled -- the\n",
                  if (is.na(max_candidates)) "16" else as.character(max_candidates)))
      cat("                        label says which candidate won, not when\n")
    }
  }
}

# What the batch would cost, which is the question the plan turns on: a fill
# paid once against a per-cube time collected a thousand times.
if (nrow(ok) > 0) {
  cat(sprintf("\n  fill cost           : %.1f s, paid once\n", fill_secs))
  cat(sprintf("  1000 cubes would be : %.0f min  (%.1f s fill + 1000 x %.1f s)\n",
              (fill_secs + 1000 * median(ok$secs)) / 60, fill_secs,
              median(ok$secs)))
  cat(sprintf("  against a 10 s target: %s\n",
              if (median(ok$secs) <= 10) "inside it"
              else sprintf("%.1fx over", median(ok$secs) / 10)))
}

if (any(!tab$ok)) {
  cat(sprintf("\n  %d cube(s) did not solve. A prefilled table makes the search\n",
              sum(!tab$ok)))
  cat("  cheaper per node, not able to reach further: the node budget and the\n")
  cat("  depth ceiling are unchanged, so a cube out of reach before is out of\n")
  cat("  reach now.\n")
}

# ---- Every cube, one line each ----------------------------------------------
#
# The medians above say what a batch costs; this says which cubes it was made
# of. They are not the same reading, and on this solver the difference is the
# whole story: a median of six seconds and a worst of six minutes describe the
# same run, and only the per-cube list shows that the second number is not a
# stray. Sorted by seconds so the expensive tail is together at the bottom
# rather than scattered through the batch.
hr("every cube, slowest last")
cat(sprintf("  %5s %8s %7s %9s  %s\n",
            "cube", "secs", "moves", "candidate", "method"))
for (k in order(tab$secs, na.last = TRUE)) {
  cat(sprintf("  %5d %8s %7s %9s  %s\n", tab$cube[k],
              if (is.na(tab$secs[k])) "-" else sprintf("%.1f", tab$secs[k]),
              if (is.na(tab$moves[k])) "-" else format(tab$moves[k]),
              if (is.na(tab$candidate[k])) "-" else tab$candidate[k],
              if (tab$ok[k]) tab$method[k] else paste0(tab$method[k], " FAILED")))
}

cat("\n  Every cube here ran on the same table, filled once before the first of\n")
cat("  them -- which is the batch scenario itself rather than a proxy for it.\n")
cat("  Times still vary between cubes: the table makes each node cheaper to\n")
cat("  judge, it does not make two cubes equally deep.\n")
