#!/usr/bin/env Rscript
# Why the greedy descent fails, one failure at a time.
#
# The solver reports solved/not, and a run that did not solve can have ended two
# entirely different ways:
#
#   budget   it was still walking when the step ceiling ran out. The fix is a
#            larger budget, and nothing about the network or the descent is
#            wrong.
#   stuck    every child of the current state had already been visited, so there
#            was nowhere left to step. That is a genuine dead end and a bigger
#            budget cannot help.
#
# cube_adi_solve() does not distinguish them in its return value -- both come
# back as solved = FALSE -- but the path length does: a run that used its whole
# budget has exactly `budget` moves in it, a dead end has fewer.
#
# The two have different fixes and different costs, so they are counted apart
# here rather than together. A third possibility is measured alongside: whether
# failures cluster at particular scramble depths, which would point at the value
# being underestimated far from solved rather than at the mechanics of descent.
#
# What the descent actually does at a dead end is worth stating, because it is
# not backtracking. On hitting a state whose best child was already seen, it
# takes the next best unvisited one and carries on forward -- it never returns
# to an earlier state to try a different branch. So "stuck" means every one of
# the 18 children has been visited before, which after a short walk is a strong
# statement.
#
# Run with:  Rscript inst/examples/diag_adi_failures.R [name=value ...]
#
#   net=/mnt/Data2/DS_projects/333  model directory, or the folder holding them
#   states=100 depth=10 budget=50 backend=auto seed=2026

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

defaults <- list(
  net     = "/mnt/Data2/DS_projects/333",
  states  = 100L,
  depth   = 10L,
  budget  = 50L,
  batch   = 128L,
  backend = "auto",
  seed    = 2026L
)

opt <- defaults
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1L]]
  if (length(kv) != 2L) stop("argument must be name=value: ", a)
  key <- kv[[1L]]
  if (is.null(defaults[[key]])) {
    stop("unknown parameter: ", key, "\navailable: ",
         paste(names(defaults), collapse = ", "))
  }
  opt[[key]] <- if (is.character(defaults[[key]])) kv[[2L]]
                else if (is.integer(defaults[[key]])) as.integer(kv[[2L]])
                else as.numeric(kv[[2L]])
  if (!is.character(opt[[key]]) && is.na(opt[[key]]))
    stop("not a number: ", a)
}

set.seed(opt$seed)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------

net_dir <- path.expand(opt$net)
if (!file.exists(file.path(net_dir, "meta.rds"))) {
  found <- list.files(net_dir, full.names = TRUE)
  found <- found[file.exists(file.path(found, "meta.rds"))]
  if (length(found) == 0L) stop("no saved network in ", net_dir)
  net_dir <- found[[which.max(file.mtime(file.path(found, "meta.rds")))]]
}

net <- cube_adi_load(net_dir, backend = opt$backend)
g   <- net$group
cat("model    :", basename(net_dir), "\n")
cat("states   :", opt$states, "at depth", opt$depth,
    "with budget", opt$budget, "\n")

# ---------------------------------------------------------------------------
# Can it rank a cube's own children
# ---------------------------------------------------------------------------

# What a descent consumes is not accuracy but ordering: of the 18 children of a
# scrambled cube, the ones nearer the goal must score below the rest. An
# estimator can be accurate on average and still rank a cube's own children
# wrongly, and that -- not its error -- is what decides whether a greedy walk
# arrives.
#
# Measured exactly as predict_cube3_depth.R measures it, so the two estimators
# can be compared on the same number. Nearness is judged by how many positions
# already sit at home, a measure neither network has any part in.
hr("can it rank a cube's own children")

nm    <- length(g$moves)
sc_r  <- cayleyR:::cube_adi_scramble(g$ptr, opt$states, opt$depth)
deep  <- which(as.integer(sc_r$depth) >= 2L)   # depth 1: every child is trivial
sts_r <- sc_r$states[deep, , drop = FALSE]

ch_r  <- cayleyR:::cube_adi_children(g$ptr, sts_r)
v_all <- cayleyR:::adi_value_of(net$value, ch_r$children, opt$batch,
                                net$arch, net$layout)

ranks <- integer(0)
for (i in seq_len(nrow(sts_r))) {
  st  <- as.integer(sts_r[i, ])
  ix  <- ((i - 1L) * nm + 1L):(i * nm)        # this cube's block, state-major
  kid <- ch_r$children[ix, , drop = FALSE]

  home_parent <- sum(st == seq_along(st))
  home_child  <- apply(kid, 1L, function(r) sum(r == seq_along(r)))
  better      <- which(home_child > home_parent)
  if (length(better) == 0L) next

  ranks <- c(ranks, min(match(better, order(v_all[ix]))))
}

if (length(ranks) == 0L) {
  cat("  no usable cubes\n")
} else {
  cat(sprintf("  %d cubes with a child that puts more pieces home\n",
              length(ranks)))
  cat(sprintf("  best such child ranked 1st  : %.0f%%\n",
              100 * mean(ranks == 1L)))
  cat(sprintf("  in the top 3                : %.0f%%\n",
              100 * mean(ranks <= 3L)))
  cat(sprintf("  in the top 5                : %.0f%%\n",
              100 * mean(ranks <= 5L)))
  cat(sprintf("  mean rank                   : %.1f of %d\n",
              mean(ranks), nm))
  cat(sprintf("  chance would give           : %.1f\n", (nm + 1) / 2))

  # A greedy descent needs the first place on every step of the way, so the
  # first-place rate raised to the path length is roughly its chance of
  # arriving. That number falls away fast, which is why an estimator can be
  # much the better one and still solve fewer cubes.
  p1 <- mean(ranks == 1L)
  cat(sprintf("\n  a %d-step descent needs %d firsts in a row: %.1f%% at this rate\n",
              opt$depth, opt$depth, 100 * p1 ^ opt$depth))
}

# ---------------------------------------------------------------------------
# Run, and record how each attempt ended
# ---------------------------------------------------------------------------

# The scramble depth is recorded per state, not just its ceiling: cube_adi_scramble
# draws a length uniformly in 1..depth, so a batch at depth=10 holds everything
# from one move to ten, and the failures can be laid out against it.
rows <- list()

for (i in seq_len(opt$states)) {
  sc    <- cayleyR:::cube_adi_scramble(g$ptr, 1L, opt$depth)
  state <- as.integer(sc$states[1L, ])
  d     <- as.integer(sc$depth[[1L]])

  r <- cube_adi_solve(net, state, budget = opt$budget,
                      batch_size = opt$batch)

  n   <- length(r$path)
  how <- if (isTRUE(r$solved)) "solved"
         else if (n >= opt$budget) "budget"      # ceiling reached, still walking
         else "stuck"                            # every child already visited

  rows[[length(rows) + 1L]] <- data.frame(
    state = i, scramble_depth = d, outcome = how, moves = n,
    stringsAsFactors = FALSE)

  if (i %% 20L == 0L) cat(sprintf("  %d/%d\n", i, opt$states))
}

res <- do.call(rbind, rows)

# ---------------------------------------------------------------------------
# 1. Which of the two ends the failures hit
# ---------------------------------------------------------------------------

hr("how each attempt ended")

tab <- table(factor(res$outcome, levels = c("solved", "budget", "stuck")))
for (nm in names(tab)) {
  cat(sprintf("  %-8s %3d  (%.0f%%)\n", nm, tab[[nm]],
              100 * tab[[nm]] / nrow(res)))
}

fail <- res[res$outcome != "solved", ]

if (nrow(fail) == 0L) {
  cat("\nnothing failed -- nothing to diagnose\n")
} else {
  cat("\nfailures one by one:\n")
  cat("  state  scramble  outcome  moves walked\n")
  for (k in seq_len(nrow(fail))) {
    cat(sprintf("  %5d  %8d  %-7s  %d\n", fail$state[[k]],
                fail$scramble_depth[[k]], fail$outcome[[k]],
                fail$moves[[k]]))
  }

  # ------------------------------------------------------------------------
  # 2. Do failures cluster at particular scramble depths
  # ------------------------------------------------------------------------

  # If they do, the value is being underestimated far from solved and more
  # training would help. If they are spread evenly, the descent itself is what
  # fails, and training longer would leave the rate where it is.
  hr("solved rate by scramble depth")

  # The move count of the solves matters as much as their number. A state eight
  # moves from solved that comes back in fifteen was found by a gradient that
  # points somewhere, however weakly. One that comes back just under the ceiling
  # was found by a walk that happened to blunder into the goal, and says the
  # value barely separates deep states at all.
  cat("  depth   n  solved  budget   stuck   moves when solved\n")
  for (d in sort(unique(res$scramble_depth))) {
    dd <- res[res$scramble_depth == d, ]
    mv <- dd$moves[dd$outcome == "solved"]
    cat(sprintf("  %5d %3d  %5.0f%% %6d %7d   %s\n", d, nrow(dd),
                100 * mean(dd$outcome == "solved"),
                sum(dd$outcome == "budget"), sum(dd$outcome == "stuck"),
                if (length(mv) == 0L) "-" else
                  sprintf("%.0f (max %d, ceiling %d)", mean(mv), max(mv),
                          opt$budget)))
  }

  # Excess over the scramble length is the honest measure of the descent: a
  # scramble of 8 solved in 8 moves is a perfect line to the goal, the same one
  # solved in 40 is a wander that ended well.
  ok <- res[res$outcome == "solved", ]
  if (nrow(ok) > 0L) {
    deep <- ok[ok$scramble_depth >= 7L, ]
    cat(sprintf("\nmoves over scramble length: all solved %+.1f",
                mean(ok$moves - ok$scramble_depth)))
    if (nrow(deep) > 0L) {
      cat(sprintf(", at depth 7+ %+.1f (%d of them)",
                  mean(deep$moves - deep$scramble_depth), nrow(deep)))
    }
    cat("\n")
  }

  ok_d   <- res$scramble_depth[res$outcome == "solved"]
  bad_d  <- res$scramble_depth[res$outcome != "solved"]
  cat(sprintf("\nmean scramble depth: solved %.1f, failed %.1f\n",
              mean(ok_d), mean(bad_d)))

  # ------------------------------------------------------------------------
  # 3. What to do about it
  # ------------------------------------------------------------------------

  hr("what this points at")

  n_budget <- sum(fail$outcome == "budget")
  n_stuck  <- sum(fail$outcome == "stuck")

  # "Hit the ceiling" is not by itself a case for a bigger budget. A state eight
  # moves from solved that walked fifty and did not arrive was not running out
  # of room -- it was wandering. The ratio of budget to how far the state
  # actually was tells the two apart, and only a run that spent its budget
  # without much slack is genuinely short of it.
  if (n_budget > 0L) {
    room <- opt$budget / mean(fail$scramble_depth[fail$outcome == "budget"])
    cat(sprintf("%d run(s) spent the whole budget without arriving.\n", n_budget))
    if (room >= 3) {
      cat(sprintf("  Their scrambles averaged %.1f moves and the budget was %d,\n",
                  mean(fail$scramble_depth[fail$outcome == "budget"]),
                  opt$budget))
      cat(sprintf("  a factor of %.0f. That is wandering, not running out of\n",
                  room))
      cat("  room -> a larger budget buys a longer wander, not a solve\n")
    } else {
      cat("  -> the budget is genuinely tight for these depths; raise it\n")
    }
  }
  if (n_stuck > 0L) {
    cat(sprintf("%d run(s) ran out of unvisited children.\n", n_stuck))
    cat("  -> a real dead end; a larger budget cannot help. The descent walks\n")
    cat("     forward through the next-best child and never returns to an\n")
    cat("     earlier state, so it needs backtracking, not more training\n")
    cat(sprintf("     (dead ends came after %s moves)\n",
                paste(sort(fail$moves[fail$outcome == "stuck"]),
                      collapse = ", ")))
  }
  if (length(bad_d) > 0L && length(ok_d) > 0L &&
      mean(bad_d) > mean(ok_d) + 1.5) {
    cat("Failures sit at clearly deeper scrambles than successes.\n")
    cat("  -> the value is underestimated far from solved; more training\n")
    cat("     at this depth would move the rate\n")
  } else if (length(bad_d) > 0L) {
    cat("Failures are spread across scramble depths, not concentrated deep.\n")
    cat("  -> training longer would sharpen the numbers and leave the rate\n")
    cat("     roughly where it is; the mechanics of the descent are the limit\n")
  }
}

cat("\nDone.\n")
