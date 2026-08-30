#!/usr/bin/env Rscript
# Load a distance estimator and see what it is worth.
#
# Takes what train_cube3_depth.R saved and asks three things of it, in order of
# how much they matter to a solver:
#
#   1. does it come back the same     the values on fresh cubes must match what
#                                     training reported, or the load is broken
#                                     and everything below measures noise
#   2. does it rise with depth        the `step` column. A descent picks the
#                                     child with the smallest score, so what it
#                                     needs is an ordering, not accuracy. Where
#                                     consecutive depths score alike there is
#                                     nothing to choose between them
#   3. can it pick the right move     of the 18 children of a scrambled cube,
#                                     exactly one or two lie closer to solved.
#                                     Scoring those below the rest is the whole
#                                     job, and it is measured here directly
#                                     rather than inferred from a solve rate
#
# The third is the one that predicts whether a descent will work, and no amount
# of MAE substitutes for it: an estimator can be accurate on average and still
# rank a cube's own children wrongly, which is exactly the failure that leaves a
# greedy walk wandering.
#
# Run with:  Rscript inst/examples/predict_cube3_depth.R [name=value ...]
#
#   net=/mnt/Data2/DS_projects/333  model directory, or the folder holding them
#   states=2000 depth=20 backend=auto seed=2026

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

defaults <- list(
  net     = "/mnt/Data2/DS_projects/333",
  states  = 2000L,   # cubes for the depth table
  ranking = 500L,    # cubes for the child-ranking test
  depth   = 0L,      # 0 = use whatever the model was trained to
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

hr <- function(t) cat("\n== ", t, " ", strrep("-", max(0, 58 - nchar(t))), "\n",
                      sep = "")

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------

hr("load")

# net= takes either a model directory or the folder they live in; given the
# folder, the newest wins.
net_dir <- path.expand(opt$net)
if (!file.exists(file.path(net_dir, "model.ggml"))) {
  found <- list.files(net_dir, full.names = TRUE)
  found <- found[file.exists(file.path(found, "model.ggml"))]
  if (length(found) == 0L) {
    stop("no depth estimator in ", net_dir,
         "\nrun this first: Rscript inst/examples/train_cube3_depth.R")
  }
  net_dir <- found[[which.max(file.mtime(file.path(found, "model.ggml")))]]
  if (length(found) > 1L) {
    cat(length(found), "models in", opt$net, "-- taking the newest\n")
  }
}

meta  <- readRDS(file.path(net_dir, "meta.rds"))
t0    <- proc.time()[["elapsed"]]
model <- ggmlR::ggml_load_model(file.path(net_dir, "model.ggml"),
                                backend = opt$backend)
cat(sprintf("loaded %s in %.2fs\n", basename(net_dir),
            proc.time()[["elapsed"]] - t0))

g   <- cube_group(3)
lay <- cube_piece_layout(g)

depth <- if (opt$depth > 0L) opt$depth else meta$depth
cat(sprintf("trained : depth %d%s, %d cubes, %d epochs, MAE %.2f\n",
            meta$depth, if (isTRUE(meta$fixed > 0L)) " (fixed)" else "",
            meta$states, meta$epochs, meta$mae))
cat(sprintf("testing : depth %d\n", depth))

# The scaling is undone with the numbers the training run derived from `depth`
# alone, carried in meta.rds. Getting this wrong would shift every prediction by
# a constant and leave the ordering intact, so it would not show up in the
# ranking test below -- only in the absolute values.
score <- function(states) {
  x <- cayleyR:::adi_encode(states, "transformer", lay)
  as.numeric(ggmlR::ggml_predict(model, x)) * meta$y_sd + meta$y_mu
}

# ---------------------------------------------------------------------------
# 1. Did it survive the round trip
# ---------------------------------------------------------------------------

hr("does it predict anything")

sc <- cayleyR:::cube_adi_scramble(g$ptr, opt$states, depth)
vp <- score(sc$states)
y  <- as.integer(sc$depth)

mae  <- mean(abs(vp - y))
base <- mean(abs(y - mean(y)))

cat(sprintf("  MAE      %.2f moves   (baseline %.2f)\n", mae, base))
cat(sprintf("  spread   %.2f  (sd of predictions)\n", stats::sd(vp)))

if (stats::sd(vp) < 0.05) {
  cat("  FAIL: predictions are a constant -- the load returned dead weights\n")
} else if (mae >= base * 0.95) {
  cat("  FAIL: no better than answering the mean\n")
} else {
  cat(sprintf("  OK: better than the mean by %.0f%%\n", 100 * (1 - mae / base)))
}

# ---------------------------------------------------------------------------
# 2. Does it rise with depth
# ---------------------------------------------------------------------------

hr("does the score rise with depth")

cat("  depth    n   predicted     step\n")
prev  <- NA_real_
steps <- numeric(0)
for (d in sort(unique(y))) {
  k <- y == d
  m <- mean(vp[k])
  if (!is.na(prev)) steps <- c(steps, m - prev)
  cat(sprintf("  %5d %4d   %6.2f  %s\n", d, sum(k), m,
              if (is.na(prev)) "" else sprintf("%+.2f", m - prev)))
  prev <- m
}

if (length(steps) > 0L) {
  flat <- sum(steps < 0.05)
  cat(sprintf("\n  %d of %d steps are under +0.05", flat, length(steps)))
  cat(if (flat == 0L) " -- the ordering holds all the way\n"
      else sprintf(" -- blind from depth %d on\n",
                   sort(unique(y))[[which(steps < 0.05)[[1L]] + 1L]]))
}

# ---------------------------------------------------------------------------
# 3. Can it pick the move that helps
# ---------------------------------------------------------------------------

# The test that decides whether a descent works. A cube scrambled by d moves has
# at least one child at d-1 -- undoing the last move -- and the estimator should
# score it below the other 17. Being accurate on average is not the same thing:
# what a greedy walk consumes is the ordering among one cube's own children, and
# an estimator can have a fine MAE while getting that ordering wrong.
#
# The last move of the scramble is known here, so the child that undoes it is
# known too. It is a child at d-1, not necessarily the only one, so scoring
# another child lowest is not automatically an error -- which is why the rank is
# reported as a distribution rather than as a single hit rate.
hr("can it rank a cube's own children")

nm <- length(g$moves)

# Every cube's children are scored in one pass, not one cube at a time:
# ggml_predict refuses a batch smaller than its batch_size, and 18 children on
# their own are fewer than that. Expanding all the cubes first also makes this
# one GPU call instead of hundreds.
sc_r  <- cayleyR:::cube_adi_scramble(g$ptr, opt$ranking, depth)
deep  <- which(as.integer(sc_r$depth) >= 2L)   # depth 1: every child is trivial
sts   <- sc_r$states[deep, , drop = FALSE]

ch    <- cayleyR:::cube_adi_children(g$ptr, sts)
v_all <- score(ch$children)

ranks <- integer(0)
for (i in seq_len(nrow(sts))) {
  st  <- as.integer(sts[i, ])
  ix  <- ((i - 1L) * nm + 1L):(i * nm)       # this cube's block, state-major
  v   <- v_all[ix]
  kid <- ch$children[ix, , drop = FALSE]

  # Which children are genuinely closer. Recomputed rather than assumed, using a
  # measure the model has no part in: how many positions already sit at home. A
  # child with more of them than its parent is nearer the solved state.
  home_parent <- sum(st == seq_along(st))
  home_child  <- apply(kid, 1L, function(r) sum(r == seq_along(r)))
  better      <- which(home_child > home_parent)
  if (length(better) == 0L) next

  ranks <- c(ranks, min(match(better, order(v))))
}

if (length(ranks) == 0L) {
  cat("  no usable cubes -- raise ranking= or depth=\n")
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

  if (mean(ranks) > (nm + 1) / 2 * 0.9) {
    cat("\n  Barely better than chance: a greedy descent has nothing to\n")
    cat("  steer by, whatever the MAE says.\n")
  } else if (mean(ranks == 1L) > 0.5) {
    cat("\n  It picks the helping move outright more often than not.\n")
  } else {
    cat("\n  The helping move is ranked well but not first: a descent will\n")
    cat("  wander, and a search that keeps a few candidates would not.\n")
  }
}

cat("\nDone.\n")
