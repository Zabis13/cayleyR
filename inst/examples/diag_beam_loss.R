#!/usr/bin/env Rscript
# Where the beam loses the path.
#
# test_cube3_transformer.R says what depth_beam scores: 78 of 100, against 86
# for the same search over the ADI value head. It cannot say why the other 22
# were lost, and without that any change to the search is a guess. This script
# measures the four things that decide whether a beam arrives, on scrambles
# whose answer is known.
#
# The subject is the depth estimator, the one being improved. The ADI network is
# loaded only if net= is given, and then only as a yardstick.
#
# The known answer is the point. A random scramble is built here rather than
# taken from cube_adi_scramble, because the inverse of the scramble is an
# optimal-enough solution: at every state along it there is a move that provably
# leads home, and the questions below are all "did the estimator like that move,
# and did the beam keep it".
#
#   1. rank of the helping move
#        At each state on the reference path, score all 18 children and find
#        where the child that follows the reference sits in that order. Rank 1
#        means a greedy descent would have taken it. Rank <= k means a beam of
#        width k keeps it, if nothing else crowds it out. This is the ceiling on
#        what any beam over this estimator can do.
#
#   2. estimator error against true remaining distance
#        On the reference path the distance home is known -- it is the number of
#        moves left. Comparing the estimate against it says whether the model is
#        wrong everywhere or wrong only close in. An estimator that flattens out
#        near the solved state gives the beam nothing to steer by exactly where
#        precision matters most.
#
#   3. the step at which the beam drops the reference state
#        Run the beam and watch for the reference path's states in the frontier.
#        The step where the last one falls out is the step the solve was lost
#        at. Early losses mean the beam is too narrow; late ones mean it wandered
#        after being on track.
#
#   4. rescoring of states already seen
#        The current beam marks only the survivors as visited, so a state thrown
#        away at one step can come back and be scored again at the next. Counting
#        those says how much of the batch is wasted work.
#
# Run with:  Rscript inst/examples/diag_beam_loss.R [name=value ...]
#
#   dnet=<dir>    the depth estimator, or the folder holding models
#   net=<dir>     the ADI model, measured alongside as a yardstick; "" to skip
#   states=30     how many scrambles
#   depth=10      scramble length -- exactly this many, unlike the C++ scrambler
#   beam=20       beam width to measure
#   bsteps=30     how far the beam may walk
#   backend=auto seed=2026

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

defaults <- list(
  dnet    = "/mnt/Data2/DS_projects/333",
  net     = "",      # the ADI model, measured alongside only when given
  states  = 30L,
  depth   = 10L,
  beam    = 20L,
  bsteps  = 30L,
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

cat("parameters:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

# ---------------------------------------------------------------------------
# 1. Load the estimators
# ---------------------------------------------------------------------------

hr("load")

newest_under <- function(dir, marker) {
  dir <- path.expand(dir)
  if (file.exists(file.path(dir, marker))) return(dir)
  found <- list.files(dir, full.names = TRUE)
  found <- found[file.exists(file.path(found, marker))]
  if (length(found) == 0L) stop("no model with ", marker, " in ", dir)
  found[[which.max(file.mtime(file.path(found, marker)))]]
}

# The group is built here rather than taken from a model, so the depth
# estimator can be measured on its own -- it is the one being improved, and
# loading the ADI network to get a group would tie the two together for no
# reason. train_cube3_depth.R builds it the same way.
g     <- cube_group(3)
id    <- group_identity(g)
moves <- g$moves
dlay  <- cube_piece_layout(g)
cat("group :", g$name, "--", length(moves), "moves\n")

# Estimators are wrapped to one shape -- a matrix of states in, one number per
# row out -- so everything below runs over either without knowing which it has.
scorers <- list()

dn     <- newest_under(opt$dnet, "model.ggml")
dmeta  <- readRDS(file.path(dn, "meta.rds"))
dmodel <- ggmlR::ggml_load_model(file.path(dn, "model.ggml"),
                                 backend = opt$backend)
cat("depth :", basename(dn),
    sprintf("(trained to %d, MAE %.2f)\n", dmeta$depth, dmeta$mae))

# ggml_predict refuses a batch under the model's own, and 18 children is under
# it. Pad by repeating row one and drop the extras; the net is stateless
# between rows, so the repeats change nothing.
DPAD <- 64L
scorers$depth <- function(states) {
  n <- nrow(states)
  s <- if (n >= DPAD) states else
    states[c(seq_len(n), rep(1L, DPAD - n)), , drop = FALSE]
  x <- cayleyR:::adi_encode(s, "transformer", dlay)
  v <- as.numeric(ggmlR::ggml_predict(dmodel, x)) * dmeta$y_sd + dmeta$y_mu
  v[seq_len(n)]
}

# The ADI network is not the subject here, only a yardstick, so it is loaded
# only when asked for.
if (nzchar(opt$net)) {
  net_dir <- newest_under(opt$net, "meta.rds")
  net     <- cube_adi_load(net_dir, backend = opt$backend)
  cat("adi   :", basename(net_dir), "\n")
  stopifnot(length(net$group$moves) == length(moves))
  scorers$adi <- function(states)
    cayleyR:::adi_value_of(net$value, states, opt$batch, net$arch, net$layout)
}

cat("scoring:", paste(names(scorers), collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 2. Scrambles with a known way home
# ---------------------------------------------------------------------------

# generate_walk rather than cube_adi_scramble, for two reasons: that one picks a
# random depth up to max_depth instead of using it, and it does not hand back
# the moves it made. Both matter here -- the whole method rests on knowing the
# path back. exact = TRUE because a mixture of depths would leave every number
# below an average over distances rather than a statement about one.
#
# The reference path is an upper bound on the true distance: a shorter route may
# exist that the walk did not take. That makes the figures conservative, which is
# the safe direction -- the estimator is never blamed for preferring a genuinely
# better move.
walks <- generate_walk(g, n = opt$states, n_moves = opt$depth, exact = TRUE)

# The trail is the reference path walked out: the states passed through on the
# way home, so that step i can be asked which child the reference took.
cases <- lapply(seq_len(opt$states), function(i) {
  sol   <- walks$solution[[i]]
  trail <- vector("list", length(sol) + 1L)
  trail[[1L]] <- as.integer(walks$states[i, ])
  for (j in seq_along(sol)) {
    trail[[j + 1L]] <- group_apply(g, trail[[j]], sol[[j]])
  }
  stopifnot(identical(trail[[length(trail)]], id))
  list(state = trail[[1L]], solution = sol, trail = trail)
})

# One key per row, via the package's own hash rather than paste(). The beam
# hashes its whole frontier's children at every step, and at 18 children a row
# that is where an R-level paste() goes when the run gets long. cube_adi_keys
# returns 53-bit FNV hashes: collisions are possible in principle and would show
# here as a state wrongly called visited, but at this scale the chance is far
# below anything these figures are read to.
keys_of <- function(m) as.character(cayleyR:::cube_adi_keys(m))
key_of  <- function(v) keys_of(matrix(as.integer(v), nrow = 1L))[[1L]]

# ---------------------------------------------------------------------------
# 3. Rank of the helping move, and error against true distance
# ---------------------------------------------------------------------------

hr("rank of the helping move")

# Walked over the reference path: at each state, score the 18 children and ask
# where the one the reference takes ended up. Position 1 is what greedy needs;
# the whole distribution is what a beam needs.
#
# Every decision on every reference path is gathered first and scored in one
# pass. Done a state at a time this is two forward passes of 19 useful rows each
# -- and the depth model refuses a batch under 64, so each was padded to 64 and
# threw most of it away. The network is stateless between rows, so batching
# changes nothing but the time.
rank_of <- function(score_fn) {
  # One row per decision: the state itself, then its 18 children.
  states <- do.call(rbind, lapply(cases, function(cs)
    do.call(rbind, lapply(seq_along(cs$solution), function(i)
      as.integer(cs$trail[[i]])))))

  ch  <- cayleyR:::cube_adi_children(g$ptr, states)
  # Children first, then the parents, so both come back from a single call.
  all_sc <- score_fn(rbind(ch$children, states))

  n_dec  <- nrow(states)
  kid_sc <- all_sc[seq_len(nrow(ch$children))]
  par_sc <- all_sc[nrow(ch$children) + seq_len(n_dec)]

  # A solved child is taken whatever the model says, so it never costs a rank --
  # score it as if the model had seen it.
  kid_sc[ch$solved] <- -Inf

  ranks <- integer(n_dec)
  togos <- integer(n_dec)
  d     <- 0L
  for (cs in cases) {
    n <- length(cs$solution)
    for (i in seq_len(n)) {
      d    <- d + 1L
      # Child a of decision d sits at (d-1)*n_moves + a, the layout
      # cube_adi_children promises.
      sc   <- kid_sc[(d - 1L) * length(moves) + seq_along(moves)]
      want <- which(moves == cs$solution[[i]])
      ranks[[d]] <- sum(sc < sc[[want]]) + 1L
      togos[[d]] <- n - i + 1L    # moves left from this state along the path
    }
  }

  errs <- split(par_sc - togos, togos)
  list(ranks = ranks, errs = errs)
}

rank_res <- list()
for (nm in names(scorers)) {
  cat("\n", nm, ":\n", sep = "")
  r <- rank_of(scorers[[nm]])
  rank_res[[nm]] <- r

  n <- length(r$ranks)
  # Cumulative, because that is the question a beam width asks: with width k,
  # is the move still in the frontier at all.
  cat("  where the helping move lands among the 18 children:\n")
  for (k in c(1L, 2L, 3L, 5L, 10L, 18L)) {
    cat(sprintf("    top %2d : %5.1f%%\n", k, 100 * mean(r$ranks <= k)))
  }
  cat(sprintf("  mean rank %.2f over %d decisions\n", mean(r$ranks), n))

  # A greedy descent needs rank 1 at every one of `depth` steps in a row. That
  # product is the honest prediction for the greedy column of the test table.
  p1 <- mean(r$ranks == 1L)
  cat(sprintf("  greedy would arrive %.1f%% of the time (%.3f ^ %d)\n",
              100 * p1 ^ opt$depth, p1, opt$depth))
}

hr("estimate against true remaining distance")

# The bias column is what steers a beam. A model that reads 3.5 whether the
# state is two moves out or five cannot rank children near the goal, however
# good its MAE looks averaged over all depths.
for (nm in names(rank_res)) {
  cat("\n", nm, ":\n", sep = "")
  cat("   togo     n    mean_est    bias     sd\n")
  e <- rank_res[[nm]]$errs
  for (k in sort(as.integer(names(e)))) {
    v <- e[[as.character(k)]]
    cat(sprintf("  %5d %5d %10.2f %7.2f %6.2f\n",
                k, length(v), k + mean(v), mean(v), stats::sd(v)))
  }
}

# ---------------------------------------------------------------------------
# 4. The beam, watched
# ---------------------------------------------------------------------------

hr("where the beam drops the reference path")

# The same beam as test_cube3_transformer.R, with three counters added: whether
# a state of the reference path is still in the frontier, how many children came
# back that had been scored before, and how wide the surviving frontier is. The
# search itself is unchanged -- measuring a different beam would say nothing
# about the one in the table.
watched_beam <- function(score_fn, cs) {
  ref_keys <- vapply(cs$trail, key_of, character(1L))

  frontier <- matrix(cs$state, nrow = 1L)
  paths    <- list(character(0))
  visited  <- new.env(hash = TRUE, parent = emptyenv())
  assign(key_of(frontier[1L, ]), TRUE, envir = visited)

  last_on_track <- 0L      # last step at which the reference was in the frontier
  rescored      <- 0L      # children scored that had been scored before
  scored        <- 0L
  seen_all      <- new.env(hash = TRUE, parent = emptyenv())

  for (step in seq_len(opt$bsteps)) {
    ch   <- cayleyR:::cube_adi_children(g$ptr, frontier)
    kids <- ch$children
    nf   <- nrow(frontier)

    kid_paths <- vector("list", nrow(kids))
    for (i in seq_len(nf)) {
      for (a in seq_along(moves)) {
        kid_paths[[(i - 1L) * length(moves) + a]] <- c(paths[[i]], moves[a])
      }
    }

    done <- which(ch$solved)
    if (length(done)) {
      return(list(found = TRUE, len = length(kid_paths[[done[[1L]]]]),
                  steps = step, last_on_track = last_on_track,
                  rescored = rescored, scored = scored))
    }

    kk    <- keys_of(kids)
    fresh <- which(vapply(kk, function(k) is.null(visited[[k]]), logical(1L)))
    if (!length(fresh)) break
    kids      <- kids[fresh, , drop = FALSE]
    kid_paths <- kid_paths[fresh]
    kk        <- kk[fresh]

    # How much of this batch is work already done once. Counted before scoring,
    # over every child the beam is about to pay for.
    for (k in kk) {
      scored <- scored + 1L
      if (!is.null(seen_all[[k]])) rescored <- rescored + 1L
      assign(k, TRUE, envir = seen_all)
    }

    sc   <- score_fn(kids)
    keep <- utils::head(order(sc), opt$beam)
    frontier <- kids[keep, , drop = FALSE]
    paths    <- kid_paths[keep]
    for (k in kk[keep]) assign(k, TRUE, envir = visited)

    if (any(kk[keep] %in% ref_keys)) last_on_track <- step
  }
  list(found = FALSE, len = NA_integer_, steps = opt$bsteps,
       last_on_track = last_on_track, rescored = rescored, scored = scored)
}

for (nm in names(scorers)) {
  cat("\n", nm, ":\n", sep = "")
  out <- lapply(cases, function(cs) watched_beam(scorers[[nm]], cs))

  ok <- vapply(out, function(o) o$found, logical(1L))
  cat(sprintf("  solved %d/%d\n", sum(ok), length(ok)))
  if (any(ok)) {
    cat(sprintf("  mean length on solved  : %.1f (reference is %d)\n",
                mean(vapply(out[ok], function(o) o$len, numeric(1L))),
                opt$depth))
  }

  # For the ones that failed: how far along the reference path the beam still
  # held a state of it. Zero means it left the trail on the very first step.
  if (any(!ok)) {
    lost <- vapply(out[!ok], function(o) o$last_on_track, integer(1L))
    cat("  on the failures, last step the reference was still in the beam:\n")
    tb <- table(lost)
    for (k in names(tb)) cat(sprintf("    step %2s : %d\n", k, tb[[k]]))
    cat(sprintf("  median %.1f of %d steps walked\n",
                stats::median(lost), opt$bsteps))
  }

  rs <- vapply(out, function(o) o$rescored, numeric(1L))
  tot <- vapply(out, function(o) o$scored, numeric(1L))
  cat(sprintf("  rescored children: %.1f%% of the %.0f scored per solve\n",
              100 * sum(rs) / sum(tot), mean(tot)))
}

hr("read")

cat("A high rank-1 rate with a low greedy arrival says the search is the\n",
    "problem, not the estimator. A rank that is often outside the beam width\n",
    "says the opposite: no width will fix it and the model needs retraining.\n",
    "Bias that grows as togo falls means the estimator flattens near the goal,\n",
    "which is where a beam has least to steer by.\n", sep = "")

cat("\nDone.\n")
