#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# The schedule against the model, on the same cubes.
#
# bench_full_face_shorten.R measures what the reduction costs on a properly
# scrambled cube: all six starting faces tried, the shortest answer kept, then
# short_path_bfs over the result. This runs that same benchmark and puts a
# second solver beside it -- a 1D CNN over cube pieces, used as the heuristic
# of a beam search.
#
# Both answers go through the same shortener and the same colour check, so the
# two columns are comparable move for move.
#
# ---- What is being compared -------------------------------------------------
#
#   schedule   cube_solve4(start_face = "full"): six reductions, keep the
#              shortest finished solution. Always finishes. Costs 150-300 moves
#              whatever the cube, because the schedule runs in full regardless
#              of how far from solved the cube actually is.
#
#   model      a network that scores how far a state is from solved, and a beam
#              search that expands the best-scoring states. Finds near-optimal
#              words where it works at all, and finds nothing where the
#              estimator has gone flat.
#
# The second is not a drop-in replacement and the run is built to show where it
# stops being one. A beam is given a step budget; when it runs out the row says
# so rather than the script pretending a solve happened.
#
# ---- Two depths, and they are not the same thing ----------------------------
#
# TRAIN_MAX is the range the model was trained on -- 80 by default, matching
# train_cube4_cnn.R. It is passed here NOT to limit anything but because the
# training target was scaled by that range: reading the weights back with a
# different TRAIN_MAX returns numbers on the wrong scale, and nothing crashes
# when it happens. The beam just follows a quietly stretched heuristic. So the
# two scripts are handed the same number by hand.
#
# BEAM_STEPS is how far the beam may walk at inference, and it is a separate
# question. A 500-move cube is deeper than any training scramble, and its walk
# home is longer than 80 moves, so the search is allowed 200 steps regardless
# of what the model was trained on. Whether the answers out there mean anything
# is what the `by depth` table from the training script is for: where its steps
# flatten, the beam is walking blind until it gets back under that depth.
#
# ---- The model comes from train_cube4_cnn.R ---------------------------------
#
# This script does not train. It loads what train_cube4_cnn.R saved and stops
# with an instruction if there is nothing there.
#
# Usage:  Rscript bench_model_vs_schedule.R [n_cubes] [scramble] [train_max] [beam] [model]
#   e.g.  Rscript bench_model_vs_schedule.R 10 500 80 256
#         Rscript bench_model_vs_schedule.R 10 20 80 256 /tmp/other.ggml
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args      <- commandArgs(trailingOnly = TRUE)
N_CUBES   <- if (length(args) >= 1) as.integer(args[1]) else 10L
SCRAMBLE  <- if (length(args) >= 2) as.integer(args[2]) else 500L
TRAIN_MAX <- if (length(args) >= 3) as.integer(args[3]) else 80L
BEAM      <- if (length(args) >= 4) as.integer(args[4]) else 256L
MODEL     <- if (length(args) >= 5) path.expand(args[5]) else NA_character_

## How many moves the beam may spend before giving up. This is an inference
## limit and has nothing to do with TRAIN_MAX: the model was trained on
## scrambles up to 80 but is asked here about cubes far deeper, and the walk
## home from one of those is longer than any training scramble. A step holds
## BEAM states and expands 24 successors from each, so 200 steps at width 256
## is 1.2M states scored -- the budget is what keeps a hopeless cube from
## running for hours, not a claim about how far the model can see.
BEAM_STEPS    <- 200L
SHORTEN_DEPTH <- 3L
METHOD        <- "kociemba"

## Where train_cube4_cnn.R puts its model by default -- keep the two in step.
## Passing an explicit path as the fifth argument overrides this.
MODEL_DIR <- "/mnt/Data2/DS_projects/444/cayleyR"
default_model <- function(train_max)
  path.expand(file.path(MODEL_DIR,
                        sprintf("cube4_cnn_d%d_n%d_e%d.ggml",
                                train_max, 60000L, 40L)))

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N); names(mv) <- cube_move_names(N)
nm <- names(mv)
id <- group_identity(g)

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

## ---- the piece layout ----------------------------------------------------
## Transcribed from /mnt/Data2/DS_projects/444/archive, unified_training/
## models.py, _cube4_layout: which three stickers make a corner is a fact about
## the cube, and copying a working table beats re-deriving one.
CORNERS <- rbind(
  c(0, 51, 64), c(3, 35, 48), c(12, 16, 67), c(15, 19, 32),
  c(28, 79, 80), c(31, 44, 83), c(47, 60, 95), c(63, 76, 92))
WINGS <- rbind(
  c(1, 50), c(2, 49), c(4, 65), c(7, 34), c(8, 66), c(11, 33),
  c(13, 17), c(14, 18), c(20, 71), c(23, 36), c(24, 75), c(27, 40),
  c(29, 81), c(30, 82), c(39, 52), c(43, 56), c(45, 87), c(46, 91),
  c(55, 68), c(59, 72), c(61, 94), c(62, 93), c(77, 88), c(78, 84))
CENTRES <- as.integer(vapply(0:5, function(f) f * 16L + c(5L, 6L, 9L, 10L),
                             integer(4)))

N_PIECE   <- 56L
SLOTS     <- 3L
SLOT_DIM  <- 6L
PIECE_DIM <- SLOTS * SLOT_DIM + 3L
PIECE_IX  <- rbind(CORNERS + 1L, cbind(WINGS + 1L, 0L),
                   cbind(matrix(CENTRES + 1L, ncol = 1L), 0L, 0L))
PIECE_TYPE <- c(rep(0L, 8L), rep(1L, 24L), rep(2L, 24L))
stopifnot(identical(sort(as.integer(PIECE_IX[PIECE_IX > 0L])), seq_len(96L)))

encode_pieces <- function(states) {
  n <- nrow(states)
  out <- array(0, dim = c(n, N_PIECE, PIECE_DIM))
  col <- (states - 1L) %/% 16L
  for (p in seq_len(N_PIECE)) {
    for (sl in seq_len(SLOTS)) {
      ix <- PIECE_IX[p, sl]
      if (ix == 0L) next
      cvec <- col[, ix]
      base <- (sl - 1L) * SLOT_DIM
      for (cc in 0:5) out[, p, base + cc + 1L] <- (cvec == cc) * 1.0
    }
    out[, p, SLOTS * SLOT_DIM + PIECE_TYPE[p] + 1L] <- 1.0
  }
  out
}

make_data <- function(n, seed) {
  set.seed(seed)
  states <- matrix(0L, n, 96L)
  y <- integer(n)
  for (i in seq_len(n)) {
    d <- sample.int(TRAIN_MAX, 1L)
    s <- id
    for (m in sample(nm, d, replace = TRUE)) s <- s[mv[[m]]]
    states[i, ] <- s
    y[i] <- d
  }
  list(states = states, y = y)
}

## The scaling of the target has to survive a cached model, or a loaded one
## would return numbers on the wrong scale. It is a function of TRAIN_MAX
## alone -- depths are drawn uniformly from 1..TRAIN_MAX -- so it is recomputed
## rather than stored.
y_mu <- (1 + TRAIN_MAX) / 2
y_sd <- stats::sd(seq_len(TRAIN_MAX))

cat(sprintf("\n== model against schedule | %d cubes, %d moves from solved ==\n\n",
            N_CUBES, SCRAMBLE))
cat(sprintf("schedule   : red+%s, start_face = \"full\"\n", METHOD))
cat(sprintf("model      : 1D CNN over %d pieces, beam %d, %d steps max\n",
            N_PIECE, BEAM, BEAM_STEPS))
cat(sprintf("             trained on depths 1..%d\n", TRAIN_MAX))
cat(sprintf("shortener  : short_path_bfs, depth %d, on both answers\n\n",
            SHORTEN_DEPTH))

## ---- the model, trained elsewhere ----------------------------------------
model_path <- if (!is.na(MODEL)) MODEL else default_model(TRAIN_MAX)
if (!file.exists(model_path)) {
  cat(sprintf("no model at %s\n\n", model_path))
  cat("train one first:\n")
  cat(sprintf("  Rscript train_cube4_cnn.R %d 60000 40\n", TRAIN_MAX))
  cat("or pass an explicit path as the fifth argument.\n")
  quit(save = "no", status = 1L)
}
cat(sprintf("loading %s ... ", model_path))
model <- ggml_load_model(model_path)
cat("done\n")

## How well the estimator ranks, on fresh cubes of the depths it was trained
## for. Printed before the benchmark because a flat table here predicts a beam
## that solves nothing, and it is better to see that than to infer it.
va <- make_data(1500L, 777L)
vp <- as.numeric(ggml_predict(model, encode_pieces(va$states))) * y_sd + y_mu
cat(sprintf("\nestimator: MAE %.2f moves (baseline %.2f)\n",
            mean(abs(vp - va$y)), mean(abs(va$y - y_mu))))
qs <- unique(round(seq(1, TRAIN_MAX, length.out = 6)))
cat("  by depth: ")
cat(paste(vapply(qs, function(d) {
  k <- abs(va$y - d) < 0.5
  if (!any(k)) "" else sprintf("%d->%.1f", d, mean(vp[k]))
}, character(1)), collapse = "  "), "\n\n")

## ---- the beam ------------------------------------------------------------
key_of <- function(m) apply(m, 1L, function(r) paste(r, collapse = ","))

## A beam step at width 256 expands 6144 successors and scores them all: about
## 0.2 s of R for the keys and the visited set, plus 0.4 s in ggml_predict. Two
## hundred steps is therefore minutes per cube, so the walk reports where it is
## rather than sitting silent behind a \r that never advances.
beam_solve <- function(state, width = BEAM, max_steps = BEAM_STEPS,
                       note = NULL) {
  if (isTRUE(cube_is_colour_solved(state))) return(list(ok = TRUE, path = character(0)))
  frontier <- matrix(as.integer(state), nrow = 1L)
  paths <- list(character(0))
  visited <- new.env(hash = TRUE, parent = emptyenv())
  assign(key_of(frontier)[1], TRUE, envir = visited)

  for (step in seq_len(max_steps)) {
    nf <- nrow(frontier)
    kids <- matrix(0L, nf * length(nm), 96L)
    kid_paths <- vector("list", nf * length(nm))
    r <- 0L
    for (i in seq_len(nf)) {
      for (j in seq_along(nm)) {
        r <- r + 1L
        kids[r, ] <- frontier[i, ][mv[[nm[j]]]]
        kid_paths[[r]] <- c(paths[[i]], nm[j])
      }
    }

    # Solved is checked BY COLOUR: four indistinguishable centres to a face
    # mean a finished cube need not have its sticker numbers back where they
    # started, and a sticker-order test would walk straight past the answer.
    done <- which(apply(kids, 1L, cube_is_colour_solved))
    if (length(done)) {
      if (!is.null(note)) cat("\r", strrep(" ", 44), "\r", sep = "")
      return(list(ok = TRUE, path = kid_paths[[done[1]]]))
    }

    kk <- key_of(kids)
    fresh <- which(vapply(kk, function(k) is.null(visited[[k]]), logical(1)))
    if (!length(fresh)) break
    kids <- kids[fresh, , drop = FALSE]
    kid_paths <- kid_paths[fresh]
    kk <- kk[fresh]

    sc <- as.numeric(ggml_predict(model, encode_pieces(kids)))
    keep <- utils::head(order(sc), width)
    frontier <- kids[keep, , drop = FALSE]
    paths <- kid_paths[keep]
    for (k in kk[keep]) assign(k, TRUE, envir = visited)

    # The best score on the frontier is the one number worth watching: if it
    # stops falling, the beam has run out of gradient and the remaining steps
    # will not find anything either.
    if (!is.null(note) && step %% 10L == 0L) {
      cat(sprintf("\r  %s step %3d/%d  best %.1f   ", note, step, max_steps,
                  min(sc)))
      flush(stdout())
    }
  }
  if (!is.null(note)) cat("\r", strrep(" ", 44), "\r", sep = "")
  list(ok = FALSE)
}

## ---- the benchmark -------------------------------------------------------
shorten <- function(path, state) {
  if (!length(path)) return(path)
  sh <- tryCatch(short_path_bfs(path, state, depth = SHORTEN_DEPTH, group = g),
                 error = function(e) NULL)
  if (!is.null(sh) && !is.null(sh$path) && length(sh$path) < length(path) &&
      isTRUE(cube_is_colour_solved(replay(state, sh$path))))
    sh$path else path
}

cat(sprintf("%5s  %19s  %19s\n", "", "schedule", "model"))
cat(sprintf("%5s  %8s %8s  %8s %8s %8s\n",
            "cube", "moves", "short", "moves", "short", "secs"))

rows <- list()
set.seed(500)
for (i in seq_len(N_CUBES)) {
  walk  <- sample(nm, SCRAMBLE, replace = TRUE)
  state <- replay(id, walk)

  cat(sprintf("%5d  %8s %8s  %8s %8s %8s\r", i, "", "", "", "", "running"))
  flush(stdout())

  ## the schedule
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(cube_solve4(state, method = METHOD, start_face = "full"),
                  error = function(e) NULL)
  sch_raw <- if (!is.null(res) && isTRUE(res$found)) res$path else NULL
  sch_short <- if (!is.null(sch_raw)) shorten(sch_raw, state) else NULL
  t_sch <- proc.time()[["elapsed"]] - t0

  ## the model
  t0 <- proc.time()[["elapsed"]]
  bm <- beam_solve(state, note = sprintf("cube %d beam", i))
  mod_raw <- if (isTRUE(bm$ok)) bm$path else NULL
  mod_short <- if (!is.null(mod_raw)) shorten(mod_raw, state) else NULL
  t_mod <- proc.time()[["elapsed"]] - t0

  ## Every kept word is replayed and checked by colour. A short wrong answer
  ## would otherwise be the best-looking number on the row.
  sch_ok <- !is.null(sch_short) &&
    isTRUE(cube_is_colour_solved(replay(state, sch_short)))
  mod_ok <- !is.null(mod_short) &&
    isTRUE(cube_is_colour_solved(replay(state, mod_short)))

  fmt <- function(v, ok) if (is.null(v)) "--" else
    sprintf("%d%s", length(v), if (ok) "" else "!")
  cat(sprintf("%5d  %8s %8s  %8s %8s %8.1f\n", i,
              fmt(sch_raw, TRUE), fmt(sch_short, sch_ok),
              fmt(mod_raw, TRUE), fmt(mod_short, mod_ok), t_sch + t_mod))
  flush(stdout())

  rows[[i]] <- data.frame(
    cube = i,
    sch = if (is.null(sch_short)) NA_integer_ else length(sch_short),
    mod = if (is.null(mod_short)) NA_integer_ else length(mod_short),
    sch_raw = if (is.null(sch_raw)) NA_integer_ else length(sch_raw),
    mod_raw = if (is.null(mod_raw)) NA_integer_ else length(mod_raw),
    t_sch = t_sch, t_mod = t_mod,
    sch_ok = sch_ok, mod_ok = mod_ok)
}

tb <- do.call(rbind, rows)

cat("\n== summary ---------------------------------------------------\n\n")
cat(sprintf("  schedule solved : %d of %d\n", sum(tb$sch_ok), nrow(tb)))
cat(sprintf("  model solved    : %d of %d\n", sum(tb$mod_ok), nrow(tb)))

if (any(tb$sch_ok))
  cat(sprintf("\n  schedule : median %4.0f  mean %6.1f  (before shortening %6.1f)\n",
              stats::median(tb$sch[tb$sch_ok]), mean(tb$sch[tb$sch_ok]),
              mean(tb$sch_raw[tb$sch_ok])))
if (any(tb$mod_ok))
  cat(sprintf("  model    : median %4.0f  mean %6.1f  (before shortening %6.1f)\n",
              stats::median(tb$mod[tb$mod_ok]), mean(tb$mod[tb$mod_ok]),
              mean(tb$mod_raw[tb$mod_ok])))

both <- tb$sch_ok & tb$mod_ok
if (any(both)) {
  cat(sprintf("\n  on the %d cube%s both solved:\n", sum(both),
              if (sum(both) > 1) "s" else ""))
  cat(sprintf("    schedule %6.1f moves, model %6.1f -- model is %.0f%% of it\n",
              mean(tb$sch[both]), mean(tb$mod[both]),
              100 * mean(tb$mod[both]) / mean(tb$sch[both])))
  cat(sprintf("    model shorter on %d of %d\n",
              sum(tb$mod[both] < tb$sch[both]), sum(both)))
} else {
  cat("\n  No cube was solved by both, so there is nothing to compare move for\n")
  cat("  move. The columns above still say which solver reached an answer.\n")
}

cat(sprintf("\n  seconds  : schedule %.1f each, model %.1f each\n",
            mean(tb$t_sch), mean(tb$t_mod)))

if (!any(tb$mod_ok)) {
  cat("\n  The model solved nothing. At this scramble length every state looks\n")
  cat("  alike to an estimator trained on shallower cubes, so the beam has no\n")
  cat("  gradient to follow -- check the `by depth` line above: if it has gone\n")
  cat("  flat, a wider beam will not help and a deeper TRAIN_MAX is the lever.\n")
}
