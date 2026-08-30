#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# An estimator and a search: 1D CNN over cube pieces, then beam search.
#
# The shape is the standard one for this problem -- a network that scores how
# far a state is from solved, and a search that uses those scores as its
# heuristic. Neither half works alone: the network is never accurate enough to
# be followed one move at a time, and the search has nothing to steer by
# without it.
#
# ---- What is read, and how --------------------------------------------------
#
# Not the 96 stickers as ninety-six independent numbers. They are grouped into
# the 56 pieces they physically belong to,
#
#     8 corners   3 stickers each   type 0
#    24 wings     2 stickers each   type 1
#    24 centres   1 sticker each    type 2
#
# so that the network is told from the start that stickers 0, 51 and 64 are one
# corner rather than having to discover it. The layout is transcribed from
# /mnt/Data2/DS_projects/444/archive (unified_training/models.py, _cube4_layout)
# rather than derived here: which three stickers make a corner is a fact about
# the cube, and copying a working table beats re-deriving one. Its indices are
# 0-based and are shifted by one on the way in; the check below confirms every
# sticker lands in exactly one piece.
#
# That makes each cube a sequence of 56 positions with 21 channels -- which is
# what a 1D convolution wants, and why the pieces are the sequence rather than
# the stickers.
#
# ---- What is predicted ------------------------------------------------------
#
# The length of the scramble. A random word of d moves can occasionally be
# undone in fewer -- R R' is two moves and none -- so the label is an upper
# bound rather than the true distance, and the run reports how many states were
# seen with more than one label. At depth 10 that was 346 in 20000, which is
# small enough to train on and too large to ignore in the last decimal.
#
# ---- What is NOT here -------------------------------------------------------
#
# The DQN fine-tuning that would normally follow this pre-training. What is
# built is the pre-trained estimator and the search on top of it, which is
# enough to answer whether the pieces encoding and the beam earn their place.
#
# One ggmlR quirk, found by bisecting: a dropout layer placed BEFORE a conv1d
# aborts inside ggml (`GGML_ASSERT(ggml_is_padded_1d(a))`). Batch norm in the
# same position is fine. So dropout appears only after the flatten, which is
# where it does most of its work in a net this small anyway.
#
# Usage:  Rscript diag_model_pieces.R [n_train] [n_test] [epochs] [max_depth] [beam]
#   e.g.  Rscript diag_model_pieces.R 20000 4000 30 10 64
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args    <- commandArgs(trailingOnly = TRUE)
N_TRAIN <- if (length(args) >= 1) as.integer(args[1]) else 20000L
N_TEST  <- if (length(args) >= 2) as.integer(args[2]) else 4000L
EPOCHS  <- if (length(args) >= 3) as.integer(args[3]) else 30L
MAXD    <- if (length(args) >= 4) as.integer(args[4]) else 10L
BEAM    <- if (length(args) >= 5) as.integer(args[5]) else 64L

g  <- cube_group(4)
id <- group_identity(g)
mv <- cube_moves(4); names(mv) <- cube_move_names(4)
nm <- names(mv)

## ---- the piece layout ----------------------------------------------------
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

N_PIECE <- 56L
SLOTS   <- 3L
PIECE_IX <- rbind(CORNERS + 1L,
                  cbind(WINGS + 1L, 0L),
                  cbind(matrix(CENTRES + 1L, ncol = 1L), 0L, 0L))
PIECE_TYPE <- c(rep(0L, 8L), rep(1L, 24L), rep(2L, 24L))

stopifnot(nrow(PIECE_IX) == N_PIECE, ncol(PIECE_IX) == SLOTS)
stopifnot(identical(sort(as.integer(PIECE_IX[PIECE_IX > 0L])), seq_len(96L)))

## ---- data ----------------------------------------------------------------
make_data <- function(n, seed) {
  set.seed(seed)
  states <- matrix(0L, n, 96L)
  y <- integer(n)
  for (i in seq_len(n)) {
    d <- sample.int(MAXD, 1L)
    s <- id
    for (m in sample(nm, d, replace = TRUE)) s <- s[mv[[m]]]
    states[i, ] <- s
    y[i] <- d
  }
  list(states = states, y = y)
}

## ---- the piece encoding --------------------------------------------------
## 56 positions x 21 channels. Per piece: for each of three slots the colour
## sitting there as six indicators (all zero where the piece has no such slot,
## which is how a wing is told from a corner without a separate mask), then
## three indicators for the piece type.
##
## Colours as indicators rather than as 0..5: the colours are names, and
## feeding 5 where the meaning is "blue" invents an order that is not there.
SLOT_DIM  <- 6L
PIECE_DIM <- SLOTS * SLOT_DIM + 3L           # 21

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

## ---- the estimator -------------------------------------------------------
## Two 1D convolutions with batch norm, then a dense head. The convolutions
## look along the piece sequence, so a filter sees a piece together with its
## neighbours in the layout; batch norm keeps the two layers trainable at this
## learning rate; the dropout sits after the flatten for the ggmlR reason given
## at the top.
FILTERS <- 64L
KERNEL  <- 3L

build_cnn <- function() {
  ggml_model_sequential() |>
    ggml_layer_conv_1d(FILTERS, kernel_size = KERNEL, activation = "relu",
                       padding = "same", input_shape = c(N_PIECE, PIECE_DIM),
                       name = "conv1") |>
    ggml_layer_batch_norm(name = "bn1") |>
    ggml_layer_conv_1d(FILTERS, kernel_size = KERNEL, activation = "relu",
                       padding = "same", name = "conv2") |>
    ggml_layer_batch_norm(name = "bn2") |>
    ggml_layer_flatten() |>
    ggml_layer_dropout(0.1, stochastic = TRUE, name = "drop") |>
    ggml_layer_dense(128L, activation = "relu", name = "head") |>
    ggml_layer_dense(1L, name = "depth") |>
    ggml_compile(optimizer = "adam", loss = "mse", metrics = character(0))
}

## ---- run: train ----------------------------------------------------------
cat(sprintf("estimator + beam | n=4, depths 1..%d\n", MAXD))
cat(sprintf("%d train, %d test, %d epochs, beam %d\n", N_TRAIN, N_TEST,
            EPOCHS, BEAM))
cat(sprintf("%d pieces x %d channels; 1D CNN, %d filters, kernel %d\n\n",
            N_PIECE, PIECE_DIM, FILTERS, KERNEL))

cat("building data ... ")
tr <- make_data(N_TRAIN, 1L)
te <- make_data(N_TEST, 999L)
Xtr <- encode_pieces(tr$states)
Xte <- encode_pieces(te$states)
cat("done\n")

keys <- apply(tr$states, 1L, function(r) paste(r, collapse = ","))
dup <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
collisions <- if (any(dup))
  sum(tapply(tr$y[dup], keys[dup], function(v) length(unique(v)) > 1L)) else 0L
cat(sprintf("label noise: %d states carry more than one depth (of %d)\n",
            collisions, N_TRAIN))

y_mu <- mean(tr$y); y_sd <- stats::sd(tr$y)
ytr  <- matrix((tr$y - y_mu) / y_sd, ncol = 1L)
base_mae <- mean(abs(te$y - y_mu))
cat(sprintf("baseline (always answer %.1f): MAE %.2f moves\n\n", y_mu, base_mae))

cat("training ... ")
t0 <- proc.time()[["elapsed"]]
# ggml_fit RETURNS the trained model; it does not train in place. Assigning the
# result back is what vignettes/keras-like-api.Rmd does throughout, and dropping
# it leaves an untrained network scoring like the mean.
model <- build_cnn()
invisible(utils::capture.output(
  model <- ggml_fit(model, Xtr, ytr, epochs = EPOCHS, batch_size = 64L,
                    verbose = 0)))
cat(sprintf("%.1f s\n\n", proc.time()[["elapsed"]] - t0))

pred <- as.numeric(ggml_predict(model, Xte)) * y_sd + y_mu
mae  <- mean(abs(pred - te$y))

cat("== the estimator ========================================\n\n")
cat(sprintf("  MAE        %.2f moves   (baseline %.2f)\n", mae, base_mae))
cat(sprintf("  within 1   %.0f%%\n", 100 * mean(abs(pred - te$y) <= 1)))
cat(sprintf("  exact      %.0f%%\n", 100 * mean(round(pred) == te$y)))

if (stats::sd(pred) < 0.05) {
  cat("\n  The predictions barely vary: the model has not converged.\n")
} else if (mae >= base_mae * 0.95) {
  cat("\n  No better than answering the mean.\n")
} else {
  cat(sprintf("\n  Better than the mean by %.0f%%.\n", 100 * (1 - mae / base_mae)))
}

cat("\n  by depth:\n")
for (d in seq_len(MAXD)) {
  k <- te$y == d
  if (!any(k)) next
  cat(sprintf("    %2d moves  n=%4d   predicted %5.2f +- %.2f\n",
              d, sum(k), mean(pred[k]), stats::sd(pred[k])))
}
cat("\n  A model whose predictions rise with the depth has the ORDERING the\n")
cat("  search needs, even where the numbers themselves are off. That, not the\n")
cat("  MAE, is what the beam below actually consumes.\n")

## ---- run: the search -----------------------------------------------------
##
## Beam search. From the states currently held, generate every successor, score
## them all in one batch, keep the best BEAM, and repeat. A solved cube among
## the successors ends it.
##
## Why a beam rather than following the model one move at a time: a greedy walk
## commits to the single best-looking move and cannot recover when the estimate
## is wrong, and the estimate is wrong often -- the MAE above is in moves, not
## in fractions of one. A beam of width w tolerates being wrong about the best
## move as long as the right one stays in the top w, which is exactly the
## slack an imperfect heuristic needs.
##
## `visited` holds every state already expanded, so the beam does not spend its
## width re-examining places it has been. Without it the commonest failure is a
## pair of inverse moves traded back and forth for ever.
cat(sprintf("\n== the beam (width %d) ==================================\n\n", BEAM))

key_of <- function(m) apply(m, 1L, function(r) paste(r, collapse = ","))

beam_solve <- function(state, width = BEAM, max_steps = 3L * MAXD) {
  if (identical(as.integer(state), id)) return(list(ok = TRUE, len = 0L))
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

    hit <- which(apply(kids, 1L, function(row) identical(row, id)))
    if (length(hit)) return(list(ok = TRUE, len = length(kid_paths[[hit[1]]]),
                                 word = kid_paths[[hit[1]]]))

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
  }
  list(ok = FALSE)
}

N_SOLVE <- 20L
sv <- make_data(N_SOLVE, 4242L)
solved_n <- 0L; got <- integer(0); want <- integer(0)
for (i in seq_len(N_SOLVE)) {
  r <- beam_solve(sv$states[i, ])
  mark <- if (isTRUE(r$ok)) sprintf("%d moves", r$len) else "not found"
  cat(sprintf("  cube %2d  scrambled %2d  ->  %s\n", i, sv$y[i], mark))
  if (isTRUE(r$ok)) {
    solved_n <- solved_n + 1L
    got <- c(got, r$len); want <- c(want, sv$y[i])
  }
  flush(stdout())
}

cat(sprintf("\n  solved %d of %d within %d steps\n", solved_n, N_SOLVE,
            3L * MAXD))
if (solved_n) {
  cat(sprintf("  their scrambles were %.1f moves on average, the solutions %.1f\n",
              mean(want), mean(got)))
  cat(sprintf("  found shorter than the scramble on %d of %d\n",
              sum(got < want), solved_n))
}
if (solved_n == 0L) {
  cat("\n  Nothing solved. Widening the beam is the first thing to try, and a\n")
  cat("  flat `by depth` table above is the sign that it will not help.\n")
}
