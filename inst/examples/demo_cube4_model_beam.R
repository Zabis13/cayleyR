#!/usr/bin/env Rscript
# Beam search on the 4x4x4 with a trained model.
#
# The model scores all 24 moves on a state; the search keeps the best `width`
# states it has reached and expands all of them each step. Greedy descent is
# this with width 1, and it stalls: some fifteen moves from the solved cube the
# values flatten out, the right move stops being the first one, and a search
# that keeps only the first loses it. A beam keeps enough of the ordering that
# the right move survives even when it is not on top.
#
# Measured on one cube: greedy sat between 16 and 18 and then cycled; a beam of
# 10 went 28.9 -> 11.1 in forty steps and was still descending.
#
# Depth barely matters. A cube 30 random moves from solved scores about 27, one
# 1000 moves from solved about 29 -- past thirty moves a cube is as far away as
# it gets, and the model says so.
#
# Usage:
#   Rscript inst/examples/demo_cube4_model_beam.R [depth] [width] [steps] [cubes]
#
#   depth   quarter turns of scramble          (default 1000)
#   width   beam width, 1 is greedy            (default 10)
#   steps   give up after this many            (default 40)
#   cubes   how many to solve                  (default 1)
#
# Examples:
#   Rscript inst/examples/demo_cube4_model_beam.R              # one cube
#   Rscript inst/examples/demo_cube4_model_beam.R 1000 30 100  # wider, longer
#   Rscript inst/examples/demo_cube4_model_beam.R 30 1 60      # greedy, to see it stall

suppressMessages(library(ggmlR))
suppressMessages(library(cayleyR))

args  <- commandArgs(trailingOnly = TRUE)
arg   <- function(i, default) if (length(args) >= i) as.integer(args[i]) else default

DEPTH <- arg(1L, 1000L)
WIDTH <- arg(2L, 20L)
STEPS <- arg(3L, 400L)
CUBES <- arg(4L, 1L)
ARCHIVE <- Sys.getenv("CUBE4_ARCHIVE", "/mnt/Data2/DS_projects/444/archive")
TRACE <- CUBES == 1L      # the per-step trace is only readable for one cube

# ---- the model and its alphabet --------------------------------------------

model <- pt_transformer_load(file.path(ARCHIVE, "model/model.pth"))

# The generators the model scores, in the model's own action order. The order
# matters: its output is 24 numbers and nothing in them says which move each
# belongs to.
gen    <- paste(readLines(file.path(ARCHIVE, "generators/p002.json"),
                          warn = FALSE), collapse = "")
mn_txt <- regmatches(gen, regexpr("\"move_names\"\\s*:\\s*\\[[^]]*\\]", gen))
moves  <- gsub("\"", "", regmatches(mn_txt, gregexpr("\"-?[a-z][0-9]\"",
                                                     mn_txt))[[1]])
mv_txt <- regmatches(gen, regexpr("\"moves\"\\s*:\\s*\\[.*\\]\\s*,\\s*\"move_names\"",
                                  gen))
nums   <- as.integer(regmatches(mv_txt, gregexpr("-?[0-9]+", mv_txt))[[1]])
G      <- lapply(seq_len(24L),
                 function(i) nums[((i - 1L) * 96L + 1L):(i * 96L)] + 1L)
names(G) <- moves

is_solved <- function(c)
  all(vapply(0:5, function(f)
    length(unique(c[f * 16L + 1:16])) == 1L, logical(1)))

key_of <- function(c) paste(c, collapse = ",")

# ---- the search ------------------------------------------------------------
#
# Visited states are shared across the whole beam, not kept per branch. Two
# branches that arrive at the same cube are one branch, and keeping both would
# spend the width on a single line of play. It also covers every length of
# cycle at once -- banning the move that undoes the last one does not, since
# four of the same quarter turn come back to where they started too.

beam_solve <- function(start, width, steps, trace = FALSE) {
  seen <- new.env(hash = TRUE, parent = emptyenv())
  assign(key_of(start), TRUE, envir = seen)

  # The beam as a matrix, one state per row, with the paths alongside. The
  # whole beam is scored in ONE call to the model rather than one call per
  # state: pt_forward() batches, and a tall matmul is what BLAS is for.
  beam  <- matrix(start, 1L)
  paths <- list(character(0))

  for (step in seq_len(steps)) {
    q <- pt_forward(model, beam)
    if (is.null(dim(q))) q <- matrix(q, 1L)

    n_beam <- nrow(beam)
    n_mv   <- length(moves)

    # Every successor of every state at once: row (b-1)*n_mv + j is state b
    # after move j, which is the order as.vector(t(q)) puts the values in.
    nxt <- matrix(0L, n_beam * n_mv, ncol(beam))
    for (j in seq_len(n_mv))
      nxt[seq.int(j, by = n_mv, length.out = n_beam), ] <-
        beam[, G[[moves[j]]], drop = FALSE]

    vals <- as.vector(t(q))
    ord  <- order(vals)

    for (i in ord) {
      if (!is_solved(nxt[i, ])) next
      b <- (i - 1L) %/% n_mv + 1L
      return(list(found = TRUE, steps = step,
                  path = c(paths[[b]], moves[(i - 1L) %% n_mv + 1L])))
    }

    keep_rows <- integer(0)
    keep_path <- list()
    for (i in ord) {
      if (length(keep_rows) >= width) break
      k2 <- key_of(nxt[i, ])
      if (exists(k2, envir = seen, inherits = FALSE)) next
      assign(k2, TRUE, envir = seen)
      keep_rows <- c(keep_rows, i)
      b <- (i - 1L) %/% n_mv + 1L
      keep_path[[length(keep_path) + 1L]] <-
        c(paths[[b]], moves[(i - 1L) %% n_mv + 1L])
    }
    if (!length(keep_rows))
      return(list(found = FALSE, why = "beam exhausted", steps = step))

    beam  <- nxt[keep_rows, , drop = FALSE]
    paths <- keep_path

    if (trace)
      cat(sprintf("%5d  %8.3f  %8.3f  %7d\n", step, vals[keep_rows[1L]],
                  vals[keep_rows[length(keep_rows)]], length(vals)))
  }
  list(found = FALSE, why = "out of steps", steps = steps)
}

# ---- run -------------------------------------------------------------------

cat(sprintf("depth %d   width %d   steps %d   cubes %d\n",
            DEPTH, WIDTH, STEPS, CUBES))

set.seed(2026)
g <- cube_group(4)
rows <- vector("list", CUBES)

for (i in seq_len(CUBES)) {
  state <- generate_state(group = g, n_moves = DEPTH)
  # A package state is sticker numbers; the model wants colours in Kaggle's
  # face order. Colours first, then the relabelling.
  start <- cube_santa_state_out(cube_colours(state, 4), 4)

  if (TRACE) {
    cat("value at the start:",
        sprintf("%.2f\n\n", min(pt_forward(model, start))))
    cat(sprintf("%5s  %8s  %8s  %7s\n", "step", "best", "worst", "cands"))
  }

  t0  <- proc.time()[["elapsed"]]
  res <- beam_solve(start, WIDTH, STEPS, trace = TRACE)
  el  <- proc.time()[["elapsed"]] - t0

  rows[[i]] <- data.frame(
    cube = i,
    moves = if (isTRUE(res$found)) length(res$path) else NA_integer_,
    steps = res$steps, sec = el,
    status = if (isTRUE(res$found)) "solved" else res$why,
    stringsAsFactors = FALSE)

  if (TRACE) {
    cat("\n")
    if (isTRUE(res$found)) {
      cat("SOLVED in", length(res$path), "moves", sprintf("(%.1f s)\n", el))
      cat("path:", paste(res$path, collapse = " "), "\n")
    } else {
      cat("not solved:", res$why, "after", res$steps, "steps",
          sprintf("(%.1f s)\n", el))
    }
  } else {
    cat(sprintf("  cube %d: %-14s %5s moves  %6.1f s\n", i, rows[[i]]$status,
                if (is.na(rows[[i]]$moves)) "-" else rows[[i]]$moves, el))
  }
}

# ---- statistics ------------------------------------------------------------

if (CUBES > 1L) {
  res <- do.call(rbind, rows)
  ok  <- res$status == "solved"

  cat("\n", strrep("=", 58), "\n", sep = "")
  cat(sprintf("%d cubes at depth %d, beam width %d\n\n", CUBES, DEPTH, WIDTH))
  cat(sprintf("solved    : %d/%d\n", sum(ok), nrow(res)))
  if (any(ok)) {
    cat(sprintf("moves     : median %g, range %g-%g\n",
                stats::median(res$moves[ok]), min(res$moves[ok]),
                max(res$moves[ok])))
    cat(sprintf("seconds   : median %.1f, range %.1f-%.1f\n",
                stats::median(res$sec[ok]), min(res$sec[ok]), max(res$sec[ok])))
  }
  if (any(!ok)) {
    cat("\nnot solved:\n")
    print(table(res$status[!ok]))
  }
}
