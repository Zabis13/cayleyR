#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# The scramble against the Q-model, one move at a time.
#
# A solved cube is walked away from by random moves, and every position along
# the way is solved again by the model alone. The walk knows the way back in
# as many moves as it took; the model does not, and has only its eighteen
# numbers to go on. What the run shows is where that breaks, and what happens
# to a solution when the very same cube is made one move easier.
#
# The positions are printed from the far end inwards -- depth 10, then 9, then
# 8 -- so consecutive blocks are the same cube with one move of the scramble
# taken off. That is the point of the shape: a model that is steering rather
# than guessing should answer a slightly easier cube with a slightly shorter
# word, and the block above tells you whether it did.
#
# Two ways of using the same net are run side by side:
#
#   greedy   take the smallest q of the eighteen, and again from there.
#            Nothing but what the net learnt, so it fails exactly where the
#            net's ordering fails.
#   beam     keep the best `beam` states at every level and expand them all.
#            Where beam solves what greedy could not, the net had the right
#            move ranked but not first -- a search recovers that, a descent
#            cannot.
#
# Three things are worth reading per block:
#
#   one move less   how the length changed against the block above. An easier
#                   cube should want a shorter word.
#   shared tail     how many CLOSING moves the two solutions have in common.
#                   Read from the end, because the block below is the same
#                   cube one move closer in: its solution is the tail of this
#                   one, not its head. A short tail on nearly identical cubes
#                   means close inputs give unrelated outputs.
#   q along path    what the model thought at each step it took. It should
#                   fall towards zero; where it falls and the cube is not
#                   solved, the net is confidently wrong rather than undecided.
#
# Usage:  Rscript diag_cube3_q.R [depth] [seed] [trials] [name=value ...]
#   e.g.  Rscript diag_cube3_q.R 10 1 1        one walk, in full
#         Rscript diag_cube3_q.R 10 1 20       twenty walks, summary
#
#   net=/mnt/Data2/DS_projects/333   model directory, or the folder holding
#                                    them; given the folder, the newest wins
#   beam=32      width of the beam search; 1 makes it greedy
#   budget=0     move cap for both searches; 0 means 3x the depth
#   backend=auto
# ---------------------------------------------------------------------------

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

args <- commandArgs(trailingOnly = TRUE)

# Positional first -- depth, seed, trials -- then any name=value that follows.
pos <- args[!grepl("=", args, fixed = TRUE)]
kvs <- args[grepl("=", args, fixed = TRUE)]

DEPTH  <- if (length(pos) >= 1) as.integer(pos[1]) else 10L
SEED   <- if (length(pos) >= 2) as.integer(pos[2]) else 1L
TRIALS <- if (length(pos) >= 3) as.integer(pos[3]) else 1L

defaults <- list(net = "/mnt/Data2/DS_projects/333", backend = "auto",
                 beam = 32L, budget = 0L)
opt <- defaults
for (a in kvs) {
  kv  <- strsplit(a, "=", fixed = TRUE)[[1L]]
  key <- kv[[1L]]
  if (is.null(defaults[[key]]))
    stop("unknown parameter: ", key, "\navailable: ",
         paste(names(defaults), collapse = ", "))
  opt[[key]] <- if (is.character(defaults[[key]])) kv[[2L]]
                else as.integer(kv[[2L]])
  if (!is.character(opt[[key]]) && is.na(opt[[key]]))
    stop("not a number: ", a)
}

if (is.na(DEPTH) || DEPTH < 1L) stop("depth must be at least 1")
BUDGET  <- if (opt$budget > 0L) opt$budget else 3L * DEPTH
VERBOSE <- TRIALS == 1L

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------

# net= takes either a model directory or the folder they live in.
net_dir <- path.expand(opt$net)
if (!file.exists(file.path(net_dir, "model.ggml"))) {
  found <- list.files(net_dir, full.names = TRUE)
  found <- found[file.exists(file.path(found, "model.ggml"))]
  if (length(found) == 0L)
    stop("no model in ", net_dir,
         "\nrun this first: Rscript inst/examples/train_cube3_q_online.R")
  net_dir <- found[[which.max(file.mtime(file.path(found, "model.ggml")))]]
  if (length(found) > 1L)
    cat(length(found), "models in", opt$net, "-- taking the newest\n")
}

meta  <- readRDS(file.path(net_dir, "meta.rds"))
model <- ggmlR::ggml_load_model(file.path(net_dir, "model.ggml"),
                                backend = opt$backend)

g   <- cube_group(3)
lay <- cube_piece_layout(g)
mv  <- g$moves
n_a <- length(mv)
inv <- g$inverse_of
solved_state <- seq_len(g$n)

if (!identical(as.character(meta$moves), as.character(mv)))
  stop("the model was trained on a different move set")

# A compiled graph is built for one batch size and rebuilding it at solve time
# runs out of context memory, so every call goes through at the same width and
# short inputs are padded with repeats. That is what adi_predict_padded is for
# -- it pads up only, so the beam is capped at the batch below.
BATCH <- 32L

# The scaling is undone with what the training run derived from `depth` alone
# and stored beside the weights. Getting it wrong shifts every value by a
# constant and leaves the ordering intact, so it would not change a single
# move either search takes -- only the printed q.
#
# adi_encode is simply the encoder the model was trained through
# (train_cube3_q_online.R builds its batches with the same call); the name is
# where it first appeared, not a method being mixed in here.
score <- function(states) {
  x <- cayleyR:::adi_encode(states, "transformer", lay)
  cayleyR:::adi_predict_padded(model, x, BATCH, n_a) * meta$y_sd + meta$y_mu
}

if (opt$beam > BATCH) {
  cat(sprintf("beam %d is wider than the batch %d -- capping\n",
              opt$beam, BATCH))
  opt$beam <- BATCH
}

# ---------------------------------------------------------------------------
# The two searches
# ---------------------------------------------------------------------------

# Both read the same eighteen numbers and differ only in how many states they
# carry forward. What q means decides the shape: the net was trained so that
# q[a] at a state is the distance AFTER playing a, so the move to take is the
# argmin of the row of the state you are standing on. One call per level, not
# one per child.

children_of <- function(states) cayleyR:::cube_adi_children(g$ptr, states)

greedy <- function(state) {
  path <- character(0)
  qs   <- numeric(0)
  st   <- state
  for (i in seq_len(BUDGET)) {
    ch <- children_of(matrix(st, nrow = 1L))
    if (any(ch$solved)) {
      a <- which(ch$solved)[[1L]]
      return(list(ok = TRUE, path = c(path, mv[a]), q = c(qs, 0)))
    }
    v <- score(matrix(st, nrow = 1L))[1L, ]
    a <- which.min(v)
    path <- c(path, mv[a])
    qs   <- c(qs, v[[a]])
    st   <- as.integer(ch$children[a, ])
  }
  list(ok = FALSE, path = path, q = qs)
}

# The beam keeps `width` states, expands every one, and keeps the best `width`
# again. Duplicate states are dropped: without that the beam fills with the
# same position reached by different words and the effective width collapses.
#
# What ranks a child is q[a] read at its PARENT for the move that made it --
# never anything re-read at the child itself. That is forced by how the net
# was trained: only two of the eighteen outputs ever carried a label, and the
# other sixteen were pulled to the mean depth, so a row is two trained numbers
# among sixteen bland ones. Taking min over a row therefore mostly samples
# untrained outputs and does not order states by distance at all -- against
# true depth it runs 4.79, 1.94, 3.15, 4.89 for depths 1 to 4, so a state one
# move from solved scores WORSE than one two moves away. Read at the parent,
# q[a] is a trained output and rises 2.1, 3.4, 5.0, 5.7 (correlation 0.81
# against 0.65).
#
# Greedy is untouched by this: it uses only the argmin POSITION within a
# single row. The beam is the one comparing magnitudes ACROSS states, and
# that is what the untrained outputs cannot support.
beam_search <- function(state, width) {
  if (width <= 1L) return(greedy(state))
  cur   <- matrix(state, nrow = 1L)
  paths <- list(character(0))

  for (lvl in seq_len(BUDGET)) {
    ch <- children_of(cur)
    if (any(ch$solved)) {
      i      <- which(ch$solved)[[1L]]
      parent <- ((i - 1L) %/% n_a) + 1L   # state-major: 18 children per state
      a      <- ((i - 1L) %%  n_a) + 1L
      return(list(ok = TRUE, path = c(paths[[parent]], mv[a])))
    }

    # One row of eighteen per frontier state. Flattened by t(), row-major,
    # it lines up with the state-major children: entry i is the value of the
    # move that made child i.
    kid_q     <- as.numeric(t(score(cur)))
    kids      <- ch$children
    kid_paths <- unlist(lapply(seq_along(paths), function(p)
      lapply(seq_len(n_a), function(a) c(paths[[p]], mv[a]))),
      recursive = FALSE)

    keep      <- !duplicated(kids, MARGIN = 1L)
    kids      <- kids[keep, , drop = FALSE]
    kid_paths <- kid_paths[keep]
    kid_q     <- kid_q[keep]

    ord   <- order(kid_q)[seq_len(min(width, nrow(kids)))]
    cur   <- kids[ord, , drop = FALSE]
    paths <- kid_paths[ord]
  }
  list(ok = FALSE, path = paths[[1L]])
}

# ---------------------------------------------------------------------------
# One walk, solved at every prefix
# ---------------------------------------------------------------------------

# no_undo, the same rule the training walks used: a move never immediately
# undoes the one before it, so the walk does not sit still.
make_walk <- function(seed) {
  set.seed(seed)
  scramble <- character(DEPTH)
  last <- 0L
  for (k in seq_len(DEPTH)) {
    pick <- if (last == 0L) seq_len(n_a) else setdiff(seq_len(n_a), inv[last])
    last <- sample(pick, 1L)
    scramble[k] <- mv[last]
  }
  scramble
}

invert <- function(w) rev(mv[inv[match(w, mv)]])

# The solve is checked rather than trusted: the word is replayed on the
# scrambled cube and the result compared with solved. cube_apply_word takes
# the whole word as one string, so the moves are joined rather than passed as
# a vector.
verify <- function(state, path)
  length(path) > 0L &&
  identical(as.integer(cube_apply_word(state, paste(path, collapse = " "))),
            solved_state)

run_walk <- function(seed) {
  scramble <- make_walk(seed)
  st   <- solved_state
  rows <- vector("list", DEPTH)
  for (k in seq_len(DEPTH)) {
    st <- as.integer(cube_apply_word(st, scramble[k]))
    t0 <- proc.time()[["elapsed"]]
    gr <- greedy(st)
    t1 <- proc.time()[["elapsed"]]
    bm <- beam_search(st, opt$beam)
    t2 <- proc.time()[["elapsed"]]
    rows[[k]] <- list(depth = k, scramble = scramble[seq_len(k)],
                      back = invert(scramble[seq_len(k)]), state = st,
                      greedy = gr, g_ok = gr$ok && verify(st, gr$path),
                      g_secs = t1 - t0,
                      beam = bm, b_ok = bm$ok && verify(st, bm$path),
                      b_secs = t2 - t1)
  }
  list(scramble = scramble, rows = rows)
}

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------

cat(sprintf("scramble vs q | depth %d, seed%s %d%s\n", DEPTH,
            if (TRIALS > 1L) "s" else "", SEED,
            if (TRIALS > 1L) sprintf("..%d", SEED + TRIALS - 1L) else ""))
cat(sprintf("model: %s\n", basename(net_dir)))
cat(sprintf("       trained to depth %d, %s states, pair %.1f%%, top1 %.1f%%\n",
            meta$depth,
            format(meta$states_seen, big.mark = ",", scientific = FALSE),
            100 * meta$pair_accuracy, 100 * meta$top1_accuracy))
cat(sprintf("search: greedy and beam %d, both capped at %d moves\n",
            opt$beam, BUDGET))
if (DEPTH > meta$depth)
  cat(sprintf("NOTE: asking past the training depth of %d\n", meta$depth))
cat("\n")

if (VERBOSE) {
  w <- run_walk(SEED)
  cat(sprintf("the walk: %s\n\n", paste(w$scramble, collapse = " ")))
  cat("Printed from the far end inwards: the same cube with one move of the\n")
  cat("scramble taken off each time, so the solutions can be read against one\n")
  cat("another. A model that is steering would answer a slightly easier cube\n")
  cat("with a slightly shorter word; where the length jumps instead, the two\n")
  cat("positions went down different routes.\n\n")

  prev <- NULL
  for (row in rev(w$rows)) {
    cat(sprintf("== %d move%s from solved =================================\n",
                row$depth, if (row$depth > 1) "s" else ""))
    cat(sprintf("  scrambled by : %s\n", paste(row$scramble, collapse = " ")))
    cat(sprintf("  undone by    : %s   (%d moves)\n",
                paste(row$back, collapse = " "), length(row$back)))

    gr <- row$greedy
    cat(sprintf("  greedy       : %s   (%d moves, %.2f s)\n",
                if (row$g_ok) "SOLVED" else "gave up",
                length(gr$path), row$g_secs))
    if (row$g_ok && length(gr$path) <= 24L)
      cat(sprintf("    path       : %s\n", paste(gr$path, collapse = " ")))
    cat(sprintf("    q along    : %s\n",
                paste(sprintf("%.1f", head(gr$q, 12L)), collapse = " ")))

    bm <- row$beam
    cat(sprintf("  beam %-4d    : %s   (%d moves, %.2f s)\n",
                opt$beam, if (row$b_ok) "SOLVED" else "gave up",
                length(bm$path), row$b_secs))
    if (row$b_ok && length(bm$path) <= 24L)
      cat(sprintf("    path       : %s\n", paste(bm$path, collapse = " ")))

    if (row$b_ok && !row$g_ok) {
      cat("    the search found what the descent could not:\n")
      cat("    the right move was ranked, but not first\n")
    }

    # against the cube one move harder, which was printed just above
    if (!is.null(prev) && row$b_ok && prev$ok) {
      d <- length(bm$path) - prev$len
      cat(sprintf("  one move less: %+d moves than at depth %d   (%s)\n",
                  d, prev$depth,
                  if (abs(d) <= 1) "much the same route"
                  else if (d < 0) "a shorter route opened"
                  else "a longer route was taken"))
      # Compared from the END, not the start. The block below is the same cube
      # one move CLOSER to solved, so its solution is the tail of this one, not
      # its head: at depth 3 "D' F E'" against depth 2 "F E'" shares two moves
      # read backwards and none read forwards.
      k <- 0L
      while (k < min(length(bm$path), length(prev$path)) &&
             bm$path[length(bm$path) - k] == prev$path[length(prev$path) - k])
        k <- k + 1L
      cat(sprintf("  shared tail  : %d moves\n", k))
    }
    cat("\n")
    prev <- if (row$b_ok) list(ok = TRUE, len = length(bm$path),
                               path = bm$path, depth = row$depth)
            else list(ok = FALSE)
    flush(stdout())
  }

  # Where it stopped working, which is what the blocks above are for.
  g_ok <- vapply(w$rows, `[[`, logical(1), "g_ok")
  b_ok <- vapply(w$rows, `[[`, logical(1), "b_ok")
  cat(sprintf("greedy solved %d of %d", sum(g_ok), DEPTH))
  if (any(!g_ok)) cat(sprintf(", first failure at depth %d", which(!g_ok)[[1L]]))
  cat("\n")
  cat(sprintf("beam   solved %d of %d", sum(b_ok), DEPTH))
  if (any(!b_ok)) cat(sprintf(", first failure at depth %d", which(!b_ok)[[1L]]))
  cat("\n")

} else {
  cat(sprintf("%d walks, one line per depth\n\n", TRIALS))
  cat(sprintf("%6s %8s %10s %10s %10s %10s\n",
              "depth", "walk", "greedy", "g_moves", "beam", "b_moves"))

  n_g <- integer(DEPTH); n_b <- integer(DEPTH)
  l_g <- numeric(DEPTH); l_b <- numeric(DEPTH)

  for (t in seq_len(TRIALS)) {
    w <- run_walk(SEED + t - 1L)
    for (row in w$rows) {
      k <- row$depth
      if (row$g_ok) { n_g[k] <- n_g[k] + 1L; l_g[k] <- l_g[k] + length(row$greedy$path) }
      if (row$b_ok) { n_b[k] <- n_b[k] + 1L; l_b[k] <- l_b[k] + length(row$beam$path) }
    }
    flush(stdout())
  }

  for (k in seq_len(DEPTH))
    cat(sprintf("%6d %8d %9.0f%% %10s %9.0f%% %10s\n", k, k,
                100 * n_g[k] / TRIALS,
                if (n_g[k]) sprintf("%.1f", l_g[k] / n_g[k]) else "-",
                100 * n_b[k] / TRIALS,
                if (n_b[k]) sprintf("%.1f", l_b[k] / n_b[k]) else "-"))

  cat("\n  walk     the moves it took to scramble, and so to undo\n")
  cat("  greedy   how often argmin q alone got back, and in how many moves\n")
  cat(sprintf("  beam     the same with a beam of %d\n", opt$beam))
  cat("\n  Where beam solves and greedy does not, the net has the right move\n")
  cat("  ranked but not first. Where neither solves, the ordering is gone.\n")
}
