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
#   no_cancel=1  walks refuse U D U', U U U and the like; 0: no_undo only
#   prec=default  matmul precision of the predictions: default (f16 tiles on an
#                 fp16 Vulkan device, ~1e-3 relative noise) or f32 (matches CPU)
#   tie=0.001     a state whose best and second-best move differ by less than
#                 this many moves counts as a near tie
#   cmp=0         1: score every frontier at the other precision too, and count
#                 how often the best move changes and the largest |dq| -- what
#                 the noise actually does to the search
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
                 beam = 32L, budget = 0L, no_cancel = 1L,
                 prec = "default", tie = 0.001, cmp = 0L)
opt <- defaults
for (a in kvs) {
  kv  <- strsplit(a, "=", fixed = TRUE)[[1L]]
  key <- kv[[1L]]
  if (is.null(defaults[[key]]))
    stop("unknown parameter: ", key, "\navailable: ",
         paste(names(defaults), collapse = ", "))
  opt[[key]] <- if (is.character(defaults[[key]])) kv[[2L]]
                else if (is.integer(defaults[[key]])) as.integer(kv[[2L]])
                else as.numeric(kv[[2L]])
  if (!is.character(opt[[key]]) && is.na(opt[[key]]))
    stop("not a number: ", a)
}

if (is.na(DEPTH) || DEPTH < 1L) stop("depth must be at least 1")
if (!opt$prec %in% c("default", "f32")) stop("prec must be default or f32")
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
score_x <- function(x, prec)
  cayleyR:::adi_predict_padded(model, x, BATCH, n_a, matmul_precision = prec) *
    meta$y_sd + meta$y_mu

# Near ties, and with cmp=1 what the other precision would have said, counted
# per scramble depth. A search can only be steered differently by the noise
# where two moves are closer than the noise itself.
OTHER <- if (opt$prec == "f32") "default" else "f32"
ties  <- new.env()
ties$depth <- 1L
for (k in c("rows", "near", "flips", "dq")) ties[[k]] <- numeric(DEPTH)

score <- function(states) {
  x <- cayleyR:::adi_encode(states, "transformer", lay)
  q <- score_x(x, opt$prec)
  k <- ties$depth
  s <- t(apply(q, 1L, sort, partial = 1:2))[, 1:2, drop = FALSE]
  ties$rows[k] <- ties$rows[k] + nrow(q)
  ties$near[k] <- ties$near[k] + sum(s[, 2L] - s[, 1L] < opt$tie)
  if (opt$cmp == 1L) {
    q2 <- score_x(x, OTHER)
    ties$flips[k] <- ties$flips[k] + sum(max.col(-q, "first") != max.col(-q2, "first"))
    ties$dq[k]    <- max(ties$dq[k], abs(q - q2))
  }
  q
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

# The same generator the training walks come from. no_cancel = 1 also refuses
# U D U', U U U and the like, which no_undo alone lets through and which make
# the depth an overstatement; no_cancel = 0 gives the old no_undo walks.
make_walk <- function(seed) {
  set.seed(seed)
  generate_walk(g, n = 1L, n_moves = DEPTH, no_undo = TRUE,
                no_cancel = opt$no_cancel == 1L)$moves[[1L]]
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

run_walk <- function(seed, label = NULL) {
  scramble <- make_walk(seed)
  st   <- solved_state
  rows <- vector("list", DEPTH)
  for (k in seq_len(DEPTH)) {
    st <- as.integer(cube_apply_word(st, scramble[k]))
    ties$depth <- k
    t1 <- proc.time()[["elapsed"]]
    bm <- beam_search(st, opt$beam)
    t2 <- proc.time()[["elapsed"]]
    rows[[k]] <- list(depth = k, scramble = scramble[seq_len(k)],
                      back = invert(scramble[seq_len(k)]), state = st,
                      beam = bm, b_ok = bm$ok && verify(st, bm$path),
                      b_secs = t2 - t1)
    if (!is.null(label)) {
      r <- rows[[k]]
      cat(sprintf("%s  depth %2d   %-7s %3s moves   %6.2f s\n", label, k,
                  if (r$b_ok) "SOLVED" else "gave up",
                  if (r$b_ok) as.character(length(bm$path)) else "-",
                  r$b_secs))
      flush(stdout())
    }
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
cat(sprintf("search: beam %d, capped at %d moves\n",
            opt$beam, BUDGET))
cat(sprintf("walks : no_cancel = %s\n",
            if (opt$no_cancel == 1L) "TRUE" else "FALSE (no_undo only)"))
cat(sprintf("q     : matmul %s, near tie below %g moves%s\n", opt$prec, opt$tie,
            if (opt$cmp == 1L) sprintf(", compared with %s", OTHER) else ""))
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

    bm <- row$beam
    cat(sprintf("  beam %-4d    : %s   (%d moves, %.2f s)\n",
                opt$beam, if (row$b_ok) "SOLVED" else "gave up",
                length(bm$path), row$b_secs))
    if (row$b_ok && length(bm$path) <= 24L)
      cat(sprintf("    path       : %s\n", paste(bm$path, collapse = " ")))

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
  b_ok <- vapply(w$rows, `[[`, logical(1), "b_ok")
  cat(sprintf("beam solved %d of %d", sum(b_ok), DEPTH))
  if (any(!b_ok)) cat(sprintf(", first failure at depth %d", which(!b_ok)[[1L]]))
  cat("\n")
  cat(sprintf("near ties (< %g moves): %.2f%% of %d frontier states", opt$tie,
              100 * sum(ties$near) / max(1, sum(ties$rows)), as.integer(sum(ties$rows))))
  if (opt$cmp == 1L)
    cat(sprintf("; best move flips at %s: %.2f%%, max |dq| %.2g", OTHER,
                100 * sum(ties$flips) / max(1, sum(ties$rows)), max(ties$dq)))
  cat("\n")

} else {
  cat(sprintf("%d walks, one line per depth\n\n", TRIALS))
  n_b <- integer(DEPTH)
  l_b <- numeric(DEPTH)
  s_b <- numeric(DEPTH)

  for (t in seq_len(TRIALS)) {
    w <- run_walk(SEED + t - 1L,
                  label = sprintf("walk %2d/%d", t, TRIALS))
    for (row in w$rows) {
      k <- row$depth
      s_b[k] <- s_b[k] + row$b_secs
      if (row$b_ok) { n_b[k] <- n_b[k] + 1L; l_b[k] <- l_b[k] + length(row$beam$path) }
    }
    cat("\n")
    flush(stdout())
  }

  cat(sprintf("%6s %8s %10s %10s %8s %8s%s\n", "depth", "walk", "beam", "b_moves",
              "secs", "near%",
              if (opt$cmp == 1L) sprintf(" %8s %9s", "flip%", "max|dq|") else ""))
  for (k in seq_len(DEPTH))
    cat(sprintf("%6d %8d %9.0f%% %10s %8.2f %7.2f%%%s\n", k, k,
                100 * n_b[k] / TRIALS,
                if (n_b[k]) sprintf("%.1f", l_b[k] / n_b[k]) else "-",
                s_b[k] / TRIALS,
                100 * ties$near[k] / max(1, ties$rows[k]),
                if (opt$cmp == 1L)
                  sprintf(" %7.2f%% %9.2g", 100 * ties$flips[k] / max(1, ties$rows[k]),
                          ties$dq[k]) else ""))

  cat("\n  walk     the moves it took to scramble, and so to undo\n")
  cat(sprintf("  beam     how often a beam of %d got back, and in how many moves\n",
              opt$beam))
  cat("  secs     mean time of one solve at that depth\n")
  cat(sprintf("  near%%    frontier states whose best two moves differ by < %g moves\n",
              opt$tie))
  if (opt$cmp == 1L) {
    cat(sprintf("  flip%%    frontier states whose best move changes at %s precision\n", OTHER))
    cat("  max|dq|  largest change of any q between the two precisions, in moves\n")
  }
}
