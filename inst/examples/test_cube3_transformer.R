#!/usr/bin/env Rscript
# The transformer against the classical methods on the 3x3x3.
#
# Loads the network trained by train_cube3_transformer.R and measures it on the
# same states as Kociemba and CFOP: a hundred random scrambles of ten moves,
# each solved three ways, compared on solution length and time.
#
# The comparison is uneven by construction, which is worth knowing up front:
#
#   KociembaMod  two-phase search. Solves any state, and its length barely
#                depends on how scrambled that state was.
#   CFOP         layer by layer. Also solves any state, but its length is set
#                by the method's own structure, not by the distance to solved.
#   adi          a greedy descent on a learned distance. Solves only what falls
#                within the horizon it was trained on, but on close states it
#                can beat both -- it walks towards the goal rather than
#                executing a program.
#   adi_beam     the ADI estimator again, searched rather than descended: a
#                beam of `beam` candidates instead of a single best child.
#                Added with beam=N; omitted at 0.
#   depth        the same descent on an estimator trained the other way round:
#                labelled by scramble length instead of by its own opinion of a
#                state's children. Added with dnet=; omitted when that is empty.
#
# The two learned methods share the descent exactly, so what separates them in
# the table is their estimators and nothing else.
#
# So on a ten-move scramble the honest question is not "who is shorter" in
# general, but "does the network reach solved at all, and in how many moves".
#
# The first thing the script does after loading is compare the loaded network's
# values against what training produced. If ggml_load_model() returned anything
# other than what was written, everything below would be measuring random
# weights, and that needs to surface immediately.
#
# Run with:  Rscript inst/examples/test_cube3_transformer.R [name=value ...]
#
#   net=/mnt/Data2/DS_projects/333  the ADI model, or the folder holding models
#   dnet=<dir>               a depth estimator to add as a fourth method
#   states=100               how many states
#   depth=10                 scramble length
#   budget=50                move ceiling for the greedy descent
#   backend=auto seed=2026 shorten=2

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

defaults <- list(
  net     = "/mnt/Data2/DS_projects/333",
  dnet    = "",      # depth estimator to add as a fourth method; "" to skip
  beam    = 0L,      # beam width for the ADI model; 0 leaves the beam out
  bsteps  = 30L,     # how far the beam may walk
  states  = 100L,
  depth   = 10L,
  budget  = 50L,
  batch   = 128L,
  shorten = 2L,      # BFS shortener depth; see the note below
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
# 1. Load
# ---------------------------------------------------------------------------

hr("load")

# net= takes either a model directory or the folder they live in. Given the
# folder, the newest model wins: the training script names each run after its
# settings, so a folder holds several and the last one trained is what someone
# testing right after training means.
net_dir <- path.expand(opt$net)

if (!file.exists(file.path(net_dir, "meta.rds"))) {
  found <- list.files(net_dir, full.names = TRUE)
  found <- found[file.exists(file.path(found, "meta.rds"))]
  if (length(found) == 0L) {
    stop("no saved network in ", net_dir,
         "\nrun this first: Rscript inst/examples/train_cube3_transformer.R")
  }
  net_dir <- found[[which.max(file.mtime(file.path(found, "meta.rds")))]]
  if (length(found) > 1L) {
    cat(length(found), "models in", opt$net, "-- taking the newest\n")
    for (f in found) cat(sprintf("  %s%s\n", basename(f),
                                 if (identical(f, net_dir)) "  <-" else ""))
  }
}

t0  <- proc.time()[["elapsed"]]
net <- cube_adi_load(net_dir, backend = opt$backend)
cat(sprintf("loaded from %s in %.2fs\n", net_dir,
            proc.time()[["elapsed"]] - t0))
print(net)

g <- net$group
cat("\nmoves:", paste(g$moves, collapse = " "), "\n")

# The group was rebuilt from permutations rather than stored whole. Check it is
# the same one: the solved state must have the right length, and every move must
# actually change it.
id <- group_identity(g)
stopifnot(length(id) == net$state_len)
moved <- vapply(g$moves, function(m) !identical(group_apply(g, id, m), id),
                logical(1L))
stopifnot(all(moved))
cat("group    : rebuilt,", length(g$moves), "moves all disturb the cube\n")

# ---------------------------------------------------------------------------
# 2. Did the network survive the round trip
# ---------------------------------------------------------------------------

# The value must grow with depth. Had load returned uninitialised weights, this
# would be noise or a constant, and everything below would measure the wrong
# network.
#
# generate_walk with exact = TRUE, so a row labelled "depth 10" really is ten
# moves out. The ADI scrambler draws its depth uniformly from 1 to what it is
# given, which is right for training but would make every row here an average
# over everything shallower -- the deep rows pulled down hardest, exactly where
# the spread is being read.
hr("network after loading")

cat("mean value by scramble depth:\n")
probe <- c(1L, 3L, 6L, 10L, 15L, 20L)
vals  <- numeric(length(probe))
for (i in seq_along(probe)) {
  s <- generate_walk(g, n = opt$batch, n_moves = probe[[i]], exact = TRUE)
  vals[[i]] <- mean(cayleyR:::adi_value_of(net$value, s$states, opt$batch,
                                           net$arch, net$layout))
  cat(sprintf("  depth %2d : %6.2f\n", probe[[i]], vals[[i]]))
}

# Three outcomes, and they need telling apart. A network that never left the
# solved state reads as a flat line at 1 -- undertrained, but loaded fine. One
# that came back as noise has no order to it at all. Only a spread that actually
# widens says the weights arrived and mean something. Requiring "not falling" is
# not enough: a constant passes that, which is exactly what an untrained network
# produces.
spread <- vals[[length(vals)]] - vals[[1L]]
drops  <- sum(diff(vals) < -0.05)

cat(if (spread < 0.5)
      "FLAT: value barely moves with depth -- undertrained, train for longer\n"
    else if (drops > 1L)
      "WARNING: value is not monotone -- misloaded or undertrained\n"
    else
      sprintf("OK: value spans %.2f from depth 1 to 20 -- the weights are there\n",
              spread))

# ---------------------------------------------------------------------------
# 3. The methods
# ---------------------------------------------------------------------------

# The classical methods return a Solution -- stages, a path, a found flag. The
# search and the network have no stages and return the word alone, so they are
# dressed in the same shape here rather than given their own branch in the
# measuring loop below.
kociemba_as_method <- function(state) {
  path <- cube_kociemba(state)$path
  list(path = path, found = length(path) > 0L)
}

transformer_as_method <- function(state) {
  r <- cube_adi_solve(net, state, budget = opt$budget, batch_size = opt$batch)
  list(path = r$path, found = isTRUE(r$solved))
}

# The same estimator, searched instead of descended. A greedy walk needs the
# helping move to score lowest of the eighteen at every step; measured on this
# model that happens about two thirds of the time, which over ten steps is under
# one run in fifty. The move is in the top three nine times out of ten, though,
# so a beam that carries a handful of candidates keeps the path it needs without
# the estimator having to be any better.
#
# Width is the whole knob. At width 1 this is the greedy descent again, and its
# cost per step grows linearly with the width -- but on a GPU the successors go
# in one batch, so a wide beam costs far less than its width suggests.
#
# The estimator is a parameter so that the same search can be run over either
# network. That is the only way to say what the beam is worth as against what
# the estimator under it is worth: hold one fixed and vary the other.
make_beam <- function(score_fn) function(state) {
  if (all(state == seq_along(state)))
    return(list(path = character(0), found = TRUE))

  key_of <- function(m) apply(m, 1L, paste, collapse = ",")

  frontier <- matrix(as.integer(state), nrow = 1L)
  paths    <- list(character(0))
  visited  <- new.env(hash = TRUE, parent = emptyenv())
  assign(key_of(frontier)[[1L]], TRUE, envir = visited)

  for (step in seq_len(opt$bsteps)) {
    ch   <- cayleyR:::cube_adi_children(g$ptr, frontier)
    kids <- ch$children
    nf   <- nrow(frontier)

    # Child a of frontier row i sits at (i-1)*n_moves + a, so the path leading
    # to each successor is its parent's path with that move appended.
    kid_paths <- vector("list", nrow(kids))
    for (i in seq_len(nf)) {
      for (a in seq_along(g$moves)) {
        kid_paths[[(i - 1L) * length(g$moves) + a]] <- c(paths[[i]], g$moves[a])
      }
    }

    # cube_adi_children already reports which successors are the solved state,
    # so the check costs nothing and happens before any scoring.
    done <- which(ch$solved)
    if (length(done)) return(list(path = kid_paths[[done[[1L]]]], found = TRUE))

    kk    <- key_of(kids)
    fresh <- which(vapply(kk, function(k) is.null(visited[[k]]), logical(1L)))
    if (!length(fresh)) break
    kids      <- kids[fresh, , drop = FALSE]
    kid_paths <- kid_paths[fresh]
    kk        <- kk[fresh]

    sc   <- score_fn(kids)
    keep <- utils::head(order(sc), opt$beam)
    frontier <- kids[keep, , drop = FALSE]
    paths    <- kid_paths[keep]
    for (k in kk[keep]) assign(k, TRUE, envir = visited)
  }
  list(path = character(0), found = FALSE)
}

# The depth estimator answers the same question the ADI value head does -- how
# far from solved -- so it can drive the same descent. It is loaded here rather
# than being a cube_adi_net because it was trained the other way round: one
# model, labelled by scramble length, with the target scaling in its meta.rds.
#
# The walk below is deliberately the same as cube_adi_solve's, down to the rule
# about revisiting: expand the children, score them, step into the best one not
# yet stood on, stop when solved or out of budget. Only the source of the score
# differs, so a difference in the table is a difference between the estimators
# and not between two solvers.
depth_solver <- NULL
if (nzchar(opt$dnet)) {
  dn <- path.expand(opt$dnet)
  if (!file.exists(file.path(dn, "model.ggml"))) {
    found <- list.files(dn, full.names = TRUE)
    found <- found[file.exists(file.path(found, "model.ggml"))]
    if (length(found) == 0L) stop("no depth estimator in ", dn)
    dn <- found[[which.max(file.mtime(file.path(found, "model.ggml")))]]
  }
  dmeta  <- readRDS(file.path(dn, "meta.rds"))
  dmodel <- ggmlR::ggml_load_model(file.path(dn, "model.ggml"),
                                   backend = opt$backend)
  dlay   <- cube_piece_layout(g)
  cat("depth model:", basename(dn),
      sprintf("(trained to %d, MAE %.2f)\n", dmeta$depth, dmeta$mae))

  # ggml_predict refuses a batch smaller than the model's own batch_size, and a
  # descent scores exactly 18 children at a time -- fewer than that. The rows are
  # padded out by repeating the first one and the extras are dropped again; the
  # network is stateless between rows, so the repeats change nothing.
  DPAD <- 64L
  depth_score <- function(states) {
    n <- nrow(states)
    s <- if (n >= DPAD) states else
      states[c(seq_len(n), rep(1L, DPAD - n)), , drop = FALSE]
    x <- cayleyR:::adi_encode(s, "transformer", dlay)
    v <- as.numeric(ggmlR::ggml_predict(dmodel, x)) * dmeta$y_sd + dmeta$y_mu
    v[seq_len(n)]
  }

  depth_solver <- function(state) {
    cur    <- as.integer(state)
    path   <- character(0)
    seen   <- new.env(hash = TRUE, parent = emptyenv())
    assign(paste(cur, collapse = ","), TRUE, envir = seen)

    for (step in seq_len(opt$budget)) {
      if (all(cur == seq_along(cur)))
        return(list(path = path, found = TRUE))

      ch <- cayleyR:::cube_adi_children(g$ptr, matrix(cur, nrow = 1L))
      v  <- depth_score(ch$children)
      v[ch$solved] <- -Inf     # a solved child ends it, whatever the model says

      took <- FALSE
      for (a in order(v)) {
        cand <- ch$children[a, ]
        key  <- paste(cand, collapse = ",")
        if (!is.null(seen[[key]])) next
        assign(key, TRUE, envir = seen)
        cur  <- cand
        path <- c(path, g$moves[a])
        took <- TRUE
        break
      }
      if (!took) break         # every child already visited
    }
    list(path = path, found = all(cur == seq_along(cur)))
  }
}

# Timed through to the end of the shortener: the figure is what a short word
# costs, not what the solver alone took.
#
# Depth 2 rather than the default 5: the cube's alphabet is 18 moves against
# TopSpin's 3, so the neighbourhood grows as 18^d. Measured on one solve, depth 2
# took 0.06s and saved 14 moves, depth 3 took 0.78s for 16, and depth 4 took 14s
# for the same 16.
solve_once <- function(method, state) {
  t0 <- proc.time()[["elapsed"]]
  r  <- try(method(state), silent = TRUE)

  if (inherits(r, "try-error")) {
    return(list(status = "error", moves = NA_integer_, short = NA_integer_,
                seconds = proc.time()[["elapsed"]] - t0))
  }
  if (!isTRUE(r$found)) {
    return(list(status = "unsolved", moves = NA_integer_, short = NA_integer_,
                seconds = proc.time()[["elapsed"]] - t0))
  }

  sh <- short_path_bfs(r$path, state, depth = opt$shorten, group = g)
  list(status = "solved", moves = length(r$path), short = sh$new_length,
       seconds = proc.time()[["elapsed"]] - t0)
}

methods <- list(KociembaMod = kociemba_as_method,
                CFOP        = cube_solve_cfop,
                adi         = transformer_as_method)

# Greedy and beam for each estimator that is present, so the table separates the
# two things being compared: the estimators against each other under one search,
# and the search against itself under one estimator.
if (opt$beam > 0L) {
  methods$adi_beam <- make_beam(function(kids)
    cayleyR:::adi_value_of(net$value, kids, opt$batch, net$arch, net$layout))
}
if (!is.null(depth_solver)) {
  methods$depth <- depth_solver
  if (opt$beam > 0L) methods$depth_beam <- make_beam(depth_score)
}

# ---------------------------------------------------------------------------
# 4. Measure
# ---------------------------------------------------------------------------

hr(sprintf("%d states, exactly %d moves out", opt$states, opt$depth))

# All the methods get THE SAME state -- otherwise there is nothing to compare.
# The scramble is built from the group's own moves, the ones the network trained
# on.
#
# exact = TRUE, so every state really is `depth` moves out. The ADI scrambler
# used to stand here and draws its depth uniformly from 1 to what it is given:
# a run labelled "depth 10" was in fact a mixture averaging five and a half, and
# the learned methods were being credited for states one or two moves from
# solved. Their share of the table falls accordingly, and what is left of it
# means what it says.
#
# The walk also reports the word it took, so `depth` is an upper bound on each
# state's true distance -- worth knowing when reading a solver that comes in
# under it.
walks <- generate_walk(g, n = opt$states, n_moves = opt$depth, exact = TRUE)

rows <- list()

for (i in seq_len(opt$states)) {
  state <- as.integer(walks$states[i, ])

  for (nm in names(methods)) {
    r <- solve_once(methods[[nm]], state)
    rows[[length(rows) + 1L]] <- data.frame(
      state = i, method = nm, status = r$status,
      moves = r$moves, short = r$short, seconds = r$seconds,
      stringsAsFactors = FALSE)
  }

  if (i %% 10L == 0L) cat(sprintf("  %d/%d\n", i, opt$states))
}

res <- do.call(rbind, rows)

# ---------------------------------------------------------------------------
# 5. Summary
# ---------------------------------------------------------------------------

# Averaged over the states that finished. A method that solved none has no mean
# to report, and saying so is more use than a NaN.
hr("summary")

summary_row <- function(nm) {
  d  <- res[res$method == nm, ]
  ok <- d$status == "solved"
  data.frame(
    method     = nm,
    solved     = sprintf("%d/%d", sum(ok), nrow(d)),
    mean_moves = if (any(ok)) round(mean(d$moves[ok]), 1) else NA_real_,
    mean_short = if (any(ok)) round(mean(d$short[ok]), 1) else NA_real_,
    mean_sec   = if (any(ok)) round(mean(d$seconds[ok]), 2) else NA_real_,
    errors     = sum(d$status == "error"),
    stringsAsFactors = FALSE)
}

print(do.call(rbind, lapply(names(methods), summary_row)), row.names = FALSE)

# The learned methods walk downhill and stop where the estimate stops pointing
# anywhere, so their solved count is the number that carries information; the
# classical two solve everything and are here to say what a solution costs.
#
# Both networks run the same descent, so the gap between them is the gap between
# their estimators. What a descent needs is not accuracy but that the helping
# move score lowest of the eighteen, every step of the way -- ten steps at even
# 60% a step is under one run in a hundred, which is why a better estimator can
# move these numbers far less than its error suggests.
for (nm in intersect(c("adi", "adi_beam", "depth", "depth_beam"),
                     names(methods))) {
  tr <- res[res$method == nm, ]
  cat(sprintf("\n%s: solved %d of %d (%.0f%%)\n", nm,
              sum(tr$status == "solved"), nrow(tr),
              100 * mean(tr$status == "solved")))

  if (any(tr$status == "solved")) {
    both <- tr$state[tr$status == "solved"]
    mean_on <- function(m) {
      d <- res[res$method == m & res$state %in% both & res$status == "solved", ]
      if (nrow(d) == 0L) NA_real_ else mean(d$short)
    }
    cat("  on those states, moves after shortening:\n")
    for (m in names(methods)) {
      cat(sprintf("    %-12s %5.1f\n", m, mean_on(m)))
    }
  }
}

cat("\nDone.\n")
