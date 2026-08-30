#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Where do the seconds go inside cube_adi_astar?
#
# A run of bench_cube3_sizes.R spent 700 seconds solving eight cubes that
# needed 2327 expanded nodes between them -- about 0.04 s a node, on a search
# whose per-node work is one forward pass over twelve children. The forward
# pass is not 0.04 s; something around it is.
#
# Three parts of the loop are candidates, and this measures all three rather
# than picking one. That order matters: the last four attempts to explain a
# slowdown in this package each named a cause confidently and each was wrong,
# and what settled every one of them was timing the parts separately.
#
#   queue    which(open) over every node ever created, then order() over every
#            live one, to take the best `batch`. O(N log N) in the size of the
#            open list, which grows by up to batch*12 an iteration.
#
#   closed   the loop over every child, looking each up in an R environment and
#            appending to add_ix with c(). The append is quadratic in the
#            number of new children, and there are thousands per iteration.
#
#   grow     doubling the node arrays with rbind/c, which copies the whole
#            state matrix each time it happens.
#
# Two more are timed as controls, because a share is only meaningful against
# the whole:
#
#   score    the network over the children -- the work the search exists to do
#   expand   cube_adi_children, the C++ move application
#
# ---- What the answer decides -----------------------------------------------
#
# If `queue` dominates, the fix is a partial selection and a live list that is
# not recomputed from scratch. If `closed` dominates, the fix is preallocating
# add_ix and hashing outside the loop -- a different change entirely. If
# `score` dominates, there is nothing to fix here at all and the 700 seconds
# were the network being asked 2327 times, which would make the whole idea of
# speeding the queue up a waste.
#
# The parts are also summed and checked against the measured total, so a
# breakdown that misses something says so instead of quietly adding up to less.
#
# Usage:  Rscript diag_astar_profile.R [depth] [cubes] [max_nodes]
#   e.g.  Rscript diag_astar_profile.R 14 4 60000
# ---------------------------------------------------------------------------

suppressMessages({
  library(cayleyR)
  library(ggmlR)
})

args      <- commandArgs(trailingOnly = TRUE)
DEPTH     <- if (length(args) >= 1) as.integer(args[1]) else 14L
CUBES     <- if (length(args) >= 2) as.integer(args[2]) else 4L
MAX_NODES <- if (length(args) >= 3) as.integer(args[3]) else 60000L

TRAIN_MIN    <- 2
BATCH_STATES <- 8192L
BATCH_SIZE   <- 2048L
WEIGHT       <- 0.6
ASTAR_BATCH  <- 200L

FACES <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g   <- cube_group(3, moves = FACES)
id  <- group_identity(g)
tbl <- cube_moves(3)

## A network good enough to make the search behave as it does in the benchmark.
## Its quality is not what is being measured -- but a network that steers badly
## expands more nodes, so it has to be trained at all, not stubbed out.
cat(sprintf("training %g minutes to have something to steer with\n", TRAIN_MIN))
set.seed(99)
net <- cube_adi_model(g, hidden = 512L, n_blocks = 3L, arch = "resnet",
                      encoding = "piece")
deadline <- proc.time()[["elapsed"]] + TRAIN_MIN * 60
repeat {
  net <- cube_adi_train(net, iterations = 10L, batch_states = BATCH_STATES,
                        max_depth = 20L, batch_size = BATCH_SIZE,
                        verbose = FALSE)
  if (proc.time()[["elapsed"]] >= deadline) break
}
cat("trained\n\n")

set.seed(2024)
cubes <- lapply(seq_len(CUBES), function(k) {
  s <- id
  for (m in sample(FACES, DEPTH, replace = TRUE)) s <- s[tbl[[m]]]
  s
})

## ---------------------------------------------------------------------------
## The search, rewritten only to carry a clock. Every line that does work is
## the line cube_adi_astar has; the timing calls are the only additions, so a
## step measured here is that step and not an approximation of it.
## ---------------------------------------------------------------------------
astar_timed <- function(net, state, weight, batch, max_nodes, batch_size) {
  g <- net$group
  n_moves <- net$n_moves
  cur <- as.integer(state)
  t <- c(queue = 0, expand = 0, score = 0, closed = 0, grow = 0, rest = 0)

  cap    <- max(4096L, batch * n_moves * 2L)
  states <- matrix(0L, cap, net$state_len)
  gcost  <- numeric(cap); fcost <- numeric(cap)
  parent <- integer(cap); pmove <- integer(cap)
  open   <- logical(cap)

  states[1L, ] <- cur
  gcost[1L] <- 0; fcost[1L] <- 0; parent[1L] <- 0L; pmove[1L] <- 0L
  open[1L] <- TRUE
  n_used <- 1L

  seen <- new.env(hash = TRUE, parent = emptyenv(), size = 1e5L)
  assign(as.character(cayleyR:::cube_adi_keys(matrix(cur, nrow = 1L))), 1L,
         envir = seen)

  expanded <- 0L; it <- 0L; solved <- FALSE; path_len <- NA_integer_
  t_start <- proc.time()[["elapsed"]]

  while (expanded < max_nodes) {
    it <- it + 1L
    t0 <- proc.time()[["elapsed"]]
    live <- which(open[seq_len(n_used)])
    if (!length(live)) break
    take <- if (length(live) <= batch) live else
      live[order(fcost[live])[seq_len(batch)]]
    open[take] <- FALSE
    expanded <- expanded + length(take)
    t1 <- proc.time()[["elapsed"]]; t["queue"] <- t["queue"] + t1 - t0

    ch <- cayleyR:::cube_adi_children(g$ptr, states[take, , drop = FALSE])
    kids <- ch$children
    t2 <- proc.time()[["elapsed"]]; t["expand"] <- t["expand"] + t2 - t1

    hit <- which(ch$solved)
    if (length(hit)) {
      k <- hit[1L]
      node <- take[(k - 1L) %/% n_moves + 1L]
      steps <- 1L
      while (node > 1L || parent[node] != 0L) {
        steps <- steps + 1L; node <- parent[node]
        if (node == 0L) break
      }
      solved <- TRUE; path_len <- steps
      break
    }

    kid_g <- rep(gcost[take], each = n_moves) + 1
    kid_h <- cayleyR:::adi_value_of(net$value, kids, batch_size, net$arch,
                                    net$layout)
    kid_f <- weight * kid_g + kid_h
    t3 <- proc.time()[["elapsed"]]; t["score"] <- t["score"] + t3 - t2

    keys <- as.character(cayleyR:::cube_adi_keys(kids))
    best_in_batch <- !duplicated(keys[order(kid_g)])[order(order(kid_g))]
    cand <- which(best_in_batch)
    add_ix <- integer(0)
    for (k in cand) {
      old <- seen[[keys[k]]]
      if (is.null(old)) {
        add_ix <- c(add_ix, k)
      } else if (kid_g[k] < gcost[old]) {
        gcost[old] <- kid_g[k]; fcost[old] <- kid_f[k]
        parent[old] <- take[(k - 1L) %/% n_moves + 1L]
        pmove[old] <- (k - 1L) %% n_moves + 1L
        open[old] <- TRUE
      }
    }
    t4 <- proc.time()[["elapsed"]]; t["closed"] <- t["closed"] + t4 - t3

    if (length(add_ix)) {
      if (n_used + length(add_ix) > cap) {
        new_cap <- cap
        while (n_used + length(add_ix) > new_cap) new_cap <- new_cap * 2L
        states <- rbind(states, matrix(0L, new_cap - cap, net$state_len))
        gcost <- c(gcost, numeric(new_cap - cap))
        fcost <- c(fcost, numeric(new_cap - cap))
        parent <- c(parent, integer(new_cap - cap))
        pmove <- c(pmove, integer(new_cap - cap))
        open <- c(open, logical(new_cap - cap))
        cap <- new_cap
      }
      slots <- n_used + seq_along(add_ix)
      states[slots, ] <- kids[add_ix, , drop = FALSE]
      gcost[slots] <- kid_g[add_ix]; fcost[slots] <- kid_f[add_ix]
      parent[slots] <- take[(add_ix - 1L) %/% n_moves + 1L]
      pmove[slots] <- (add_ix - 1L) %% n_moves + 1L
      open[slots] <- TRUE
      for (i in seq_along(add_ix)) assign(keys[add_ix[i]], slots[i], envir = seen)
      n_used <- n_used + length(add_ix)
    }
    t5 <- proc.time()[["elapsed"]]; t["grow"] <- t["grow"] + t5 - t4
  }

  total <- proc.time()[["elapsed"]] - t_start
  t["rest"] <- total - sum(t[c("queue", "expand", "score", "closed", "grow")])
  list(t = t, total = total, nodes = expanded, iters = it, solved = solved,
       len = path_len, open_end = sum(open[seq_len(n_used)]), n_used = n_used)
}

cat(sprintf("profiling %d cubes at depth %d, batch %d, cap %d nodes\n\n",
            CUBES, DEPTH, ASTAR_BATCH, MAX_NODES))

acc <- c(queue = 0, expand = 0, score = 0, closed = 0, grow = 0, rest = 0)
tot <- 0; nodes <- 0; iters <- 0; nsolved <- 0L; created <- 0

for (k in seq_len(CUBES)) {
  r <- astar_timed(net, cubes[[k]], WEIGHT, ASTAR_BATCH, MAX_NODES, BATCH_SIZE)
  acc <- acc + r$t; tot <- tot + r$total; nodes <- nodes + r$nodes
  iters <- iters + r$iters; created <- created + r$n_used
  if (r$solved) nsolved <- nsolved + 1L
  cat(sprintf("  cube %d: %s in %.1f s | %d nodes, %d iters, %d in the open list\n",
              k, if (r$solved) sprintf("%d moves", r$len) else "unsolved",
              r$total, r$nodes, r$iters, r$open_end))
  flush(stdout())
}

cat(sprintf("\n  %d/%d solved, %d nodes expanded, %.0f nodes created\n\n",
            nsolved, CUBES, nodes, created))

cat("== seconds by step =======================================\n\n")
cat(sprintf("%10s %10s %8s %14s\n", "step", "seconds", "share", "per node ms"))
for (k in names(acc))
  cat(sprintf("%10s %10.2f %7.0f%% %14.2f\n", k, acc[[k]],
              100 * acc[[k]] / tot, 1000 * acc[[k]] / max(nodes, 1)))
cat(sprintf("%10s %10.2f %7.0f%% %14.2f\n", "total", tot, 100,
            1000 * tot / max(nodes, 1)))

if (acc[["rest"]] > 0.15 * tot)
  cat("\n  A sixth or more is outside the five steps: the breakdown is missing\n  something, and the largest named step is not safely the target.\n")

cat("\n== what this decides =====================================\n\n")
named <- acc[c("queue", "expand", "score", "closed", "grow")]
top <- names(which.max(named))
cat(sprintf("  Largest step: %s, at %.0f%% of the search.\n", top,
            100 * max(named) / tot))
msg <- switch(top,
  queue  = "  Fix the queue: partial selection, and a live list that is not\n  rebuilt from `open` every iteration.",
  closed = "  Fix the closed-list loop: preallocate add_ix instead of growing it\n  with c(), and take the lookups out of the per-child loop.",
  grow   = "  Fix the growth: preallocate the arrays rather than doubling them\n  with rbind, which copies the state matrix each time.",
  score  = "  Nothing in the queue is worth touching -- the search is spending its\n  time in the network, which is what it is supposed to do.",
  expand = "  The C++ move application dominates, which would be surprising and\n  worth checking before optimising anything in R.")
cat(msg, "\n")
