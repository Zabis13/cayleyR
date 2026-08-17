#!/usr/bin/env Rscript
# Along a reduction solve, is there ever a move much better than the one taken?
#
# cube_solve4() writes its path from geometry, without consulting any model. At
# each state on that path the model can score all 24 moves and say what the best
# of them is worth. Two numbers per position: what the algorithm's own next move
# gains, and what the best available move gains. Where the gap is large, the
# solve walked past something.
#
# Measured once over fifty positions of a 314-move solve: a better move existed
# at 47 of them, at 33 by more than a whole move, and the algorithm's own move
# was often NEGATIVE -- it moves away from the solved cube to set up the stage
# that follows. The gaps are widest through the CFOP tail and close to nothing
# in the last twenty moves, where the solve is already choosing the best move
# available.
#
# This only measures. Taking the better move would leave the plan: cube_solve4
# is not a search, it executes a sequence that assumes the centres stay built
# and the edges stay paired, and a move that improves the score may undo either.
# Acting on what this finds means re-solving from the new state, not splicing
# the move into the old path.
#
# Run with:  Rscript inst/examples/demo_cube4_scout_moves.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE <- 600L   # quarter turns walked away from solved
SAMPLE   <- 50L    # positions probed along the path
N_CUBES  <- 1L

ARCHIVE  <- "/mnt/Data2/DS_projects/444/archive"
SEED     <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)
set.seed(SEED)

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N); names(mv) <- cube_move_names(N)
mn <- names(mv)
solved <- seq_len(96)

score <- function(M) {
  if (!is.matrix(M)) M <- matrix(as.integer(M), 1L)
  storage.mode(M) <- "integer"
  cayley_distance("cube4_model")(M, solved, 4L)
}

for (cube in seq_len(N_CUBES)) {
  state <- generate_state(group = g, n_moves = SCRAMBLE)
  res   <- cube_solve4(state)
  cat(sprintf("cube %d: path %d moves, solved %s\n",
              cube, length(res$path), isTRUE(res$found)))
  if (!isTRUE(res$found)) next

  bounds <- cumsum(res$stages$n_moves)
  stage_at <- function(i)
    res$stages$name[pmin(findInterval(i - 1L, c(0L, bounds)),
                         nrow(res$stages))]

  # Every state the path passes through.
  s <- state
  states <- vector("list", length(res$path) + 1L)
  states[[1L]] <- s
  for (i in seq_along(res$path)) {
    s <- s[mv[[res$path[i]]]]
    states[[i + 1L]] <- s
  }

  pos <- unique(round(seq(1, length(res$path), length.out = SAMPLE)))

  cat(sprintf("\n%6s %8s %9s %9s %8s  %-9s %s\n",
              "pos", "q", "algo", "best", "gap", "stage", "move"))
  gaps <- numeric(0); stages <- character(0)
  for (p in pos) {
    cur <- states[[p]]
    q0  <- score(cur)
    nxt <- t(vapply(mn, function(m) as.integer(cur[mv[[m]]]), integer(96)))
    qn  <- score(nxt)
    algo <- q0 - score(states[[p + 1L]])   # what the solve's own move gains
    best <- q0 - min(qn)                   # what the best move would gain
    gaps <- c(gaps, best - algo); stages <- c(stages, stage_at(p))
    cat(sprintf("%6d %8.2f %9.2f %9.2f %8.2f  %-9s %s\n",
                p - 1L, q0, algo, best, best - algo, stage_at(p),
                mn[which.min(qn)]))
    flush.console()
  }

  cat(sprintf("\ngap: mean %.2f  median %.2f  max %.2f\n",
              mean(gaps), stats::median(gaps), max(gaps)))
  cat(sprintf("positions where a better move existed: %d/%d (by >1: %d)\n",
              sum(gaps > 0.01), length(gaps), sum(gaps > 1)))
  cat("\nby stage:\n")
  print(round(tapply(gaps, stages, mean), 2))
}
