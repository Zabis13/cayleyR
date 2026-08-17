#!/usr/bin/env Rscript
# How many good moves does a position offer, not just the best one?
#
# Branching only makes sense if a position has more than one move worth taking.
# The earlier scout reported the best move at each position and found a better
# one almost everywhere -- but "better than the algorithm's" is a low bar, and it
# said nothing about how many moves clear it. A search that branches on the top
# move alone is a chain, not a tree.
#
# So this counts, at each probed position, how many of the twenty-four moves beat
# the algorithm's own by various margins, and reports the spread of the best few.
# The thresholds matter: a gain over 5 turned up once in fifty positions in the
# earlier run, so branching at 5 would almost never branch. What follows says
# where the real cut lies.
#
# Read the columns as: how many moves gain more than 1, more than 2, more than 3,
# and what the top three gains actually are.
#
# Run with:  Rscript inst/examples/demo_cube4_scout_branches.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE <- 600L
SAMPLE   <- 40L    # positions probed along the path
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
  n     <- length(res$path)
  cat(sprintf("cube %d: path %d moves, solved %s\n", cube, n, isTRUE(res$found)))
  if (!isTRUE(res$found)) next

  bounds <- cumsum(res$stages$n_moves)
  stage_at <- function(i)
    res$stages$name[pmin(findInterval(i - 1L, c(0L, bounds)),
                         nrow(res$stages))]

  s <- state
  states <- vector("list", n + 1L)
  states[[1L]] <- s
  for (i in seq_along(res$path)) {
    s <- s[mv[[res$path[i]]]]
    states[[i + 1L]] <- s
  }

  pos <- unique(round(seq(1, n, length.out = SAMPLE)))

  cat(sprintf("\n%6s %7s %7s %5s %5s %5s  %-9s %s\n",
              "pos", "q", "algo", ">1", ">2", ">3", "stage", "top three gains"))
  n1 <- integer(0); n2 <- integer(0); n3 <- integer(0); stg <- character(0)

  for (p in pos) {
    cur  <- states[[p]]
    q0   <- score(cur)
    nxt  <- t(vapply(mn, function(m) as.integer(cur[mv[[m]]]), integer(96)))
    qn   <- score(nxt)
    algo <- q0 - score(states[[p + 1L]])

    # Each move's gain over what the algorithm itself achieves here.
    gains <- (q0 - qn) - algo
    top   <- sort(gains, decreasing = TRUE)[1:3]

    c1 <- sum(gains > 1); c2 <- sum(gains > 2); c3 <- sum(gains > 3)
    n1 <- c(n1, c1); n2 <- c(n2, c2); n3 <- c(n3, c3)
    stg <- c(stg, stage_at(p))

    cat(sprintf("%6d %7.2f %7.2f %5d %5d %5d  %-9s %s\n",
                p - 1L, q0, algo, c1, c2, c3, stage_at(p),
                paste(sprintf("%+.2f", top), collapse = " ")))
    flush.console()
  }

  cat(sprintf("\nmoves per position beating the algorithm by more than:\n"))
  cat(sprintf("  1 move : mean %.1f   positions with at least one: %d/%d\n",
              mean(n1), sum(n1 > 0), length(n1)))
  cat(sprintf("  2 moves: mean %.1f   positions with at least one: %d/%d\n",
              mean(n2), sum(n2 > 0), length(n2)))
  cat(sprintf("  3 moves: mean %.1f   positions with at least one: %d/%d\n",
              mean(n3), sum(n3 > 0), length(n3)))
  cat(sprintf("\npositions offering 2+ moves over 1: %d/%d  -- this is what a\n",
              sum(n1 >= 2), length(n1)))
  cat("tree would branch on; where it is 1 or 0 the search is a chain.\n")
  cat("\nmean count (>1) by stage:\n")
  print(round(tapply(n1, stg, mean), 1))
}
