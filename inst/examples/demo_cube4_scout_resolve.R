#!/usr/bin/env Rscript
# When the model sees a better move, is it a shortcut or a broken plan?
#
# demo_cube4_scout_moves.R found that a better-scoring move exists at nearly
# every position of a reduction solve. That does not make it a better move: a
# solve is not a search, and cube_solve4() executes a plan that assumes the
# centres stay built and the edges stay paired. A move that improves the model's
# score may well undo one of those, and the algorithm would then have to build
# it again.
#
# Whether it does is measurable without any theory about invariants. Take the
# move, re-solve from where it lands, and compare:
#
#   remaining   what the original solve still had to play from this position
#   after       one move, plus a fresh solve from the state it reaches
#
# If `after` is shorter, the move was a shortcut the plan walked past. If it is
# longer, the move broke something the plan was relying on, and the cost of
# repairing it exceeds what the better score was worth. The q gap says nothing
# about which of the two happened -- this does.
#
# It is slow: every probed position pays for a whole extra solve. That is the
# price of the answer, and it is the same price the "replace and re-solve"
# strategy would pay at every step, so the timing here is the strategy's own
# cost measured in advance.
#
# Run with:  Rscript inst/examples/demo_cube4_scout_resolve.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE <- 600L   # quarter turns walked away from solved
SAMPLE   <- 14L    # positions probed -- each costs a full re-solve
MIN_GAP  <- 1.0    # only probe where the better move beats the algorithm's own
                   # by this much; below it there is nothing to explain
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

  cat(sprintf("\n%6s %7s %7s %10s %8s %8s  %-9s %s\n",
              "pos", "q", "gap", "remaining", "after", "delta", "stage", "verdict"))
  deltas <- numeric(0); verdicts <- character(0); stages <- character(0)
  gaps_kept <- numeric(0)

  for (p in pos) {
    cur <- states[[p]]
    q0  <- score(cur)
    nxt <- t(vapply(mn, function(m) as.integer(cur[mv[[m]]]), integer(96)))
    qn  <- score(nxt)
    algo <- q0 - score(states[[p + 1L]])
    best <- q0 - min(qn)
    gap  <- best - algo
    if (gap < MIN_GAP) next

    j    <- which.min(qn)
    take <- as.integer(nxt[j, ])
    rem  <- n - (p - 1L)                       # moves the plan still had to play

    r2 <- try(cube_solve4(take), silent = TRUE)
    if (inherits(r2, "try-error") || !isTRUE(r2$found)) {
      cat(sprintf("%6d %7.2f %7.2f %10d %8s %8s  %-9s %s\n",
                  p - 1L, q0, gap, rem, "-", "-", stage_at(p),
                  "re-solve FAILED"))
      verdicts <- c(verdicts, "failed")
      next
    }

    after <- 1L + length(r2$path)
    d     <- after - rem
    v     <- if (d < 0) "shortcut" else if (d == 0) "even" else "broke plan"
    cat(sprintf("%6d %7.2f %7.2f %10d %8d %+8d  %-9s %s (%s)\n",
                p - 1L, q0, gap, rem, after, d, stage_at(p), v, mn[j]))
    flush.console()
    deltas    <- c(deltas, d)
    verdicts  <- c(verdicts, v)
    stages    <- c(stages, stage_at(p))
    gaps_kept <- c(gaps_kept, gap)
  }

  if (length(deltas)) {
    cat(sprintf("\nprobed %d positions with gap >= %.1f\n",
                length(verdicts), MIN_GAP))
    print(table(verdicts))
    cat(sprintf("\ndelta moves: mean %+.1f  median %+.1f  best %+d  worst %+d\n",
                mean(deltas), stats::median(deltas), min(deltas), max(deltas)))

    # Per stage, because the count of good moves already splits sharply by stage
    # (reduction 4.8 per position, parity 3.0, CFOP 1.4) and the cost of taking
    # one may split the same way. A mean over all positions would hide a stage
    # that works inside a stage that does not.
    ok_stage <- stages
    cat("\nby stage:\n")
    print(data.frame(
      n      = as.vector(table(ok_stage)),
      mean   = round(as.vector(tapply(deltas, ok_stage, mean)), 1),
      median = round(as.vector(tapply(deltas, ok_stage, stats::median)), 1),
      worse  = as.vector(tapply(deltas, ok_stage, function(x) sum(x > 0))),
      row.names = names(table(ok_stage))))

    # Does a bigger q gap buy a bigger saving? If it does, the q gap is a fair
    # rule for choosing where to branch. If it does not, branching has to be
    # decided on the re-solve itself, which costs a solve per candidate.
    ok_gap <- gaps_kept
    if (length(ok_gap) >= 4L) {
      r <- stats::cor(ok_gap, deltas)
      cat(sprintf("\ncorrelation between q gap and delta: %+.2f\n", r))
      cat(if (r < -0.3)
        "  a bigger gap does buy a bigger saving -- q is a usable rule\n"
      else if (r > -0.1)
        "  the gap does not predict the saving -- branch on the re-solve, not on q\n"
      else "  weak relation; q is a rough guide at best\n")
    }
    cat(sprintf("\npositions where re-solving helped: %d/%d\n",
                sum(deltas < 0), length(deltas)))
  }
}
