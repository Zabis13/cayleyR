#!/usr/bin/env Rscript
# Branch only where the model sees a big win, and re-solve from there.
#
# The rule is narrow on purpose: at a position where some move beats the one the
# algorithm plays by more than THRESHOLD, take that move and solve the cube again
# from where it lands. The result is a whole new path -- prefix up to the branch,
# the move, then a fresh solve -- and it is compared against the original by
# total length, not by what was left to play.
#
# Every position is scanned, not a sample, so nothing that clears the threshold
# is missed. Positions that clear it are rare: over one 314-move solve, a gain
# above 5 turned up twice, above 3 six times, above 2 a dozen times. The scan
# reports all of them and re-solves each, so the threshold can be judged from
# what it actually yields rather than picked in advance.
#
# What was already measured, and why this is not that: re-solving from the MIDDLE
# of a solve mostly hurts -- eight of nine probes came out longer, by +48 moves
# on average, because a move that improves the model's score often undoes the
# centres or the edge pairs the plan depends on. This asks the different question
# of whether the resulting whole path is shorter, which is the only comparison
# that decides anything.
#
# Run with:  Rscript inst/examples/demo_cube4_branch5.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE  <- 600L   # quarter turns walked away from solved
THRESHOLD <- 5.0    # branch where a move beats the algorithm's by this much
SCAN_STEP <- 1L     # scan every Nth position (1 = all of them)
N_CUBES   <- 1L

ARCHIVE   <- "/mnt/Data2/DS_projects/444/archive"
SEED      <- 2026L

# ---- run -------------------------------------------------------------------

Sys.setenv(CUBE4_ARCHIVE = ARCHIVE)
set.seed(SEED)

N  <- 4L
g  <- cube_group(N)
mv <- cube_moves(N); names(mv) <- cube_move_names(N)
mn <- names(mv)
solved <- seq_len(96)

CHUNK <- 500L
score <- function(M) {
  if (!is.matrix(M)) M <- matrix(as.integer(M), 1L)
  storage.mode(M) <- "integer"
  n <- nrow(M)
  if (n <= CHUNK) return(cayley_distance("cube4_model")(M, solved, 4L))
  out <- numeric(n)
  for (a in seq(1L, n, by = CHUNK)) {
    b <- min(a + CHUNK - 1L, n)
    out[a:b] <- cayley_distance("cube4_model")(M[a:b, , drop = FALSE], solved, 4L)
  }
  out
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

for (cube in seq_len(N_CUBES)) {
  state <- generate_state(group = g, n_moves = SCRAMBLE)
  res   <- cube_solve4(state)
  n     <- length(res$path)
  cat(sprintf("cube %d: original path %d moves\n", cube, n))
  if (!isTRUE(res$found)) next

  bounds <- cumsum(res$stages$n_moves)
  stage_at <- function(i)
    res$stages$name[pmin(findInterval(i - 1L, c(0L, bounds)),
                         nrow(res$stages))]

  states <- vector("list", n + 1L)
  s <- state; states[[1L]] <- s
  for (i in seq_along(res$path)) { s <- s[mv[[res$path[i]]]]; states[[i + 1L]] <- s }

  # ---- scan for positions worth branching at ------------------------------

  cat("scanning for gains over", THRESHOLD, "...\n"); flush.console()
  pos <- seq(1L, n, by = SCAN_STEP)
  q_all <- score(do.call(rbind, states[pos]))
  q_nxt <- score(do.call(rbind, states[pos + 1L]))

  cand <- list()
  t0 <- proc.time()[["elapsed"]]
  for (i in seq_along(pos)) {
    p    <- pos[i]
    cur  <- states[[p]]
    nxt  <- t(vapply(mn, function(m) as.integer(cur[mv[[m]]]), integer(96)))
    qn   <- score(nxt)
    algo <- q_all[i] - q_nxt[i]
    gains <- (q_all[i] - qn) - algo
    hit <- which(gains > THRESHOLD)
    for (j in hit)
      cand[[length(cand) + 1L]] <- list(pos = p, move = mn[j],
                                        gain = gains[j], q = q_all[i],
                                        stage = stage_at(p))
  }
  cat(sprintf("scanned %d positions in %.0fs -- %d branch points\n",
              length(pos), proc.time()[["elapsed"]] - t0, length(cand)))

  if (!length(cand)) {
    cat("nothing cleared the threshold; lower it to see anything\n")
    next
  }

  # ---- re-solve from each branch, compare whole paths ---------------------

  cat(sprintf("\n%6s %6s %7s %8s %9s %8s  %-9s %s\n",
              "pos", "move", "gain", "prefix", "re-solve", "total", "stage",
              "vs original"))
  best <- list(len = n, path = res$path, at = NA_integer_)
  for (cd in cand) {
    mid <- as.integer(states[[cd$pos]][mv[[cd$move]]])
    r2  <- try(cube_solve4(mid), silent = TRUE)
    if (inherits(r2, "try-error") || !isTRUE(r2$found)) {
      cat(sprintf("%6d %6s %7.2f %8d %9s %8s  %-9s %s\n",
                  cd$pos - 1L, cd$move, cd$gain, cd$pos - 1L, "-", "-",
                  cd$stage, "re-solve failed"))
      next
    }
    prefix <- cd$pos - 1L
    total  <- prefix + 1L + length(r2$path)
    cat(sprintf("%6d %6s %7.2f %8d %9d %8d  %-9s %+d\n",
                cd$pos - 1L, cd$move, cd$gain, prefix, length(r2$path), total,
                cd$stage, total - n))
    flush.console()
    if (total < best$len)
      best <- list(len = total, at = cd$pos - 1L,
                   path = c(res$path[seq_len(prefix)], cd$move, r2$path))
  }

  cat(sprintf("\noriginal %d moves; best branch %d moves", n, best$len))
  if (!is.na(best$at)) cat(sprintf(" (branched at %d)", best$at)) else
    cat(" (no branch beat it)")
  cat("\n")
  ok <- identical(as.integer(replay(state, best$path)), solved)
  cat(sprintf("verified: %s\n", ok))
}
