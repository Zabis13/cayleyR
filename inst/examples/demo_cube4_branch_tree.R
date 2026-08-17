#!/usr/bin/env Rscript
# Branch where the model sees a big win, re-solve, and do it again on the result.
#
# One level of this was measured first: scan a 314-move reduction solve for
# positions where some move beats the algorithm's own by more than five, take
# each, re-solve from there, and compare whole paths. Sixteen positions cleared
# the bar and five of them shortened the solve, the best by 18 moves.
#
# But a re-solved path is just another path, with its own positions worth
# branching at -- so the same scan applies to it, and to whatever that yields.
# That is what this does: keep the best path found so far, scan it, re-solve at
# every branch point, and if anything came out shorter, start over on the best of
# them. It stops when a whole level produces nothing better.
#
# Two things worth knowing before reading the output:
#
#   The gain does not rank the branches. Correlation between the model's gain and
#   the moves actually saved came out at -0.24 over one run, and the single
#   largest gain seen (9.91) produced the WORST branch of all (+144 moves). The
#   threshold decides where to look; only the re-solve says what was found. Every
#   branch point therefore gets re-solved, and they are judged on length alone.
#
#   Solved means solved by COLOUR. A 4x4x4 has four indistinguishable centres per
#   face, so a finished cube need not have its sticker numbers back in order --
#   comparing against 1:96 calls a solved cube unsolved.
#
# Run with:  Rscript inst/examples/demo_cube4_branch_tree.R

suppressMessages(library(cayleyR))

# ---- parameters ------------------------------------------------------------

SCRAMBLE  <- 600L   # quarter turns walked away from solved
THRESHOLD <- 5.0    # branch where a move beats the algorithm's by this much
MAX_LEVEL <- 6L     # how many times to re-scan the improved path
MAX_BRANCH <- 20L   # re-solve at most this many branch points per level
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

# Every position of a path where some move beats the path's own next move by
# more than THRESHOLD, best move first.
branch_points <- function(state, path) {
  n <- length(path)
  states <- vector("list", n + 1L)
  s <- state; states[[1L]] <- s
  for (i in seq_len(n)) { s <- s[mv[[path[i]]]]; states[[i + 1L]] <- s }

  q <- score(do.call(rbind, states))
  out <- list()
  for (p in seq_len(n)) {
    cur  <- states[[p]]
    nxt  <- t(vapply(mn, function(m) as.integer(cur[mv[[m]]]), integer(96)))
    qn   <- score(nxt)
    gains <- (q[p] - qn) - (q[p] - q[p + 1L])
    for (j in which(gains > THRESHOLD))
      out[[length(out) + 1L]] <- list(pos = p, move = mn[j], gain = gains[j],
                                      state = as.integer(nxt[j, ]))
  }
  out[order(-vapply(out, `[[`, numeric(1), "gain"))]
}

for (cube in seq_len(N_CUBES)) {
  state <- generate_state(group = g, n_moves = SCRAMBLE)
  res   <- cube_solve4(state)
  if (!isTRUE(res$found)) { cat("cube", cube, "not solved\n"); next }

  best_path <- res$path
  cat(sprintf("cube %d: start %d moves\n", cube, length(best_path)))

  for (level in seq_len(MAX_LEVEL)) {
    t0 <- proc.time()[["elapsed"]]
    cand <- branch_points(state, best_path)
    if (!length(cand)) {
      cat(sprintf("level %d: no branch point over %.1f -- done\n",
                  level, THRESHOLD))
      break
    }

    cand <- cand[seq_len(min(MAX_BRANCH, length(cand)))]
    improved <- NULL; best_len <- length(best_path)
    for (cd in cand) {
      r2 <- try(cube_solve4(cd$state), silent = TRUE)
      if (inherits(r2, "try-error") || !isTRUE(r2$found)) next
      total <- (cd$pos - 1L) + 1L + length(r2$path)
      if (total < best_len) {
        best_len <- total
        improved <- c(best_path[seq_len(cd$pos - 1L)], cd$move, r2$path)
      }
    }

    el <- proc.time()[["elapsed"]] - t0
    if (is.null(improved)) {
      cat(sprintf("level %d: %d branch points, none shorter (%.0fs) -- done\n",
                  level, length(cand), el))
      break
    }
    cat(sprintf("level %d: %d branch points -> %d moves (%+d)  %.0fs\n",
                level, length(cand), best_len, best_len - length(best_path), el))
    best_path <- improved
    flush.console()
  }

  end <- replay(state, best_path)
  cat(sprintf("\nfinal: %d moves (from %d, %+d)   solved by colour: %s\n",
              length(best_path), length(res$path),
              length(best_path) - length(res$path),
              cube_is_colour_solved(end)))
}
