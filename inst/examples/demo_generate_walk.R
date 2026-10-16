#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_walk(), one mode at a time, on the 3x3x3.
#
#   1. exact = TRUE     every walk n_moves long; solution undoes it
#   2. exact = FALSE    lengths drawn uniformly from 1 to n_moves
#   3. no_undo          refuses a move that undoes the one before it
#   4. no_cancel        refuses a move that cancels through commuting moves
#                       (U D U', U U U), which no_undo lets through
#   5. pivot = TRUE     a point inside the walk and the two moves meeting there
#
# The cancellation count in 3 and 4 is computed here in R, from the
# permutations, independently of the C++ that generates the walks.
#
# Usage:  Rscript inst/examples/demo_generate_walk.R [n] [seed]
#   n     walks per count (default 2000)
#   seed  default 1
# ---------------------------------------------------------------------------

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)
N    <- if (length(args) >= 1) as.integer(args[1]) else 2000L
SEED <- if (length(args) >= 2) as.integer(args[2]) else 1L

g   <- cube_group(3)
mv  <- g$moves
inv <- g$inverse_of
n_a <- length(mv)
id  <- group_identity(g)

# Which moves commute, and the order of each, read off the permutations.
after <- function(word) group_apply(g, id, word)
comm  <- outer(seq_len(n_a), seq_len(n_a), Vectorize(function(a, b)
  identical(after(mv[c(a, b)]), after(mv[c(b, a)]))))
ord <- vapply(seq_len(n_a), function(a) {
  s <- id
  for (k in 1:100) {
    s <- group_apply(g, s, mv[a])
    if (identical(s, id)) return(k)
  }
  0L
}, integer(1))

# A word is reducible if some move cancels against the tail behind it through
# moves it commutes with, or repeats often enough to be shorter the other way.
reducible <- function(word) {
  idx <- match(word, mv)
  for (j in seq_along(idx)) {
    a <- idx[j]; cnt <- 0L; k <- j - 1L
    while (k >= 1L) {
      b <- idx[k]
      if (b == inv[a]) return(TRUE)
      if (!comm[a, b]) break
      if (b == a) cnt <- cnt + 1L
      k <- k - 1L
    }
    if (ord[a] > 0L && 2L * (cnt + 1L) > ord[a]) return(TRUE)
  }
  FALSE
}

immediate_undo <- function(word) {
  idx <- match(word, mv)
  length(idx) > 1L && any(idx[-1L] == inv[idx[-length(idx)]])
}

words <- function(w) vapply(w$moves, paste, character(1), collapse = " ")
pct   <- function(x) sprintf("%5.1f%%", 100 * mean(x))

cat(sprintf("generate_walk on cube3x3x3 -- %d moves, seed %d, %d walks per count\n\n",
            n_a, SEED, N))

# ---------------------------------------------------------------------------
cat("== 1. exact = TRUE (default) ==========================================\n")
set.seed(SEED)
w <- generate_walk(g, n = 3, n_moves = 8)
for (i in 1:3) {
  ok <- identical(group_apply(g, w$states[i, ], w$solution[[i]]), id)
  cat(sprintf("  walk     : %s\n  solution : %s   (%s)\n\n",
              words(w)[i], paste(w$solution[[i]], collapse = " "),
              if (ok) "solves" else "DOES NOT SOLVE"))
}

# ---------------------------------------------------------------------------
cat("== 2. exact = FALSE: lengths drawn from 1 to n_moves ==================\n")
set.seed(SEED)
w <- generate_walk(g, n = N, n_moves = 10, exact = FALSE)
cat("  length :", sprintf("%5d", 1:10), "\n")
cat("  walks  :", sprintf("%5d", tabulate(w$depth, 10)), "\n\n")

# ---------------------------------------------------------------------------
cat("== 3-4. no_undo / no_cancel: how many walks cancel somewhere ==========\n")
cat(sprintf("  %-28s %14s %12s\n", "", "immediate undo", "reducible"))
modes <- list(
  "no_undo = FALSE"            = list(no_undo = FALSE, no_cancel = FALSE),
  "no_undo = TRUE  (default)"  = list(no_undo = TRUE,  no_cancel = FALSE),
  "no_cancel = TRUE"           = list(no_undo = TRUE,  no_cancel = TRUE))
for (nm in names(modes)) {
  set.seed(SEED)
  w <- generate_walk(g, n = N, n_moves = 12,
                     no_undo = modes[[nm]]$no_undo,
                     no_cancel = modes[[nm]]$no_cancel)
  cat(sprintf("  %-28s %14s %12s\n", nm,
              pct(vapply(w$moves, immediate_undo, logical(1))),
              pct(vapply(w$moves, reducible, logical(1)))))
}
set.seed(SEED)
w <- generate_walk(g, n = N, n_moves = 12)
ex <- head(words(w)[vapply(w$moves, reducible, logical(1))], 3)
cat("\n  reducible walks that no_undo let through:\n")
cat(sprintf("    %s\n", ex), sep = "")
cat("\n")

# ---------------------------------------------------------------------------
cat("== 5. pivot = TRUE: a point inside the walk ===========================\n")
set.seed(SEED)
p <- generate_walk(g, n = 6, n_moves = 10, exact = FALSE, pivot = TRUE,
                   no_cancel = TRUE)
print(data.frame(depth = p$depth, back = p$prev_move, away = p$next_move,
                 walk = words(p)), row.names = FALSE)
cat("\n  depth is where the cut fell; back leads to depth - 1, away to depth + 1\n")

set.seed(SEED)
p <- generate_walk(g, n = N, n_moves = 10, exact = FALSE, pivot = TRUE)
cat("\n  cut depth:", sprintf("%5d", 2:9), "\n")
cat("  walks    :", sprintf("%5d", tabulate(p$depth, 9)[2:9]), "\n")
cat("  (k_min = 2 by default; every depth equally likely)\n")
