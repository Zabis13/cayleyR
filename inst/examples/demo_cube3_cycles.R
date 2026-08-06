#!/usr/bin/env Rscript
# Cycles on the 3x3x3 cube: repeat a word until the cube comes back.
#
# Every word is a permutation, and every permutation has finite order -- repeat
# it enough times and the cube returns to where it started. The order is the
# length of that cycle, and it is a property of the word alone, so the same
# number comes back whatever state you start from.
#
# The point of the cycles is that they are cheap structure in a graph far too
# big to enumerate: 4.3e19 states, but the orbit of a word is walkable. The
# package's own cycle_shortcut() uses exactly this to find rejoin points that
# a depth-limited BFS cannot reach.
#
# Run with:  Rscript inst/examples/demo_cube3_cycles.R

library(cayleyR)

N <- 3L
g <- cube_group(N)
id <- cube_identity(N)

hr <- function(title) cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")

# Walk the orbit of a word by hand: apply it over and over, and stop when the
# state repeats. group_order() answers the same question in C++; doing it in R
# here shows what the number means.
orbit <- function(group, state, word, max_reps = 2000L) {
  seen <- state
  cur <- state
  for (i in seq_len(max_reps)) {
    cur <- group_apply(group, cur, word)
    if (identical(cur, state)) return(i)
  }
  NA_integer_
}

# ---------------------------------------------------------------- orders

hr("the order of a word")

words <- list(
  "R"                 = c("R"),
  "R R"               = c("R", "R"),            # the half turn, as a word
  "R U"               = c("R", "U"),
  "R U'"              = c("R", "U'"),
  "R U R' U'"         = c("R", "U", "R'", "U'"),   # the sexy move
  "M"                 = c("M"),
  "R M"               = c("R", "M"),
  "R U R' U' M"       = c("R", "U", "R'", "U'", "M"),
  "R L' U"            = c("R", "L'", "U")
)

cat(sprintf("  %-14s %8s %8s\n", "word", "order", "by hand"))
for (nm in names(words)) {
  w <- words[[nm]]
  cat(sprintf("  %-14s %8d %8d\n", nm, group_order(g, w), orbit(g, id, w)))
}

# ---------------------------------------------------------------- the orbit itself

hr("what the orbit looks like")

w <- c("R", "U", "R'", "U'")
ord <- group_order(g, w)
cat("word              :", paste(w, collapse = " "), "\n")
cat("order             :", ord, "\n\n")

cur <- id
for (i in seq_len(ord)) {
  cur <- group_apply(g, cur, w)
  cat(sprintf("  after %d x (%s) : %2d stickers off, solved %s\n",
              i, paste(w, collapse = " "), sum(cur != id),
              if (identical(cur, id)) "YES" else "no"))
}

# ---------------------------------------------------------------- order is not the state

hr("the order does not depend on where you start")

set.seed(1)
starts <- list(
  "solved"    = id,
  "scramble1" = generate_state(group = g, n_moves = 12L),
  "scramble2" = generate_state(group = g, n_moves = 30L)
)
w <- c("R", "U")
for (nm in names(starts)) {
  cat(sprintf("  from %-10s the orbit of R U closes after %3d repeats\n",
              nm, orbit(g, starts[[nm]], w)))
}

# ---------------------------------------------------------------- a long one

hr("a word with a long orbit")

# R U has order 105 -- the cube passes through 105 distinct states before it
# comes home, and the identity appears only at the very end
w <- c("R", "U")
ord <- group_order(g, w)
cur <- id
returns <- integer(0)
for (i in seq_len(ord)) {
  cur <- group_apply(g, cur, w)
  if (identical(cur, id)) returns <- c(returns, i)
}
cat("word              :", paste(w, collapse = " "), "\n")
cat("order             :", ord, "\n")
cat("returns to solved : after", paste(returns, collapse = ", "), "repeats\n")

# ---------------------------------------------------------------- orders across the alphabet

hr("orders of every single move")

ords <- vapply(group_moves(g), function(m) group_order(g, m), integer(1))
cat("  every move has order 4:", all(ords == 4L), "\n")

hr("orders of every two-move word")

pairs <- expand.grid(a = group_moves(g), b = group_moves(g),
                     stringsAsFactors = FALSE)
pairs$order <- vapply(seq_len(nrow(pairs)),
                      function(i) group_order(g, c(pairs$a[i], pairs$b[i])),
                      integer(1))
cat("  ", nrow(pairs), " two-move words\n", sep = "")
cat("  distinct orders :", paste(sort(unique(pairs$order)), collapse = " "), "\n")
cat("  longest orbit   :", max(pairs$order), "\n")

top <- pairs[order(-pairs$order), ][seq_len(6), ]
cat("\n  the longest ones:\n")
for (i in seq_len(nrow(top))) {
  cat(sprintf("    %-8s order %4d\n",
              paste(top$a[i], top$b[i]), top$order[i]))
}

cat("\n")
