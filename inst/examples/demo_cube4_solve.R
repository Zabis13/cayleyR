#!/usr/bin/env Rscript
# Solving the 4x4x4 by reduction, stage by stage.
#
# An even cube cannot be solved the way a 3x3x3 is. Its centres are four pieces
# that move relative to each other, its edges are two halves apiece, and until
# both are gathered there is no "the U face" to speak of. Reduction is the
# standard answer: build the centres, pair the edges, and what is left behaves
# as a 3x3x3 with fat pieces, which the solver already in this package
# finishes.
#
#   centres   Pochmann's layer-by-layer: build one, put it on the left, fill
#             the layer beside it, then shoot the rest down from the top
#   edges     slice-flip-slice: a slice brings two halves together, a face
#             turn flips one, the slice goes back
#   parity    two last-layer states a 3x3x3 never reaches, so its tables have
#             no entry for them -- repaired with inner-layer algorithms
#   3x3x3     CFOP on the squeezed cube, its solution lifted back
#
# Move counts are quarter turns, the metric the whole package uses. Expect
# around 300 -- reduction is a method a person can hold in their head, not a
# short solution. A search-based solver reaches the same cube in a fraction of
# the moves and a great deal more time.
#
# Run with:  Rscript inst/examples/demo_cube4_solve.R

library(cayleyR)

N        <- 4L
n_states <- 5L
n_moves  <- 60L       # quarter turns walked away from the solved cube

set.seed(2026)

g  <- cube_group(N)
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

# Replay a path move by move, independently of the solver that produced it.
# A solver checked with its own arithmetic proves nothing.
replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

hr("one solve, in detail")

state <- generate_state(group = g, n_moves = n_moves)
res   <- cube_solve4(state)

cat("scramble      : ", n_moves, " quarter turns\n", sep = "")
cat("solved        : ", res$found, "\n", sep = "")
cat("total moves   : ", length(res$path), "\n\n", sep = "")

print(res$stages)

cat("\nthe first twenty moves:\n  ",
    paste(head(res$path, 20), collapse = " "), " ...\n", sep = "")

hr("what each stage leaves behind")

# Read off the state, not taken on trust -- and read as COLOUR, not as face
# number. cube_centre_counts() compares a piece against the face it started on,
# which stops meaning anything once the solve has turned the cube; a face whose
# four centre pieces all match each other is built wherever it now sits.
faces_built <- function(s) {
  sum(vapply(0:5, function(f) {
    idx <- f * 16L + c(5L, 6L, 9L, 10L) + 1L
    length(unique((s[idx] - 1L) %/% 16L)) == 1L
  }, logical(1)))
}

pairs_made <- function(s) {
  p <- cube_pieces(4)
  e <- p[p$n_stickers == 2L, ]
  st <- lapply(strsplit(e$stickers, ","), as.integer)
  key <- vapply(st, function(i) paste(sort((i - 1L) %/% 16L), collapse = "-"),
                character(1))
  sum(vapply(split(st, key), function(g2) {
    a <- sort((s[g2[[1]]] - 1L) %/% 16L)
    b <- sort((s[g2[[2]]] - 1L) %/% 16L)
    identical(a, b)
  }, logical(1)))
}

for (i in seq_len(nrow(res$stages))) {
  s <- res$states[[i]]
  cat(sprintf("  %-10s %-20s faces built: %d of 6   edges paired: %2d of 12\n",
              res$stages$name[i], res$stages$detail[i],
              faces_built(s), pairs_made(s)))
}

hr(paste(n_states, "states"))

cat(sprintf("%5s  %8s  %7s  %s\n", "state", "moves", "solved", "stages"))
lengths <- integer(0)
for (i in seq_len(n_states)) {
  s <- generate_state(group = g, n_moves = n_moves)
  r <- cube_solve4(s)

  # verified by replaying the path, not by the solver's own flag
  final <- replay(s, r$path)
  ok <- isTRUE(r$found) && cube_is_colour_solved(final)

  lengths <- c(lengths, length(r$path))
  cat(sprintf("%5d  %8d  %7s  %s\n", i, length(r$path), ok,
              paste(r$stages$name, collapse = " + ")))
}

cat("\nmedian ", median(lengths), " moves, range ",
    min(lengths), "-", max(lengths), "\n", sep = "")

hr("parity, which is what makes an even cube different")

# Reduction can leave the last layer in a state no 3x3x3 reaches. The 3x3x3
# solver knows all 57 OLL and 21 PLL cases, so its refusal IS the detection --
# there is no separate detector here.
seen <- character(0)
for (i in 1:20) {
  s <- generate_state(group = g, n_moves = n_moves)
  r <- cube_solve4(s)
  seen <- c(seen, r$stages$detail[r$stages$name == "parity"])
}
cat("over twenty solves, parity repairs applied:\n")
print(table(factor(seen, levels = c("OLL", "PLL"))))
cat("\nneither is a defect of the solve -- an odd number of inner-layer turns\n",
    "during reduction is enough to produce one, and it has to be undone with\n",
    "an inner-layer algorithm, which a 3x3x3 has no way to express.\n", sep = "")
