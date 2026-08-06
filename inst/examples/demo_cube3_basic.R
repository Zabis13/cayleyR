#!/usr/bin/env Rscript
# The 3x3x3 cube through the group API.
#
# A cube state is an integer vector of length 6n^2 -- 54 stickers for a 3x3x3 --
# and a move permutes it. That is the same shape of object the TopSpin side of
# the package works with, which is the point: one set of functions
# (group_apply, group_order, group_inverse_seq, ...) drives both puzzles.
#
# The alphabet is generated from the geometry rather than tabulated, so the
# same call gives a 2x2x2, a 4x4x4 or anything larger. It is the QUARTER-TURN
# metric: half turns are not moves of their own, so a double turn is the word
# c("R", "R") and "R2" is not a move name.
#
# Run with:  Rscript inst/examples/demo_cube3_basic.R

library(cayleyR)

N <- 3L
g <- cube_group(N)
id <- cube_identity(N)

hr <- function(title) cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")

# ---------------------------------------------------------------- the group

hr("the group")
print(g)
cat("state length      :", length(id), " (6 * ", N, "^2)\n", sep = "")
cat("moves             :", length(group_moves(g)), "\n")
cat("solved state      : 1..", length(id), "\n", sep = "")

cat("\nthe alphabet:\n")
cat("  faces  :", paste(setdiff(group_moves(g), c("M", "M'", "E", "E'", "S", "S'")),
                        collapse = " "), "\n")
cat("  slices :", paste(intersect(group_moves(g), c("M", "M'", "E", "E'", "S", "S'")),
                        collapse = " "), "\n")

# ---------------------------------------------------------------- applying moves

hr("applying moves")

s <- group_apply(g, id, "R")
cat("after R           : ", sum(s != id), " stickers moved\n", sep = "")

# a quarter turn has order 4: four of them come back to where they started
cat("order of R        :", group_order(g, "R"), "\n")
cat("R R R R == solved : ", identical(group_apply(g, id, c("R", "R", "R", "R")), id), "\n", sep = "")

# the inverse undoes it
cat("R then R' solved  : ", identical(group_apply(g, s, "R'"), id), "\n", sep = "")

# half turns are words, not letters
cat("\nhalf turns are words in this metric:\n")
cat("  \"R2\" a move?    : ", "R2" %in% group_moves(g), "\n", sep = "")
cat("  order of R R    : ", group_order(g, c("R", "R")), "\n", sep = "")

# ---------------------------------------------------------------- words

hr("words and their orders")

words <- list(
  "R"            = "R",
  "R U"          = c("R", "U"),
  "R U R' U'"    = c("R", "U", "R'", "U'"),   # the sexy move
  "M"            = "M",
  "R U R' U' M"  = c("R", "U", "R'", "U'", "M")
)

for (nm in names(words)) {
  cat(sprintf("  %-14s order %6d\n", nm, group_order(g, words[[nm]])))
}

# ---------------------------------------------------------------- inverses

hr("a word and its inverse cancel")

w <- c("R", "U", "R'", "U'")
inv <- group_inverse_seq(g, w)
cat("word              :", paste(w, collapse = " "), "\n")
cat("inverse           :", paste(inv, collapse = " "), "\n")
scrambled <- group_apply(g, id, w)
cat("w then inv solved : ", identical(group_apply(g, scrambled, inv), id), "\n", sep = "")

# ---------------------------------------------------------------- scrambling

hr("scrambling")

set.seed(42)
# generate_state walks the group, so the state it returns is always reachable
sc <- generate_state(group = g, n_moves = 25L)
cat("scrambled         : ", sum(sc != id), " of ", length(id),
    " stickers out of place\n", sep = "")
cat("colour-solved     : ", cube_is_colour_solved(sc, N), "\n", sep = "")

# the centres never move on a real cube, and this state is a real one
centres <- c(5, 14, 23, 32, 41, 50)
cat("centres fixed     : ", all(sc[centres] == centres), "\n", sep = "")

# a short scramble can be undone by inverting the word that made it
w <- c("R", "U", "F", "R'")
s2 <- group_apply(g, id, w)
cat("undo by inverse   : ",
    identical(group_apply(g, s2, group_inverse_seq(g, w)), id), "\n", sep = "")

# ---------------------------------------------------------------- writing moves

hr("writing moves")

# a word may be one string or a vector, and an unseparated string is split on
# move names, longest first -- so M' wins over M
cat("group_parse_word(\"RM'R'\")  : ",
    paste(group_moves(g)[group_parse_word(g, "RM'R'")], collapse = " "), "\n", sep = "")
cat("group_parse_word(\"R M' R'\"): ",
    paste(group_moves(g)[group_parse_word(g, "R M' R'")], collapse = " "), "\n", sep = "")

# ---------------------------------------------------------------- other sizes

hr("the same code, other cube sizes")

for (n in 2:5) {
  mv <- cube_move_names(n)
  cat(sprintf("  %dx%dx%d : %3d stickers, %2d moves  %s\n",
              n, n, n, 6L * n * n, length(mv),
              paste(utils::head(mv, 6), collapse = " ")))
}

# inner layers of a big cube are numbered, not lettered
cat("\n4x4x4 inner layers:",
    paste(setdiff(cube_move_names(4), cube_move_names(2)), collapse = " "), "\n")

# a layer move by number is the same permutation as its letter
cat("layer 2 about x == M: ",
    identical(cube_layer_move(3, axis = 1, layer = 2, turns = 1),
              cube_moves(3)[["M"]]), "\n", sep = "")

cat("\n")
