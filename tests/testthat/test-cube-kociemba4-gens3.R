# Phase 3's seventeen generators, against the block notation they come from.
#
# twips gives phase 3 nine move classes -- <Uw2, U, L, Fw2, F2, Rw2, R, B2, D>
# -- which its metric expands to seventeen moves. Ours are the same seventeen
# written out in this package's alphabet, by hand, in phase3_gens4().
#
# Writing them by hand is where the bugs have been. The same three wide half
# turns were mistranslated twice in this file's neighbourhood: once in the
# phase-2 goal list and once here, both times by taking the wrong inner layer.
# Uw takes layer 2 and Dw layer 1, which does not follow from the letter, so
# the only safe way to write these is to ask cube_expand_move() -- and the only
# safe way to keep them written is a test that asks it again.

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)

apply_words <- function(state, words) {
  for (mv in words) state <- state[moves4[[mv]]]
  state
}

# The three wide half turns as phase3_gens4() spells them, transcribed from
# src/kociemba4.h.
OUR_WIDE <- c(Uw2 = "U 2y U 2y",
              Rw2 = "R 2x R 2x",
              Fw2 = "F 2z F 2z")

test_that("phase 3's wide half turns are the moves they are named after", {
  identity4 <- cube_identity(4)
  for (name in names(OUR_WIDE)) {
    ours <- apply_words(identity4, strsplit(OUR_WIDE[[name]], " ")[[1]])
    theirs <- apply_words(identity4, cube_expand_word(name, 4))
    expect_identical(ours, theirs, info = name)
  }
})

# Each is a half turn, so doing it twice is nothing.
test_that("the wide half turns have order two", {
  identity4 <- cube_identity(4)
  for (name in names(OUR_WIDE)) {
    word <- strsplit(OUR_WIDE[[name]], " ")[[1]]
    expect_identical(apply_words(apply_words(identity4, word), word), identity4,
                     info = name)
  }
})

# What phase 3 must not do is undo phase 2. Every centre is on its own axis by
# then, and these generators have to keep it that way -- that is the whole
# reason the set is this narrow rather than all twenty-four moves.
test_that("phase 3's generators keep every centre on its axis", {
  centre_stickers <- local({
    pieces <- cube_pieces(4)
    as.integer(vapply(strsplit(pieces$stickers[pieces$n_stickers == 1], ","),
                      `[`, "", 1))
  })
  axis_of <- function(face) c(1L, 2L, 3L, 1L, 2L, 3L)[face + 1L]
  slot_axis <- axis_of((centre_stickers - 1L) %/% 16L)

  generators <- c(
    list("U", "U'", c("U", "U"), "D", "D'", c("D", "D"),
         "L", "L'", c("L", "L"), "R", "R'", c("R", "R"),
         c("F", "F"), c("B", "B")),
    lapply(OUR_WIDE, function(w) strsplit(w, " ")[[1]])
  )
  expect_length(generators, 17L)

  # From the solved cube every centre is on its axis; after any one generator
  # it still has to be.
  for (i in seq_along(generators)) {
    after <- apply_words(cube_identity(4), generators[[i]])
    colour_axis <- axis_of((after[centre_stickers] - 1L) %/% 16L)
    expect_identical(colour_axis, slot_axis,
                     info = paste("generator", i,
                                  paste(generators[[i]], collapse = " ")))
  }
})

# And they have to be moves of the cube, not arbitrary permutations.
test_that("every generator is a reachable state", {
  for (word in c(list(c("U")), lapply(OUR_WIDE, function(w) strsplit(w, " ")[[1]]))) {
    after <- apply_words(cube_identity(4), word)
    expect_setequal(after, 1:96)
  }
})
