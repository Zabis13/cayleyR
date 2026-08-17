# Phase 2's twelve goals, checked as states rather than as text.
#
# The centres of a face are indistinguishable, so many finished positions are
# the same cube as far as phase 2 can tell, and each is a legitimate place to
# stop. twips lists twelve, in block notation:
#
#   "", y2, Lw2, Rw2, Uw2, Dw2, Lw2 Fw2, Rw2 Fw2, Uw2 Fw2, Dw2 Fw2,
#   Dw2 Fw2 Lw2, Lw2 Fw2 Uw2
#
# Ours are the same twelve rewritten by hand in this package's alphabet, in
# src/kociemba4.h. A hand translation is exactly the kind of thing that agrees
# with itself and disagrees with the cube -- the centre_class array in the same
# file was copied from twips position by position, without allowing for their
# face order being U L F R B D against ours of U R F D L B, and phase 2 spent
# its life leaving two centres on the wrong axis while reporting success.
#
# So this checks the translation the only way that can catch that: apply both
# spellings and compare the cubes.

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)

apply_words <- function(state, words) {
  for (mv in words) state <- state[moves4[[mv]]]
  state
}

# twips's list, in the block notation it is written in.
TWIPS_GOALS <- list(
  character(0),
  "y2",
  "Lw2",
  "Rw2",
  "Uw2",
  "Dw2",
  c("Lw2", "Fw2"),
  c("Rw2", "Fw2"),
  c("Uw2", "Fw2"),
  c("Dw2", "Fw2"),
  c("Dw2", "Fw2", "Lw2"),
  c("Lw2", "Fw2", "Uw2")
)

# Ours, transcribed from the `words` array in Solver4::init().
OUR_GOALS <- c(
  "",
  "U U D' D' 1y 1y 2y 2y",
  "L 1x' L 1x'",
  "R 2x R 2x",
  "U 2y U 2y",
  "D 1y' D 1y'",
  "L 1x' L 1x' F 2z F 2z",
  "R 2x R 2x F 2z F 2z",
  "U 2y U 2y F 2z F 2z",
  "D 1y' D 1y' F 2z F 2z",
  "D 1y' D 1y' F 2z F 2z L 1x' L 1x'",
  "L 1x' L 1x' F 2z F 2z U 2y U 2y"
)

split_word <- function(w) {
  if (!nzchar(w)) return(character(0))
  strsplit(trimws(w), " +")[[1]]
}

expand_block <- function(blocks) {
  if (!length(blocks)) return(character(0))
  unlist(lapply(blocks, function(b) cube_expand_word(b, 4)), use.names = FALSE)
}

test_that("our phase-2 goal words spell twips's goals", {
  identity4 <- cube_identity(4)
  for (i in seq_along(TWIPS_GOALS)) {
    theirs <- apply_words(identity4, expand_block(TWIPS_GOALS[[i]]))
    ours <- apply_words(identity4, split_word(OUR_GOALS[i]))
    expect_identical(ours, theirs,
                     info = paste("goal", i, "-",
                                  paste(TWIPS_GOALS[[i]], collapse = " ")))
  }
})

# The point of the twelve is that they are twelve: if any two coincide, the
# phase is being offered fewer targets than it looks, and a duplicate would
# most likely mean a mistranslation rather than a real coincidence.
test_that("the twelve goals are twelve distinct cubes", {
  states <- lapply(TWIPS_GOALS, function(g) apply_words(cube_identity(4),
                                                        expand_block(g)))
  keys <- vapply(states, function(s) paste(s, collapse = ","), "")
  expect_length(unique(keys), 12L)
})

# Every goal has to be a finished position for phase 2 -- each centre on its
# own axis -- or it is not somewhere the phase may stop.
test_that("every goal has its centres on their axes", {
  centre_stickers <- local({
    pieces <- cube_pieces(4)
    as.integer(vapply(strsplit(pieces$stickers[pieces$n_stickers == 1], ","),
                      `[`, "", 1))
  })
  axis_of <- function(face) c(1L, 2L, 3L, 1L, 2L, 3L)[face + 1L]
  slot_axis <- axis_of((centre_stickers - 1L) %/% 16L)

  for (i in seq_along(TWIPS_GOALS)) {
    state <- apply_words(cube_identity(4), expand_block(TWIPS_GOALS[[i]]))
    colour_axis <- axis_of((state[centre_stickers] - 1L) %/% 16L)
    expect_identical(colour_axis, slot_axis,
                     info = paste("goal", i, "-",
                                  paste(TWIPS_GOALS[[i]], collapse = " ")))
  }
})
