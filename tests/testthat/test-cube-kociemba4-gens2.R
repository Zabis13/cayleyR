# Phase 2's generators, against what the phase promises.
#
# Unlike the phase-3 list, these are written straight in this package's
# alphabet -- "1x2", "2z'" -- rather than translated from block notation, so
# the mistranslation that hit centre_class, goals2 and phase3_gens4 has no
# foothold here. What can still be wrong is the choice: phase 2 has to keep
# what phase 1 built, and a generator that moves an F/B centre off its axis
# would undo the phase before it while its own coordinate reported progress.
#
# twips gives this phase <Uw2, U, L, Fw, F, Rw2, R, B, D>. Ours is broader --
# twenty-eight moves against their twenty-three -- because their wide turns
# are single steps of a derived puzzle and ours are words; the note above
# phase2_gens4() records the measurement that none of the extra five is spare.
# What both sets must agree on is the invariant, and that is what is checked.

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)

apply_words <- function(state, words) {
  for (mv in words) state <- state[moves4[[mv]]]
  state
}

# Transcribed from phase2_gens4() in src/kociemba4.h.
PHASE2_GENERATORS <- c(
  "U", "U'", "U2", "D", "D'", "D2", "L", "L'", "L2", "R", "R'", "R2",
  "F", "F'", "F2", "B", "B'", "B2",
  "1x2", "2x2", "1y2", "2y2",
  "1z", "1z'", "1z2", "2z", "2z'", "2z2"
)

# The phase's own spelling: a trailing 2 means the move twice.
as_moves <- function(generator) {
  if (grepl("2$", generator) && nchar(generator) > 1) {
    base <- sub("2$", "", generator)
    c(base, base)
  } else {
    generator
  }
}

centre_stickers <- local({
  pieces <- cube_pieces(4)
  as.integer(vapply(strsplit(pieces$stickers[pieces$n_stickers == 1], ","),
                    `[`, "", 1))
})

FACE_F <- 2L
FACE_B <- 5L
fb_slots <- centre_stickers[(centre_stickers - 1L) %/% 16L %in% c(FACE_F, FACE_B)]

test_that("the generator list is twenty-eight moves", {
  expect_length(PHASE2_GENERATORS, 28L)
  expect_length(unique(PHASE2_GENERATORS), 28L)
})

test_that("every phase-2 generator is a move of the cube", {
  for (generator in PHASE2_GENERATORS) {
    after <- apply_words(cube_identity(4), as_moves(generator))
    expect_setequal(after, 1:96)
  }
})

# The invariant phase 2 inherits: phase 1 put the F/B-coloured centres on the
# F/B faces, and nothing phase 2 turns may take them off again.
test_that("no phase-2 generator moves an F/B centre off its axis", {
  for (generator in PHASE2_GENERATORS) {
    after <- apply_words(cube_identity(4), as_moves(generator))
    colours <- (after[fb_slots] - 1L) %/% 16L
    expect_true(all(colours %in% c(FACE_F, FACE_B)),
                info = paste("generator", generator, "broke the F/B axis"))
  }
})

# The reason the inner x and y layers appear only as half turns: a quarter turn
# of them would carry a centre off the F/B axis. This states that directly, so
# that anyone widening the set sees why it is narrow.
test_that("a quarter turn of an inner x or y layer would break the F/B axis", {
  broke <- vapply(c("1x", "2x", "1y", "2y"), function(mv) {
    after <- apply_words(cube_identity(4), mv)
    !all(((after[fb_slots] - 1L) %/% 16L) %in% c(FACE_F, FACE_B))
  }, logical(1))
  expect_true(all(broke))
})

# And why the inner z layers are allowed quarter turns: z is the F/B axis, so
# turning it keeps those centres on their own faces.
test_that("a quarter turn of an inner z layer keeps the F/B axis", {
  for (mv in c("1z", "1z'", "2z", "2z'")) {
    after <- apply_words(cube_identity(4), mv)
    expect_true(all(((after[fb_slots] - 1L) %/% 16L) %in% c(FACE_F, FACE_B)),
                info = mv)
  }
})
