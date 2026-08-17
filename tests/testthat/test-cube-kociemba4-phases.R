# The reduction phases, one at a time, each against what it promises.
#
# cube_kociemba4_reduce() runs all three and returns a path only if all three
# succeed, so when it comes back empty there is no way to tell which phase gave
# up or what it left behind. These tests go phase by phase instead, checking
# the promise each one makes rather than the outcome of the chain.
#
# The promises are read off the derivers in src/kociemba4.h, and checked here
# on the stickers -- deliberately not on the phase's own coordinate, so that a
# coordinate that has drifted from what it is supposed to mean shows up as a
# failure rather than agreeing with itself.

moves4 <- cube_moves(4)
names(moves4) <- cube_move_names(4)

apply_path <- function(state, path) {
  for (mv in path) state <- state[moves4[[mv]]]
  state
}

# The centre slots, from the geometry rather than a table: a centre is the
# piece carrying one sticker.
centre_stickers <- local({
  pieces <- cube_pieces(4)
  as.integer(vapply(strsplit(pieces$stickers[pieces$n_stickers == 1], ","),
                    `[`, "", 1))
})

face_of_sticker <- function(sticker) (sticker - 1L) %/% 16L
colour_of <- function(state, sticker) (state[sticker] - 1L) %/% 16L

# Faces are numbered in cube_move_names() order: U R F D L B. So F is 2 and
# B is 5, which is what face_is_fb() says in the C++.
FACE_F <- 2L
FACE_B <- 5L

# Which centre slots sit on the F and B faces.
fb_slots <- centre_stickers[face_of_sticker(centre_stickers) %in% c(FACE_F, FACE_B)]

# Phase 1's promise, stated as a predicate: every centre on the F/B faces
# carries an F or B colour. Equivalently, no F/B-coloured centre is anywhere
# else -- there are exactly eight of each, so the two readings agree.
fb_centres_on_axis <- function(state) {
  all(colour_of(state, fb_slots) %in% c(FACE_F, FACE_B))
}

test_that("phase 1 puts the F/B centres on the F/B axis", {
  set.seed(4001)
  for (i in 1:8) {
    scramble <- generate_state(group = cube_group(4), n_moves = 10)

    # Phase 1 alone.
    path <- cayleyR:::cube_kociemba4_phase12_cpp(scramble, upto_phase = 1L)
    after <- apply_path(scramble, path)

    expect_true(fb_centres_on_axis(after),
                info = paste("scramble", i, "-- F/B centres not on their axis"))
  }
})

test_that("phase 1 leaves a solved cube alone", {
  expect_length(cayleyR:::cube_kociemba4_phase12_cpp(cube_identity(4),
                                                     upto_phase = 1L), 0L)
})

# Phase 2 keeps phase 1's work and adds its own: the L/R and U/D centres go to
# their own axes too. After it, every centre is on the axis it belongs to --
# which face of the pair it sits on is not settled until later, and is not
# checked here.
axis_of_face <- function(face) {
  # U/D is one axis, R/L another, F/B the third.
  c(1L, 2L, 3L, 1L, 2L, 3L)[face + 1L]
}

all_centres_on_axis <- function(state) {
  all(axis_of_face(colour_of(state, centre_stickers)) ==
      axis_of_face(face_of_sticker(centre_stickers)))
}

test_that("phase 2 puts every centre on its own axis", {
  set.seed(4002)
  for (i in 1:8) {
    scramble <- generate_state(group = cube_group(4), n_moves = 10)
    path <- cayleyR:::cube_kociemba4_phase12_cpp(scramble)
    after <- apply_path(scramble, path)

    expect_true(all_centres_on_axis(after),
                info = paste("scramble", i, "-- some centre is off its axis"))
  }
})

test_that("phase 2 does not undo phase 1", {
  set.seed(4003)
  for (i in 1:8) {
    scramble <- generate_state(group = cube_group(4), n_moves = 10)
    after <- apply_path(scramble, cayleyR:::cube_kociemba4_phase12_cpp(scramble))

    expect_true(fb_centres_on_axis(after),
                info = paste("scramble", i, "-- phase 2 lost the F/B axis"))
  }
})

# Both phases have to hand on a cube that is still a cube: their moves are
# moves, so the state stays a permutation of the stickers.
test_that("the phases return a reachable state", {
  set.seed(4004)
  scramble <- generate_state(group = cube_group(4), n_moves = 10)
  after <- apply_path(scramble, cayleyR:::cube_kociemba4_phase12_cpp(scramble))

  expect_length(after, 96L)
  expect_setequal(after, 1:96)
})
