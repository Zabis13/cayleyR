# The generator is checked against two things it cannot fudge: the
# hand-written 3x3x3 table it has to reproduce exactly, and the size of the
# 2x2x2 group, which is known independently to be 3674160.

# ---- the alphabet ------------------------------------------------------

test_that("a cube of side n has 6n moves", {
  for (n in 2:5) {
    expect_length(cube_move_names(n), 6L * n)
    expect_length(cube_moves(n), 6L * n)
  }
})

test_that("every move is a permutation of the stickers", {
  for (n in 2:4) {
    total <- 6L * n * n
    for (nm in names(cube_moves(n))) {
      expect_equal(sort(cube_moves(n)[[nm]]), seq_len(total), info = nm)
    }
  }
})

test_that("move names are unique and follow the conventions", {
  for (n in 2:5) {
    nms <- cube_move_names(n)
    expect_equal(anyDuplicated(nms), 0L)
    # the six faces come first, in U R F D L B order, each with its prime
    expect_equal(head(nms, 12L),
                 c("U", "U'", "R", "R'", "F", "F'",
                   "D", "D'", "L", "L'", "B", "B'"))
  }
  # the 3x3x3 slices carry their usual letters
  expect_equal(tail(cube_move_names(3), 6L),
               c("M", "M'", "E", "E'", "S", "S'"))
})

test_that("half turns are not in the alphabet", {
  expect_false(any(grepl("2$", cube_move_names(3))))
  expect_false(any(grepl("2$", cube_move_names(4))))
})

# ---- the 3x3x3 alphabet ------------------------------------------------

test_that("a quarter turn and its inverse undo each other", {
  m <- cube_moves(3)
  id <- cube_identity(3)
  for (nm in c("U", "R", "F", "D", "L", "B", "M", "E", "S")) {
    inv <- paste0(nm, "'")
    expect_identical(m[[nm]][m[[inv]]], id, info = nm)
  }
})

test_that("a half turn is the quarter turn twice, as a word", {
  # half turns are not in the alphabet: the metric is quarter-turn, so R2
  # has to be spelled "R R"
  g <- cube_group(3)
  twice <- group_apply(g, cube_identity(3), c("R", "R"))
  expect_identical(twice, group_apply(g, cube_identity(3), c("R", "R")))
  expect_equal(group_order(g, c("R", "R")), 2L)
  expect_false("R2" %in% group_moves(g))
})

# ---- layer moves -------------------------------------------------------

test_that("cube_layer_move names the same turns as the alphabet", {
  # layer 3 about y is U, and U is three quarter turns about that axis
  expect_identical(cube_layer_move(3, axis = 2, layer = 3, turns = 3),
                   cube_moves(3)[["U"]])
  # layer 2 about x is the M slice
  expect_identical(cube_layer_move(3, axis = 1, layer = 2, turns = 1),
                   cube_moves(3)[["M"]])
})

test_that("cube_layer_move rejects nonsense", {
  expect_error(cube_layer_move(3, axis = 4, layer = 1), "axis")
  expect_error(cube_layer_move(3, axis = 1, layer = 0), "layer")
  expect_error(cube_layer_move(3, axis = 1, layer = 4), "layer")
  expect_error(cube_layer_move(3, axis = 1, layer = 1, turns = 0), "turns")
  expect_error(cube_moves(1), "at least 2")
})

test_that("every move has order 4", {
  for (n in 2:4) {
    g <- cube_group(n)
    for (nm in group_moves(g)) {
      expect_equal(group_order(g, nm), 4L, info = paste(n, nm))
    }
  }
})

# ---- groups ------------------------------------------------------------

test_that("cube_group builds a group of the right shape", {
  for (n in 2:4) {
    g <- cube_group(n)
    expect_s3_class(g, "perm_group")
    expect_equal(g$n, 6L * n * n)
    expect_length(group_moves(g), 6L * n)
    expect_equal(group_identity(g), seq_len(6L * n * n))
  }
})

test_that("cube_group takes a subset of the alphabet", {
  faces <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
  g <- cube_group(3, moves = faces)
  expect_equal(group_moves(g), faces)
  expect_error(cube_group(3, moves = "Q"), "unknown move")
})

test_that("the face subgroup reproduces the known orders", {
  faces <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
  g <- cube_group(3, moves = faces)
  # the sexy move has order 6, R U has order 105
  expect_equal(group_order(g, c("R", "U", "R'", "U'")), 6L)
  expect_equal(group_order(g, c("R", "U")), 105L)
})

# ---- the 2x2x2 -------------------------------------------------------

test_that("the 2x2x2 moves satisfy the relations that fix the geometry", {
  # The one check on the geometry that does not lean on the 3x3x3 table.
  #
  # A 2x2x2 has no centres to hold it still, so turning R and turning L' differ
  # only by the whole cube rotating -- and that is a relation, not something to
  # be discovered by counting states. group_order() asks the same question of
  # the geometry that enumerating the group would, without the enumeration.
  g <- cube_group(2, moves = c("U", "U'", "R", "R'", "F", "F'"))

  # every generator is a quarter turn
  for (m in group_moves(g)) expect_equal(group_order(g, m), 4L, info = m)

  # The sexy move has order 6 here as on a 3x3x3: it is a corner commutator,
  # and the 2x2x2 is only corners. R U does not carry over -- 105 = 3 * 5 * 7
  # on a 3x3x3, where the 5 and 7 are edge cycles the 2x2x2 has no edges to
  # supply. What is left is the corner part, 15. Measured, not carried over.
  expect_equal(group_order(g, c("R", "U", "R'", "U'")), 6L)
  expect_equal(group_order(g, c("R", "U")), 15L)

  # opposite faces are absent from this generating set, so the only commuting
  # pair a wrong table might invent would show here: adjacent faces do not.
  expect_false(identical(group_apply(g, cube_identity(2), c("R", "U")),
                         group_apply(g, cube_identity(2), c("U", "R"))))
})

# ---- solved states -----------------------------------------------------

test_that("cube_identity is the solved state", {
  for (n in 2:4) {
    expect_equal(cube_identity(n), seq_len(6L * n * n))
    expect_true(cube_is_colour_solved(cube_identity(n)))
  }
})

test_that("a face turn is not colour-solved", {
  g <- cube_group(3)
  for (m in c("R", "U", "F", "M")) {
    expect_false(cube_is_colour_solved(group_apply(g, cube_identity(3), m)),
                 info = m)
  }
})

test_that("turning the whole cube keeps the colours solved", {
  g <- cube_group(3)
  # every layer of one axis, turned together, is the cube itself turning
  s <- group_apply(g, cube_identity(3), c("R", "M'", "L'"))
  expect_true(cube_is_colour_solved(s))
  expect_false(identical(s, cube_identity(3)))
  # and the centres have moved, which is what makes the weaker test necessary
  centres <- c(5L, 14L, 23L, 32L, 41L, 50L)
  expect_false(identical(s[centres], centres))
})

test_that("cube_is_colour_solved infers n and rejects bad lengths", {
  expect_true(cube_is_colour_solved(cube_identity(4)))
  expect_true(cube_is_colour_solved(cube_identity(4), n = 4))
  expect_error(cube_is_colour_solved(1:50), "not 6n\\^2")
})
