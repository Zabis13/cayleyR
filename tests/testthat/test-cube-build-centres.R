test_that("the path a build returns is the moves that produced the state", {
  # The one thing a solver must never get wrong: the word it hands back has to
  # take the cube it was given to the cube it claims. Short scrambles here so
  # the check is cheap; the full builds are exercised below.
  set.seed(3)
  for (n in c(4, 5)) {
    g <- cube_group(n)
    id <- group_identity(g)
    s <- group_apply(g, id, sample(cube_move_names(n), 12, replace = TRUE))
    r <- cube_build_lslice(s)
    expect_equal(group_apply(g, s, r$path), r$state)
    expect_lte(r$count, r$target)
  }
})

test_that("a solved cube is already built and needs no moves", {
  for (n in c(4, 5, 6)) {
    r <- cube_build_lslice(cube_identity(n))
    expect_true(r$built)
    expect_equal(r$count, r$target)
    expect_length(r$path, 0)
    expect_equal(r$rounds, 0L)
  }
})

test_that("the target is the size of the layer", {
  for (n in c(4, 5, 6)) {
    r <- cube_build_lslice(cube_identity(n))
    expect_equal(r$target, nrow(cube_lslice_cells(n)))
  }
})

test_that("the layer is built, and the three-cycles are what close it", {
  # Measured over ten scrambles at each of 4, 5 and 6: thirty of thirty. The
  # ladder alone managed two, both on a 4x4x4 -- the three-cycles finish the
  # rest, because what the ladder leaves is pieces already inside the layer
  # sitting in each other's places, which no short word rearranges.
  #
  # One scramble per size here rather than ten: a full build costs seconds, and
  # the rate is recorded in the documentation rather than re-measured on every
  # test run. `rounds` is checked so the second stage cannot be quietly lost --
  # without it the first two sizes would still pass on the ladder alone.
  set.seed(1)
  used <- 0L
  for (n in c(4, 5, 6)) {
    g <- cube_group(n)
    id <- group_identity(g)
    s <- group_apply(g, id, sample(cube_move_names(n), 40, replace = TRUE))
    r <- cube_build_lslice(s)
    expect_true(r$built)
    expect_equal(r$count, r$target)
    expect_equal(group_apply(g, s, r$path), r$state)
    used <- used + r$rounds
  }
  expect_gt(used, 0L)
})

test_that("a build respects max_rounds", {
  # With no cycles allowed the build is the ladder alone, which on a 5x5x5
  # reaches most of the layer and stops short of it.
  set.seed(1)
  g <- cube_group(5)
  id <- group_identity(g)
  s <- group_apply(g, id, sample(cube_move_names(5), 40, replace = TRUE))

  r <- cube_build_lslice(s, max_rounds = 0L)
  expect_equal(r$rounds, 0L)
  expect_false(r$built)
  expect_lt(r$count, r$target)
  expect_equal(group_apply(g, s, r$path), r$state)

  # and allowing them finishes the same cube
  full <- cube_build_lslice(s)
  expect_true(full$built)
  expect_gt(full$rounds, 0L)
})

test_that("an odd cube reads its frame off the fixed centres", {
  # A three-cycle turns the cube to reach its triple and turns it back, and
  # while it is turned "the colour that belongs on face f" is not f. On an odd
  # cube the fixed centre of each face says what belongs there whatever the
  # rotation, so a bodily turned cube is still built and needs no moves.
  #
  # An even cube has no such sticker and cannot tell: with nothing to read the
  # frame from, the face keeps its own number, and a rotated cube genuinely
  # looks unbuilt. That is a real limit rather than a bug, and stating it here
  # keeps it from being mistaken for one later.
  g <- cube_group(5)
  s <- cube_identity(5)
  for (rot in c("x", "y", "z")) {
    turned <- group_apply(g, s, cube_expand_word(rot, 5))
    r <- cube_build_lslice(turned)
    expect_true(r$built)
    expect_length(r$path, 0)
  }

  # the even case, for the record
  g4 <- cube_group(4)
  turned4 <- group_apply(g4, cube_identity(4), cube_expand_word("x", 4))
  r4 <- cube_build_lslice(turned4)
  expect_false(r4$built && length(r4$path) == 0)
})

test_that("cube_build_lslice infers n and rejects a bad length", {
  s <- cube_identity(5)
  expect_equal(cube_build_lslice(s)$target, cube_build_lslice(s, n = 5)$target)
  expect_error(cube_build_lslice(s, n = 4), "stickers")
  expect_error(cube_build_lslice(1:50), "no cube")
})

test_that("a size with no movable centres builds trivially", {
  # A 3x3x3 centre is the axis of its face: there is no layer to build.
  for (n in c(2, 3)) {
    r <- cube_build_lslice(cube_identity(n))
    expect_true(r$built)
    expect_length(r$path, 0)
  }
})
