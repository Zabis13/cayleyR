## Colours and positions are the two ways to write a cube down, and the
## conversion between them is exact only on the small cubes. These tests pin
## down both halves of that: that it is exact where it should be, and that it
## says so where it cannot be.

test_that("positions to colours is six blocks on a solved cube", {
  for (n in 2:6) {
    cols <- cube_colours(cube_identity(n))
    expect_equal(length(cols), 6L * n * n)
    expect_equal(sort(unique(cols)), 0:5)
    expect_equal(as.vector(table(cols)), rep(n * n, 6L))
    # the colour of a sticker is the face it sits on
    expect_identical(cols, (seq_len(6L * n * n) - 1L) %/% (n * n))
  }
})

test_that("colours round trip exactly on a 2x2x2 and a 3x3x3", {
  # Every piece of these cubes shows a different set of colours, so a
  # colouring names exactly one state.
  for (n in c(2L, 3L)) {
    set.seed(n)
    for (trial in 1:20) {
      s <- generate_state(group = cube_group(n), n_moves = 15L)
      expect_silent(back <- cube_colour_state(cube_colours(s), n))
      expect_identical(back, s)
    }
  }
})

test_that("colours are ambiguous from 4x4x4 up, and say so", {
  for (n in c(4L, 5L)) {
    set.seed(n)
    s <- generate_state(group = cube_group(n), n_moves = 20L)
    cols <- cube_colours(s)

    expect_warning(back <- cube_colour_state(cols, n), "does not name one state")

    # what comes back is a real state showing the colours asked for, even
    # though it need not be the state we started from
    expect_setequal(back, seq_len(6L * n * n))
    expect_identical(cube_colours(back, n), cols)
  }
})

test_that("a solved colouring converts to a solved cube", {
  for (n in 2:5) {
    cols <- cube_colours(cube_identity(n))
    back <- suppressWarnings(cube_colour_state(cols, n))
    expect_identical(back, cube_identity(n))
    expect_true(cube_is_colour_solved(back))
  }
})

test_that("colours may be numbered from 0 or from 1", {
  n <- 3L
  set.seed(11)
  s <- generate_state(group = cube_group(n), n_moves = 10L)
  cols <- cube_colours(s)
  expect_identical(cube_colour_state(cols, n), cube_colour_state(cols + 1L, n))
})

test_that("an impossible colouring is refused", {
  n <- 3L
  cols <- cube_colours(cube_identity(n))
  cols[1L] <- 5L                              # a U sticker that shows B
  expect_error(cube_colour_state(cols, n),
               "not ones a cube can display|more often than")
})

test_that("the solvers refuse a colour state with a message that helps", {
  set.seed(4)
  s <- generate_state(group = cube_group(3L), n_moves = 12L)
  cols <- cube_colours(s)

  # what used to happen silently: found = FALSE and no clue why
  expect_error(cube_solve_cfop(cols), "cube_colour_state")
  expect_error(cube_solve_lbl(cols), "cube_colour_state")

  # and the converted state solves
  conv <- cube_colour_state(cols, 3L)
  res <- cube_solve_cfop(conv)
  expect_true(res$found)
})

test_that("the solvers still refuse states of the wrong size or range", {
  expect_error(cube_solve_cfop(1:53), "54 entries")
  bad <- cube_identity(3L); bad[1L] <- 99L
  expect_error(cube_solve_cfop(bad), "1\\.\\.54")
})

test_that("colours travel across the Santa notation unchanged", {
  # The two conversions compose: a Santa colour state read in and then turned
  # into positions is the same as the state it stood for.
  n <- 3L
  set.seed(21)
  s <- generate_state(group = cube_group(n), n_moves = 14L)

  santa_cols <- cube_santa_state_out(cube_colours(s), n)
  ours_cols <- cube_santa_state(santa_cols, n)
  expect_identical(ours_cols, cube_colours(s))
  expect_identical(cube_colour_state(ours_cols, n), s)
})

test_that("state length is checked", {
  expect_error(cube_colours(1:10), "not 6n\\^2")
  expect_error(cube_colour_state(rep(0:5, each = 9L), n = 4L), "does not match")
  expect_error(cube_colour_state(rep(0:9, length.out = 54L)), "0\\.\\.5 or 1\\.\\.6")
})
