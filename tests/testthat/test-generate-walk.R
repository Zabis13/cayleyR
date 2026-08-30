test_that("generate_walk returns states of the group's length", {
  g <- cube_group(3)
  w <- generate_walk(g, n = 4L, n_moves = 7L)

  expect_equal(dim(w$states), c(4L, g$n))
  expect_length(w$depth, 4L)
  expect_length(w$moves, 4L)
  expect_length(w$solution, 4L)
})

test_that("exact = TRUE walks exactly n_moves", {
  g <- cube_group(3)
  w <- generate_walk(g, n = 20L, n_moves = 9L, exact = TRUE)

  expect_true(all(w$depth == 9L))
  expect_true(all(lengths(w$moves) == 9L))
})

test_that("exact = FALSE draws lengths within 1..n_moves", {
  set.seed(11)
  g <- cube_group(3)
  w <- generate_walk(g, n = 200L, n_moves = 8L, exact = FALSE)

  expect_true(all(w$depth >= 1L & w$depth <= 8L))
  expect_true(all(lengths(w$moves) == w$depth))
  # With 200 draws over 8 depths, seeing only one would mean the length is not
  # being drawn at all.
  expect_gt(length(unique(w$depth)), 1L)
})

test_that("the reported word really produces the state", {
  set.seed(12)
  g  <- cube_group(3)
  w  <- generate_walk(g, n = 5L, n_moves = 6L)
  id <- group_identity(g)

  for (i in seq_len(5L)) {
    expect_equal(group_apply(g, id, w$moves[[i]]), as.integer(w$states[i, ]))
  }
})

test_that("the reported solution takes the state home", {
  set.seed(13)
  g  <- cube_group(3)
  w  <- generate_walk(g, n = 5L, n_moves = 8L)
  id <- group_identity(g)

  for (i in seq_len(5L)) {
    expect_equal(group_apply(g, w$states[i, ], w$solution[[i]]), id)
  }
})

test_that("no_undo refuses a move that undoes the one before it", {
  set.seed(14)
  g <- cube_group(3)
  w <- generate_walk(g, n = 60L, n_moves = 12L, no_undo = TRUE)

  for (word in w$moves) {
    if (length(word) < 2L) next
    # Each consecutive pair must not cancel: the second is never the inverse of
    # the first.
    undoes <- vapply(seq_len(length(word) - 1L), function(j) {
      identical(invert_path(word[[j]], group = g), word[[j + 1L]])
    }, logical(1L))
    expect_false(any(undoes))
  }
})

test_that("no_undo = FALSE lets a walk cancel its own move", {
  set.seed(15)
  g <- cube_group(3)
  w <- generate_walk(g, n = 300L, n_moves = 12L, no_undo = FALSE)

  # Over 300 walks of 12 moves in an 18-move alphabet, a cancelling pair is
  # near certain -- its absence would mean the switch is not being honoured.
  any_undo <- any(vapply(w$moves, function(word) {
    if (length(word) < 2L) return(FALSE)
    any(vapply(seq_len(length(word) - 1L), function(j) {
      identical(invert_path(word[[j]], group = g), word[[j + 1L]])
    }, logical(1L)))
  }, logical(1L)))

  expect_true(any_undo)
})

test_that("generate_walk works on groups other than the 3x3x3", {
  set.seed(16)
  for (g in list(cube_group(2), cube_group(4))) {
    w  <- generate_walk(g, n = 3L, n_moves = 5L)
    id <- group_identity(g)
    expect_equal(dim(w$states), c(3L, g$n))
    for (i in seq_len(3L)) {
      expect_equal(group_apply(g, w$states[i, ], w$solution[[i]]), id)
    }
  }
})

test_that("generate_walk rejects bad arguments", {
  g <- cube_group(3)
  expect_error(generate_walk("not a group"), "perm_group")
  expect_error(generate_walk(g, n = 0L), "positive")
  expect_error(generate_walk(g, n_moves = 0L), "positive")
})
