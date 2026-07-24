test_that("bidirectional_bfs finds path for simple case", {
  path <- bidirectional_bfs(5, 1:5, c(2L, 3L, 4L, 5L, 1L),
                            max_level = 5, moves = c("1", "2", "3"), k = 3)
  expect_false(is.null(path))

  # Verify the path actually works
  result <- apply_operations(1:5, path, 3)
  expect_equal(result$state, c(2L, 3L, 4L, 5L, 1L))
})

test_that("bidirectional_bfs returns empty path for identical states", {
  path <- bidirectional_bfs(5, 1:5, 1:5,
                            max_level = 3, moves = c("1", "2", "3"), k = 3)
  expect_equal(path, character(0))
})

test_that("bidirectional_bfs returns NULL when max_level too small", {
  # Use a state that requires many moves
  target <- c(3L, 4L, 5L, 2L, 1L)
  path <- bidirectional_bfs(5, 1:5, target,
                            max_level = 1, moves = c("1"), k = 3)
  # Unreachable by construction: one left shift cannot turn 1:5 into this. The
  # search has to give up rather than hand back something the caller would read
  # as a result -- an empty path, in particular, means "already there".
  expect_null(path)
})
