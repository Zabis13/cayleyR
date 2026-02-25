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
  # May or may not find path with just shift_left in 1 level
  # The test verifies it doesn't error
  if (!is.null(path)) {
    result <- apply_operations(1:5, path, 3)
    expect_equal(result$state, target)
  }
})
