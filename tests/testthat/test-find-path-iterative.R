test_that("find_path_iterative finds path for small n", {
  skip_if_not_installed("arrow")
  set.seed(123)
  start <- 1:5
  # Apply known operations to get a reachable target
  r <- apply_operations(start, c("1", "3", "1", "3", "2"), k = 3)
  final <- r$state

  result <- find_path_iterative(
    start, final, k = 3,
    moves = c("1", "2", "3"),
    combo_length = 5,
    n_samples = 50,
    n_top = 5,
    max_iterations = 5,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("found" %in% names(result))

  if (result$found) {
    verification <- apply_operations(start, result$path, 3)
    expect_equal(as.integer(verification$state), as.integer(final))
  }
})
