test_that("find_path_iterative finds path for small n", {
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
    potc = 1,
    ptr = 10,
    opd = FALSE,
    reuse_combos = FALSE,
    distance_method = "manhattan",
    sort_by = c("longest", "most_unique"),
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("found" %in% names(result))

  if (result$found) {
    verification <- apply_operations(start, result$path, 3)
    expect_equal(as.integer(verification$state), as.integer(final))
  }
})

test_that("find_path_iterative works with sort_by = shortest + most_unique", {
  set.seed(42)
  start <- 1:6
  r <- apply_operations(start, c("1", "3", "2", "3"), k = 3)
  final <- r$state

  result <- find_path_iterative(
    start, final, k = 3,
    moves = c("1", "2", "3"),
    combo_length = 5,
    n_samples = 50,
    n_top = 5,
    max_iterations = 10,
    sort_by = c("shortest", "most_unique"),
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("found" %in% names(result))

  if (result$found) {
    verification <- apply_operations(start, result$path, 3)
    expect_equal(as.integer(verification$state), as.integer(final))
  }
})

test_that("find_path_iterative works with sort_by = most_repeated", {
  set.seed(7)
  start <- 1:6
  r <- apply_operations(start, c("3", "1", "1", "3"), k = 3)
  final <- r$state

  result <- find_path_iterative(
    start, final, k = 3,
    moves = c("1", "2", "3"),
    combo_length = 8,
    n_samples = 100,
    n_top = 10,
    max_iterations = 10,
    sort_by = c("most_repeated"),
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("found" %in% names(result))

  if (result$found) {
    verification <- apply_operations(start, result$path, 3)
    expect_equal(as.integer(verification$state), as.integer(final))
  }
})
