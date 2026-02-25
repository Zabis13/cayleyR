test_that("get_reachable_states_light returns cycle info", {
  result <- get_reachable_states_light(1:5, c("1", "3"), k = 3)
  expect_type(result, "list")
  expect_true(result$total_moves > 0)
  expect_true(result$unique_states_count > 0)
})

test_that("get_reachable_states returns full cycle data", {
  skip_if_not_installed("arrow")
  result <- get_reachable_states(1:5, c("1", "3"), k = 3)
  expect_type(result, "list")
  expect_true("reachable_states_df" %in% names(result))
  expect_true("cycle_info" %in% names(result))
  expect_true(result$total_moves > 0)
  # Check LRX totals present
  expect_true(!is.null(result$nL_total))
  expect_true(!is.null(result$nR_total))
  expect_true(!is.null(result$nX_total))
})

test_that("get_reachable_states_light matches full version move count", {
  skip_if_not_installed("arrow")
  full <- get_reachable_states(1:5, c("1", "3"), k = 3)
  light <- get_reachable_states_light(1:5, c("1", "3"), k = 3)
  expect_equal(full$total_moves, light$total_moves)
  expect_equal(full$unique_states_count, light$unique_states_count)
})
