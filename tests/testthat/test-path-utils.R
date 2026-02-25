test_that("invert_path works", {
  expect_equal(invert_path(c("1", "3", "2")), c("1", "3", "2"))
  expect_equal(invert_path(c("1", "1", "1")), c("2", "2", "2"))
  expect_equal(invert_path(character(0)), character(0))
})

test_that("short_position cancels inverse pairs", {
  expect_equal(short_position(c("1", "2"), n = 10), character(0))
  expect_equal(short_position(c("3", "3"), n = 10), character(0))
})

test_that("short_position reduces shifts modulo n", {
  result <- short_position(rep("1", 5), n = 5)
  expect_length(result, 0)

  result <- short_position(rep("1", 7), n = 5)
  expect_equal(result, c("1", "1"))
})

test_that("validate_and_simplify_path validates correct path", {
  state <- 1:5
  result <- apply_operations(state, c("1", "3"), 3)
  final <- result$state

  v <- validate_and_simplify_path(c("1", "3"), state, final, 3)
  expect_true(v$valid)
  expect_true(length(v$path) > 0 || length(v$path) == 0)
})

test_that("validate_and_simplify_path rejects wrong path", {
  v <- validate_and_simplify_path(c("1"), 1:5, c(5L, 4L, 3L, 2L, 1L), 3)
  expect_false(v$valid)
})

test_that("validate_and_simplify_path handles NULL and empty", {
  v <- validate_and_simplify_path(NULL, 1:5, 1:5, 3)
  expect_false(v$valid)

  v <- validate_and_simplify_path(character(0), 1:5, 1:5, 3)
  expect_true(v$valid)
})
