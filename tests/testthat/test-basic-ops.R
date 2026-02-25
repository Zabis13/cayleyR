test_that("shift_left_simple works", {
  expect_equal(shift_left_simple(1:5), c(2L, 3L, 4L, 5L, 1L))
  expect_equal(shift_left_simple(c(3L, 1L, 2L)), c(1L, 2L, 3L))
})

test_that("shift_right_simple works", {
  expect_equal(shift_right_simple(1:5), c(5L, 1L, 2L, 3L, 4L))
  expect_equal(shift_right_simple(c(3L, 1L, 2L)), c(2L, 3L, 1L))
})

test_that("reverse_prefix_simple works", {
  expect_equal(reverse_prefix_simple(1:5, 3), c(3L, 2L, 1L, 4L, 5L))
  expect_equal(reverse_prefix_simple(1:5, 5), 5:1)
  expect_equal(reverse_prefix_simple(1:5, 1), 1:5)
})

test_that("shift_left returns list with state and coords", {
  result <- shift_left(1:5)
  expect_type(result, "list")
  expect_equal(result$state, c(2L, 3L, 4L, 5L, 1L))
  expect_equal(result$coords$nL, 1)
  expect_equal(result$coords$nR, 0)
  expect_equal(result$coords$nX, 0)
})

test_that("shift_right returns list with state and coords", {
  result <- shift_right(1:5)
  expect_type(result, "list")
  expect_equal(result$state, c(5L, 1L, 2L, 3L, 4L))
  expect_equal(result$coords$nR, 1)
})

test_that("reverse_prefix returns list with state and coords", {
  result <- reverse_prefix(1:5, 3)
  expect_type(result, "list")
  expect_equal(result$state, c(3L, 2L, 1L, 4L, 5L))
  expect_equal(result$coords$nX, 1)
})

test_that("apply_operations works with single and multiple ops", {
  result <- apply_operations(1:5, "1", 3)
  expect_equal(result$state, c(2L, 3L, 4L, 5L, 1L))

  result <- apply_operations(1:5, c("1", "3"), 3)
  expect_type(result, "list")
  expect_true("state" %in% names(result))
  expect_true("coords" %in% names(result))
})

test_that("apply_operations supports L/R/X aliases", {
  r1 <- apply_operations(1:5, c("L", "X", "R"), 3)
  r2 <- apply_operations(1:5, c("1", "3", "2"), 3)
  expect_equal(r1$state, r2$state)
})

test_that("shift_left/shift_right are inverses", {
  state <- c(3L, 1L, 4L, 5L, 2L)
  r <- shift_left(state)
  r2 <- shift_right(r$state, r$coords)
  expect_equal(r2$state, state)
})

test_that("round-trip with apply_operations", {
  state <- 1:10
  # n left shifts should return to original
  ops <- rep("1", 10)
  result <- apply_operations(state, ops, 4)
  expect_equal(result$state, state)
})

test_that("coords accumulate correctly", {
  r1 <- shift_left(1:5)
  r2 <- shift_left(r1$state, r1$coords)
  r3 <- shift_right(r2$state, r2$coords)
  expect_equal(r3$coords$nL, 2)
  expect_equal(r3$coords$nR, 1)
})
