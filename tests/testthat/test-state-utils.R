test_that("convert_digits works with digit string", {
  expect_equal(convert_digits("123"), c(1L, 2L, 3L))
  expect_equal(convert_digits("13"), c(1L, 3L))
})

test_that("convert_digits works with space-separated numbers", {
  expect_equal(convert_digits("1 5 4 3 2"), c(1L, 5L, 4L, 3L, 2L))
  expect_equal(convert_digits("10 11 12"), c(10L, 11L, 12L))
})

test_that("generate_state returns valid permutation", {
  set.seed(42)
  state <- generate_state(10)
  expect_length(state, 10)
  expect_equal(sort(state), 1:10)
})

test_that("generate_unique_states_df returns correct structure", {
  set.seed(42)
  df <- generate_unique_states_df(5, 10)
  expect_true(all(paste0("V", 1:5) %in% colnames(df)))
  expect_true(nrow(df) <= 10)
  # Each row is a permutation of 1:5
  for (i in 1:nrow(df)) {
    expect_equal(sort(as.integer(df[i, ])), 1:5)
  }
})

test_that("manhattan_distance is correct", {
  expect_equal(manhattan_distance(1:5, 1:5), 0)
  expect_equal(manhattan_distance(1:5, 5:1), 12)
  expect_equal(manhattan_distance(c(1, 2, 3), c(3, 2, 1)), 4)
})
