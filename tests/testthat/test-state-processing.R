test_that("calculate_differences computes manhattan correctly", {
  df <- data.frame(V1 = c(1, 3, 2), V2 = c(2, 1, 3))
  result <- calculate_differences(c(1, 2), df)
  expect_true("difference" %in% colnames(result))
  expect_equal(result$difference[1], 0)
  expect_true(all(diff(result$difference) >= 0))
})

test_that("select_unique removes duplicate states", {
  df <- data.frame(V1 = c(1, 1, 2), V2 = c(2, 2, 1), op = c("a", "b", "c"))
  result <- select_unique(df)
  expect_equal(nrow(result), 2)
})

test_that("check_duplicates finds common states", {
  df1 <- data.frame(V1 = c(1, 2), V2 = c(2, 1))
  df2 <- data.frame(V1 = c(2, 3), V2 = c(1, 2))
  result <- check_duplicates(df1, df2)
  expect_false(is.null(result))
  expect_true(nrow(result) >= 2)
})

test_that("check_duplicates returns NULL when no overlap", {
  df1 <- data.frame(V1 = c(1), V2 = c(2))
  df2 <- data.frame(V1 = c(3), V2 = c(4))
  result <- check_duplicates(df1, df2)
  expect_null(result)
})
