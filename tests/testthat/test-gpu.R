test_that("cayley_gpu_available returns logical", {
  result <- cayley_gpu_available()
  expect_type(result, "logical")
})

test_that("cayley_gpu_status runs without error", {
  expect_no_error(cayley_gpu_status())
})

test_that("calculate_differences_gpu matches CPU", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  start_state <- c(1L, 2L, 3L, 4L, 5L)
  df <- data.frame(
    V1 = c(5, 1, 3, 2, 1),
    V2 = c(4, 2, 1, 4, 2),
    V3 = c(3, 3, 2, 1, 5),
    V4 = c(2, 4, 5, 3, 4),
    V5 = c(1, 5, 4, 5, 3)
  )

  cpu_result <- calculate_differences(start_state, df, use_gpu = FALSE)
  gpu_result <- calculate_differences(start_state, df, use_gpu = TRUE)

  expect_equal(gpu_result$difference, cpu_result$difference, tolerance = 1e-5)
  expect_equal(gpu_result[order(gpu_result$difference), "V1"],
               cpu_result[order(cpu_result$difference), "V1"])
})

test_that("calculate_differences_gpu handles single row", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  start_state <- c(1L, 2L, 3L)
  df <- data.frame(V1 = 3, V2 = 2, V3 = 1)

  cpu_result <- calculate_differences(start_state, df, use_gpu = FALSE)
  gpu_result <- calculate_differences(start_state, df, use_gpu = TRUE)

  expect_equal(gpu_result$difference, cpu_result$difference, tolerance = 1e-5)
})

test_that("calculate_differences_gpu handles large matrix", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  n <- 20L
  nrow_states <- 1000L
  start_state <- seq_len(n)

  mat <- matrix(sample(n, n * nrow_states, replace = TRUE), nrow = nrow_states)
  df <- as.data.frame(mat)
  names(df) <- paste0("V", seq_len(n))

  cpu_result <- calculate_differences(start_state, df, use_gpu = FALSE)
  gpu_result <- calculate_differences(start_state, df, use_gpu = TRUE)

  expect_equal(gpu_result$difference, cpu_result$difference, tolerance = 1e-5)
})

# ============================================================================
# Phase 3: Batch apply_operations GPU tests
# ============================================================================

test_that("build_permutation_matrix produces correct shift_left", {
  n <- 5L
  P <- matrix(build_permutation_matrix("L", n, k = 4), nrow = n)
  state <- 1:5
  result <- as.integer(round(state %*% P))
  expected <- as.integer(apply_operations(state, "L", 4, NULL)$state)
  expect_equal(result, expected)
})

test_that("build_permutation_matrix produces correct shift_right", {
  n <- 5L
  P <- matrix(build_permutation_matrix("R", n, k = 4), nrow = n)
  state <- 1:5
  result <- as.integer(round(state %*% P))
  expected <- as.integer(apply_operations(state, "R", 4, NULL)$state)
  expect_equal(result, expected)
})

test_that("build_permutation_matrix produces correct reverse_prefix", {
  n <- 5L
  P <- matrix(build_permutation_matrix("X", n, k = 4), nrow = n)
  state <- 1:5
  result <- as.integer(round(state %*% P))
  expected <- as.integer(apply_operations(state, "X", 4, NULL)$state)
  expect_equal(result, expected)
})

test_that("compose_permutation_matrix matches sequential apply", {
  state <- 1:10
  ops <- c("1", "3", "2", "1")
  k <- 4L

  cpu_state <- state
  for (op in ops) {
    cpu_state <- apply_operations(cpu_state, op, k, NULL)$state
  }

  P <- matrix(compose_permutation_matrix(ops, length(state), k),
              nrow = length(state))
  gpu_state <- as.integer(round(state %*% P))

  expect_equal(gpu_state, as.integer(cpu_state))
})

test_that("apply_operations_batch_gpu matches CPU for multiple states", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  n <- 10L
  k <- 4L
  ops <- c("1", "3", "2")

  states <- matrix(c(
    1:10,
    10:1,
    sample(10),
    sample(10),
    sample(10)
  ), nrow = 5, byrow = TRUE)

  # CPU reference
  cpu_results <- t(apply(states, 1, function(s) {
    st <- as.integer(s)
    for (op in ops) {
      st <- apply_operations(st, op, k, NULL)$state
    }
    as.numeric(st)
  }))

  # GPU
  gpu_results <- apply_operations_batch_gpu(states, ops, k)

  expect_equal(gpu_results, cpu_results, tolerance = 1e-5)
})

test_that("apply_operations_batch_gpu handles single state", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  state <- matrix(1:8, nrow = 1)
  ops <- c("L", "X", "R")
  k <- 3L

  cpu_state <- as.integer(state)
  for (op in ops) {
    cpu_state <- apply_operations(cpu_state, op, k, NULL)$state
  }

  gpu_result <- apply_operations_batch_gpu(state, ops, k)
  expect_equal(as.integer(gpu_result[1, ]), as.integer(cpu_state))
})

test_that("apply_operations_batch_gpu handles large batch", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  n <- 20L
  k <- 5L
  nrow_states <- 500L
  ops <- c("1", "3", "2", "1", "3")

  states <- matrix(sample(n, n * nrow_states, replace = TRUE),
                   nrow = nrow_states)

  cpu_results <- t(apply(states, 1, function(s) {
    st <- as.integer(s)
    for (op in ops) {
      st <- apply_operations(st, op, k, NULL)$state
    }
    as.numeric(st)
  }))

  gpu_results <- apply_operations_batch_gpu(states, ops, k)
  expect_equal(gpu_results, cpu_results, tolerance = 1e-5)
})

# ============================================================================
# Phase 4: Manhattan distance matrix GPU tests
# ============================================================================

test_that("manhattan_distance_matrix_gpu matches CPU for small matrices", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  s1 <- matrix(c(1,2,3,4,5,
                  5,4,3,2,1,
                  3,3,3,3,3), nrow = 3, byrow = TRUE)
  s2 <- matrix(c(1,1,1,1,1,
                  5,5,5,5,5), nrow = 2, byrow = TRUE)

  # CPU reference: 3x2 distance matrix
  cpu_mat <- matrix(0, nrow = 3, ncol = 2)
  for (i in 1:3) {
    for (j in 1:2) {
      cpu_mat[i, j] <- sum(abs(s1[i, ] - s2[j, ]))
    }
  }

  gpu_mat <- manhattan_distance_matrix_gpu(s1, s2)

  expect_equal(gpu_mat, cpu_mat, tolerance = 1e-5)
})

test_that("manhattan_distance_matrix_gpu handles single-row inputs", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  s1 <- matrix(1:6, nrow = 1)
  s2 <- matrix(6:1, nrow = 1)

  expected <- matrix(sum(abs(1:6 - 6:1)), nrow = 1, ncol = 1)
  gpu_mat <- manhattan_distance_matrix_gpu(s1, s2)

  expect_equal(gpu_mat, expected, tolerance = 1e-5)
})

test_that("manhattan_distance_matrix_gpu handles square case", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  n <- 10L
  N <- 50L
  s1 <- matrix(sample(n, n * N, replace = TRUE), nrow = N)
  s2 <- matrix(sample(n, n * N, replace = TRUE), nrow = N)

  cpu_mat <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N) {
    for (j in 1:N) {
      cpu_mat[i, j] <- sum(abs(s1[i, ] - s2[j, ]))
    }
  }

  gpu_mat <- manhattan_distance_matrix_gpu(s1, s2)

  expect_equal(gpu_mat, cpu_mat, tolerance = 1e-5)
})

test_that("manhattan_distance_matrix_gpu diagonal is zero for same input", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  s <- matrix(sample(20, 20 * 30, replace = TRUE), nrow = 30)
  gpu_mat <- manhattan_distance_matrix_gpu(s, s)

  expect_equal(diag(gpu_mat), rep(0, 30), tolerance = 1e-5)
})

test_that("manhattan_distance_matrix_gpu batching works for large N2", {
  skip_if_not_installed("ggmlR")
  skip_if_not(cayley_gpu_available(), "No GPU available")

  n <- 8L
  s1 <- matrix(sample(n, n * 10, replace = TRUE), nrow = 10)
  s2 <- matrix(sample(n, n * 300, replace = TRUE), nrow = 300)

  cpu_mat <- matrix(0, nrow = 10, ncol = 300)
  for (i in 1:10) {
    for (j in 1:300) {
      cpu_mat[i, j] <- sum(abs(s1[i, ] - s2[j, ]))
    }
  }

  # Use small batch_size to test batching logic
  gpu_mat <- manhattan_distance_matrix_gpu(s1, s2, batch_size = 64L)

  expect_equal(gpu_mat, cpu_mat, tolerance = 1e-5)
})
