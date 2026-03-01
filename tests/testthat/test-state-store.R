test_that("StateStore create and basic operations", {
  store <- create_state_store(6L)
  expect_equal(state_store_size(store), 0L)
  expect_equal(state_store_perm_length(store), 6L)
  expect_equal(state_store_unique_count(store), 0L)
})

test_that("StateStore add_from_df and retrieval", {
  store <- create_state_store(4L)

  df <- data.frame(
    V1 = c(1L, 2L, 3L),
    V2 = c(2L, 3L, 1L),
    V3 = c(3L, 1L, 2L),
    V4 = c(4L, 4L, 4L),
    operation = c("1", "2", "3"),
    step = c(1L, 2L, 3L),
    combo_number = c(1L, 1L, 1L),
    nL = c(1L, 1L, 1L),
    nR = c(0L, 1L, 1L),
    nX = c(0L, 0L, 1L),
    theta = c(0.1, 0.2, 0.3),
    phi = c(0.4, 0.5, 0.6),
    omega_conformal = c(1.0, 1.4, 1.7),
    stringsAsFactors = FALSE
  )

  store_add_from_df(store, df, cycle_val = 1L)
  expect_equal(state_store_size(store), 3L)
  expect_equal(state_store_unique_count(store), 3L)

  # Get state
  s1 <- store_get_state(store, 0L)
  expect_equal(s1, c(1L, 2L, 3L, 4L))

  s3 <- store_get_state(store, 2L)
  expect_equal(s3, c(3L, 1L, 2L, 4L))

  # Get meta
  m1 <- store_get_meta(store, 0L)
  expect_equal(m1$step, 1L)
  expect_equal(m1$combo_number, 1L)
  expect_equal(m1$cycle, 1L)
  expect_equal(m1$operation, "1")

  m3 <- store_get_meta(store, 2L)
  expect_equal(m3$operation, "3")
  expect_equal(m3$nX, 1L)
})

test_that("StateStore lookup by state", {
  store <- create_state_store(3L)
  df <- data.frame(
    V1 = c(1L, 2L, 1L),
    V2 = c(2L, 1L, 2L),
    V3 = c(3L, 3L, 3L),
    operation = c("1", "2", "1"),
    step = c(1L, 2L, 3L),
    combo_number = c(1L, 1L, 2L),
    nL = integer(3), nR = integer(3), nX = integer(3),
    theta = numeric(3), phi = numeric(3), omega_conformal = numeric(3),
    stringsAsFactors = FALSE
  )
  store_add_from_df(store, df, cycle_val = 1L)

  # State [1,2,3] appears at rows 0 and 2
  idx <- store_lookup(store, c(1L, 2L, 3L))
  expect_equal(sort(idx), c(0L, 2L))

  # State [2,1,3] appears at row 1
  idx2 <- store_lookup(store, c(2L, 1L, 3L))
  expect_equal(idx2, 1L)

  # Non-existent state
  idx3 <- store_lookup(store, c(9L, 9L, 9L))
  expect_length(idx3, 0L)
})

test_that("StateStore find intersections", {
  store_a <- create_state_store(3L)
  store_b <- create_state_store(3L)

  df_a <- data.frame(
    V1 = c(1L, 2L), V2 = c(2L, 1L), V3 = c(3L, 3L),
    operation = c("1", "2"), step = 1:2, combo_number = c(1L, 1L),
    nL = integer(2), nR = integer(2), nX = integer(2),
    theta = numeric(2), phi = numeric(2), omega_conformal = numeric(2),
    stringsAsFactors = FALSE
  )
  df_b <- data.frame(
    V1 = c(2L, 3L), V2 = c(1L, 2L), V3 = c(3L, 1L),
    operation = c("1", "2"), step = 1:2, combo_number = c(1L, 1L),
    nL = integer(2), nR = integer(2), nX = integer(2),
    theta = numeric(2), phi = numeric(2), omega_conformal = numeric(2),
    stringsAsFactors = FALSE
  )

  store_add_from_df(store_a, df_a, cycle_val = 1L)
  store_add_from_df(store_b, df_b, cycle_val = 1L)

  isect <- store_find_intersections(store_a, store_b)
  expect_equal(length(isect), 1L) # [2,1,3] is common
  expect_equal(isect[1], "2_1_3")
})

test_that("StateStore find_best_match", {
  store <- create_state_store(4L)
  df <- data.frame(
    V1 = c(1L, 3L, 2L),
    V2 = c(2L, 4L, 3L),
    V3 = c(3L, 1L, 4L),
    V4 = c(4L, 2L, 1L),
    operation = c("1", "2", "3"),
    step = c(1L, 2L, 3L),
    combo_number = c(1L, 1L, 1L),
    nL = integer(3), nR = integer(3), nX = integer(3),
    theta = numeric(3), phi = numeric(3), omega_conformal = numeric(3),
    stringsAsFactors = FALSE
  )
  store_add_from_df(store, df, cycle_val = 1L)

  # Target [1,2,3,4] — exact match is index 0
  best <- store_find_best_match(store, c(1L, 2L, 3L, 4L))
  expect_equal(best, 0L)

  # Target [2,3,4,1] — closest is index 2 [2,3,4,1]
  best2 <- store_find_best_match(store, c(2L, 3L, 4L, 1L))
  expect_equal(best2, 2L)
})

test_that("StateStore filter_middle", {
  store <- create_state_store(3L)
  # 10 states in combo 1, steps 1..10
  n <- 10L
  df <- data.frame(
    V1 = rep(1L, n), V2 = rep(2L, n), V3 = seq_len(n),
    operation = rep("1", n),
    step = seq_len(n),
    combo_number = rep(1L, n),
    nL = integer(n), nR = integer(n), nX = integer(n),
    theta = numeric(n), phi = numeric(n), omega_conformal = numeric(n),
    stringsAsFactors = FALSE
  )
  store_add_from_df(store, df, cycle_val = 1L)

  # skip_first=2, skip_last=2: keep steps 3..8
  filtered <- store_filter_middle(store, 1L, skip_first = 2L, skip_last = 2L)
  # steps at these indices: 3, 4, 5, 6, 7, 8
  expect_equal(length(filtered), 6L)
})

test_that("StateStore to_dataframe roundtrip", {
  store <- create_state_store(3L)
  df_in <- data.frame(
    V1 = c(1L, 2L), V2 = c(2L, 1L), V3 = c(3L, 3L),
    operation = c("1", "2"), step = 1:2, combo_number = c(1L, 1L),
    nL = c(1L, 1L), nR = c(0L, 1L), nX = c(0L, 0L),
    theta = c(0.1, 0.2), phi = c(0.3, 0.4), omega_conformal = c(1.0, 1.4),
    stringsAsFactors = FALSE
  )
  store_add_from_df(store, df_in, cycle_val = 1L)

  df_out <- store_to_dataframe(store)
  expect_equal(nrow(df_out), 2L)
  expect_equal(df_out$V1, c(1L, 2L))
  expect_equal(df_out$V2, c(2L, 1L))
  expect_equal(df_out$operation, c("1", "2"))
  expect_equal(df_out$step, 1:2)
  expect_equal(df_out$nL, c(1L, 1L))
  expect_equal(df_out$cycle, c(1L, 1L))
  expect_equal(df_out$theta, c(0.1, 0.2), tolerance = 1e-10)
})

test_that("StateStore incremental add across cycles", {
  store <- create_state_store(3L)

  df1 <- data.frame(
    V1 = 1L, V2 = 2L, V3 = 3L,
    operation = "1", step = 1L, combo_number = 1L,
    nL = 0L, nR = 0L, nX = 0L,
    theta = 0, phi = 0, omega_conformal = 0,
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    V1 = 2L, V2 = 3L, V3 = 1L,
    operation = "2", step = 1L, combo_number = 1L,
    nL = 0L, nR = 0L, nX = 0L,
    theta = 0, phi = 0, omega_conformal = 0,
    stringsAsFactors = FALSE
  )

  store_add_from_df(store, df1, cycle_val = 1L)
  store_add_from_df(store, df2, cycle_val = 2L)

  expect_equal(state_store_size(store), 2L)
  expect_equal(state_store_unique_count(store), 2L)

  # Check cycle filtering
  c1 <- state_store_indices_for_cycle(store, 1L)
  c2 <- state_store_indices_for_cycle(store, 2L)
  expect_equal(c1, 0L)
  expect_equal(c2, 1L)
})

test_that("store_analyze_combos_gpu matches CPU store_analyze_combos", {
  skip_if_not_installed("ggmlR")
  skip_if(!cayley_gpu_available(), "GPU not available")

  set.seed(123)
  start <- 1:6
  k <- 3L

  combos <- find_best_random_combinations(
    moves = c("1", "2", "3"),
    combo_length = 5L,
    n_samples = 10L,
    n_top = 3L,
    start_state = start,
    k = k
  )

  # CPU path
  store_cpu <- create_state_store(6L)
  store_analyze_combos(store_cpu, combos, start, k, cycle_val = 1L)
  df_cpu <- store_to_dataframe(store_cpu)

  # GPU path
  store_gpu <- create_state_store(6L)
  store_analyze_combos_gpu(store_gpu, combos, start, k, cycle_val = 1L)
  df_gpu <- store_to_dataframe(store_gpu)

  # Same number of rows
  expect_equal(nrow(df_gpu), nrow(df_cpu))

  # Compare per combo: sort within each combo by step for stable comparison
  v_cols <- paste0("V", 1:6)

  for (combo_num in unique(df_cpu$combo_number)) {
    rows_cpu <- df_cpu[df_cpu$combo_number == combo_num, ]
    rows_gpu <- df_gpu[df_gpu$combo_number == combo_num, ]

    # Same row count per combo
    expect_equal(nrow(rows_gpu), nrow(rows_cpu),
                 info = paste("combo", combo_num, "row count"))

    # Sort by step (NA last)
    ord_cpu <- order(rows_cpu$step, na.last = TRUE)
    ord_gpu <- order(rows_gpu$step, na.last = TRUE)
    rows_cpu <- rows_cpu[ord_cpu, ]
    rows_gpu <- rows_gpu[ord_gpu, ]

    # State columns match
    for (vc in v_cols) {
      expect_equal(rows_gpu[[vc]], rows_cpu[[vc]],
                   info = paste("combo", combo_num, vc))
    }

    # Steps match
    expect_equal(rows_gpu$step, rows_cpu$step,
                 info = paste("combo", combo_num, "step"))

    # Operations match
    expect_equal(as.character(rows_gpu$operation),
                 as.character(rows_cpu$operation),
                 info = paste("combo", combo_num, "operation"))

    # Coordinates match
    expect_equal(rows_gpu$nL, rows_cpu$nL,
                 info = paste("combo", combo_num, "nL"))
    expect_equal(rows_gpu$nR, rows_cpu$nR,
                 info = paste("combo", combo_num, "nR"))
    expect_equal(rows_gpu$nX, rows_cpu$nX,
                 info = paste("combo", combo_num, "nX"))
    expect_equal(rows_gpu$theta, rows_cpu$theta, tolerance = 1e-10,
                 info = paste("combo", combo_num, "theta"))
    expect_equal(rows_gpu$phi, rows_cpu$phi, tolerance = 1e-10,
                 info = paste("combo", combo_num, "phi"))
    expect_equal(rows_gpu$omega_conformal, rows_cpu$omega_conformal, tolerance = 1e-10,
                 info = paste("combo", combo_num, "omega"))
  }
})

test_that("store_analyze_combos matches analyze_top_combinations", {
  set.seed(42)
  start <- 1:6
  k <- 3L

  # Get some combos
  combos <- find_best_random_combinations(
    moves = c("1", "2", "3"),
    combo_length = 5L,
    n_samples = 10L,
    n_top = 3L,
    start_state = start,
    k = k
  )

  # Old way: data.frame
  df_old <- analyze_top_combinations(combos, start, k)

  # New way: StateStore
  store <- create_state_store(6L)
  store_analyze_combos(store, combos, start, k, cycle_val = 1L)

  # Compare sizes
  expect_equal(state_store_size(store), nrow(df_old))

  # Compare actual states
  df_new <- store_to_dataframe(store)

  # V columns should match
  v_cols <- paste0("V", 1:6)
  for (vc in v_cols) {
    expect_equal(df_new[[vc]], df_old[[vc]], info = paste("Column", vc))
  }

  # Step and combo_number should match
  expect_equal(df_new$step, df_old$step)
  expect_equal(df_new$combo_number, df_old$combo_number)

  # Operations should match (convert NA handling)
  old_ops <- as.character(df_old$operation)
  new_ops <- as.character(df_new$operation)
  expect_equal(new_ops, old_ops)

  # Coordinates should match
  expect_equal(df_new$nL, as.integer(df_old$nL))
  expect_equal(df_new$nR, as.integer(df_old$nR))
  expect_equal(df_new$nX, as.integer(df_old$nX))
  expect_equal(df_new$theta, df_old$theta, tolerance = 1e-10)
  expect_equal(df_new$phi, df_old$phi, tolerance = 1e-10)
  expect_equal(df_new$omega_conformal, df_old$omega_conformal, tolerance = 1e-10)
})
