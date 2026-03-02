#' Create a New State Store
#'
#' Creates a C++ StateStore object for compact, incremental storage of
#' permutation states with hash-indexed lookup.
#'
#' @param perm_length Integer, length of each permutation state
#' @param init_capacity Integer, initial capacity (default 10000)
#' @return External pointer to StateStore (XPtr)
#' @export
#' @examples
#' store <- create_state_store(6L)
#' state_store_size(store)
create_state_store <- function(perm_length, init_capacity = 10000L) {
  state_store_create(as.integer(perm_length), as.integer(init_capacity))
}

#' Get State Store Size
#'
#' Returns the number of states currently stored in the StateStore.
#'
#' @name state_store_size
#' @param xp External pointer to StateStore
#' @return Integer, number of stored states
#' @export
#' @examples
#' store <- create_state_store(6L)
#' state_store_size(store)
NULL

#' Add States to Store from Data Frame
#'
#' Converts a data.frame/data.table of states (as returned by
#' \code{analyze_top_combinations}) into the compact C++ store.
#'
#' @param store External pointer to StateStore
#' @param df Data frame with V1..Vn columns plus metadata
#' @param cycle_val Integer, cycle number to assign
#' @return Number of rows added (invisible)
#' @export
store_add_from_df <- function(store, df, cycle_val) {
  n <- state_store_perm_length(store)
  v_cols <- paste0("V", 1:n)

  states_mat <- as.matrix(df[, v_cols, drop = FALSE])
  storage.mode(states_mat) <- "integer"

  n_rows <- nrow(df)

  # Operation: convert string to int code
  if ("operation" %in% names(df)) {
    op_int <- integer(n_rows)
    ops <- as.character(df$operation)
    op_int[ops == "1" | ops == "L"] <- 1L
    op_int[ops == "2" | ops == "R"] <- 2L
    op_int[ops == "3" | ops == "X"] <- 3L
    # NA or empty → 0
    op_int[is.na(ops) | ops == ""] <- 0L
  } else {
    op_int <- integer(n_rows) # all 0
  }

  step_vec <- if ("step" %in% names(df)) as.integer(df$step) else rep(NA_integer_, n_rows)
  combo_vec <- if ("combo_number" %in% names(df)) as.integer(df$combo_number) else integer(n_rows)

  nL_vec <- if ("nL" %in% names(df)) as.integer(df$nL) else integer(n_rows)
  nR_vec <- if ("nR" %in% names(df)) as.integer(df$nR) else integer(n_rows)
  nX_vec <- if ("nX" %in% names(df)) as.integer(df$nX) else integer(n_rows)
  theta_vec <- if ("theta" %in% names(df)) as.numeric(df$theta) else numeric(n_rows)
  phi_vec <- if ("phi" %in% names(df)) as.numeric(df$phi) else numeric(n_rows)
  omega_vec <- if ("omega_conformal" %in% names(df)) as.numeric(df$omega_conformal) else numeric(n_rows)

  invisible(state_store_add_batch(
    store, states_mat,
    step_vec, combo_vec, as.integer(cycle_val), op_int,
    nL_vec, nR_vec, nX_vec,
    theta_vec, phi_vec, omega_vec
  ))
}

#' Get State from Store
#'
#' Retrieves a single permutation state by 0-based index.
#'
#' @param store External pointer to StateStore
#' @param idx Integer, 0-based index
#' @return Integer vector of length perm_length
#' @export
store_get_state <- function(store, idx) {
  state_store_get_state(store, as.integer(idx))
}

#' Get Metadata for a State
#'
#' Retrieves metadata (step, combo_number, cycle, operation, coordinates)
#' for a single state by 0-based index.
#'
#' @param store External pointer to StateStore
#' @param idx Integer, 0-based index
#' @return Named list
#' @export
store_get_meta <- function(store, idx) {
  state_store_get_meta(store, as.integer(idx))
}

#' Find Intersections Between Two Stores
#'
#' Returns state keys present in both stores. O(min(N,M)) via hash lookup.
#'
#' @param store_a External pointer to StateStore
#' @param store_b External pointer to StateStore
#' @return Character vector of common state keys
#' @export
store_find_intersections <- function(store_a, store_b) {
  state_store_find_intersections(store_a, store_b)
}

#' Lookup State Indices by State Vector
#'
#' @param store External pointer to StateStore
#' @param state Integer vector
#' @return Integer vector of 0-based indices
#' @export
store_lookup <- function(store, state) {
  state_store_lookup_state(store, as.integer(state))
}

#' Find Best Match by Manhattan Distance
#'
#' @param store External pointer to StateStore
#' @param target Integer vector, target state
#' @param candidate_indices Integer vector of 0-based indices to search
#'   (empty = search all)
#' @return Integer, 0-based index of best match
#' @export
store_find_best_match <- function(store, target, candidate_indices = integer(0)) {
  state_store_find_best_match(store, as.integer(target), as.integer(candidate_indices))
}

#' Filter Middle States for a Cycle
#'
#' Returns 0-based indices of states in the given cycle, excluding
#' the first skip_first and last skip_last steps per combo.
#'
#' @param store External pointer to StateStore
#' @param target_cycle Integer
#' @param skip_first Integer (default 5)
#' @param skip_last Integer (default 5)
#' @return Integer vector of 0-based indices
#' @export
store_filter_middle <- function(store, target_cycle, skip_first = 5L, skip_last = 5L) {
  state_store_filter_middle(store, as.integer(target_cycle),
                             as.integer(skip_first), as.integer(skip_last))
}

#' Set OPD Combo Filter for a Cycle
#'
#' Restricts indices_for_cycle and filter_middle_indices to only return
#' states from the specified combo_numbers for the given cycle.
#'
#' @param store External pointer to StateStore
#' @param target_cycle Integer, cycle number to filter
#' @param combos Integer vector of allowed combo_numbers
#' @export
store_set_opd <- function(store, target_cycle, combos) {
  state_store_set_opd(store, as.integer(target_cycle), as.integer(combos))
}

#' Clear All OPD Filters
#'
#' @param store External pointer to StateStore
#' @export
store_clear_opd <- function(store) {
  state_store_clear_opd(store)
}

#' Find Combo Numbers Containing a State in a Cycle
#'
#' @param store External pointer to StateStore
#' @param state Integer vector
#' @param target_cycle Integer
#' @return Integer vector of combo_numbers
#' @export
store_combos_for_state <- function(store, state, target_cycle) {
  state_store_combos_for_state(store, as.integer(state), as.integer(target_cycle))
}

#' Convert Store to Data Frame
#'
#' For debugging and backward compatibility. Converts the entire store
#' contents to a data.frame with V1..Vn + metadata columns.
#'
#' @param store External pointer to StateStore
#' @return data.frame
#' @export
store_to_dataframe <- function(store) {
  state_store_to_dataframe(store)
}

#' Reconstruct Path from Store
#'
#' Traces back through cycle chain using bridge states to build
#' the correct operation sequence. Each bridge state defines which
#' combo path to follow in each cycle.
#'
#' @param store External pointer to StateStore
#' @param bridge_states List of bridge state entries, each with \code{$state}
#'   (integer vector). Element 1 = root (cycle 0), element i+1 = bridge at cycle i.
#' @param target_state Integer vector
#' @param target_cycle Integer
#' @param target_combo Integer
#' @return Character vector of operations, or NULL
#' @export
store_reconstruct_path <- function(store, bridge_states, target_state,
                                    target_cycle, target_combo) {
  # Build bridge states matrix: n_bridges x L
  n_bridges <- length(bridge_states)
  L <- state_store_perm_length(store)
  bridge_mat <- matrix(0L, nrow = n_bridges, ncol = L)
  for (i in seq_along(bridge_states)) {
    bridge_mat[i, ] <- as.integer(bridge_states[[i]]$state)
  }
  storage.mode(bridge_mat) <- "integer"

  result <- state_store_reconstruct_path(
    store,
    bridge_mat,
    as.integer(target_state),
    as.integer(target_cycle),
    as.integer(target_combo)
  )
  if (is.null(result)) return(NULL)
  as.character(result)
}

#' Analyze Combinations Directly into Store
#'
#' C++ implementation that runs full cycle expansion for each combination
#' and writes states + coordinates directly into the StateStore,
#' bypassing data.frame creation entirely.
#'
#' @param store External pointer to StateStore
#' @param top_combos Data frame with \code{combination} column
#' @param start_state Integer vector, the initial permutation state
#' @param k Integer, parameter for reverse operations
#' @param cycle_val Integer, cycle number to assign
#' @return Number of states added (invisible)
#' @export
store_analyze_combos <- function(store, top_combos, start_state, k, cycle_val) {
  combinations <- as.character(top_combos$combination)
  invisible(analyze_combos_to_store_cpp(
    store, combinations, as.integer(start_state), as.integer(k), as.integer(cycle_val)
  ))
}

#' Analyze Combinations into Store Using GPU Batch Operations
#'
#' GPU-accelerated version of \code{store_analyze_combos}. Processes all
#' combinations in parallel, step by step, grouping by operation type (L/R/X)
#' for GPU matrix multiplication. Falls back to CPU if GPU is unavailable.
#'
#' @param store External pointer to StateStore
#' @param top_combos Data frame with \code{combination} column
#' @param start_state Integer vector, the initial permutation state
#' @param k Integer, parameter for reverse operations
#' @param cycle_val Integer, cycle number to assign
#' @return Number of states added (invisible)
#' @export
store_analyze_combos_gpu <- function(store, top_combos, start_state, k, cycle_val) {
  start_state <- as.integer(start_state)
  k <- as.integer(k)
  cycle_val <- as.integer(cycle_val)
  n <- length(start_state)

  combinations <- as.character(top_combos$combination)
  n_combos <- length(combinations)

  # Parse combo strings into lists of operation characters
  ops_list <- strsplit(combinations, "")
  combo_lengths <- vapply(ops_list, length, integer(1))

  # --- Row 0: start_state for each combo, step=1, op=first op ---
  # Build first-op codes as integer (1=L, 2=R, 3=X)
  first_ops_chr <- vapply(ops_list, `[`, character(1), 1L)
  first_ops_int <- match(first_ops_chr, c("1", "2", "3"))

  init_mat <- matrix(rep(start_state, each = n_combos), nrow = n_combos, ncol = n)
  storage.mode(init_mat) <- "integer"

  state_store_add_batch(
    store, init_mat,
    step_vec     = rep(1L, n_combos),
    combo_vec    = seq_len(n_combos),
    cycle_val    = cycle_val,
    op_vec       = as.integer(first_ops_int),
    nL_vec       = integer(n_combos),
    nR_vec       = integer(n_combos),
    nX_vec       = integer(n_combos),
    theta_vec    = numeric(n_combos),
    phi_vec      = numeric(n_combos),
    omega_vec    = numeric(n_combos)
  )

  # --- Initialize tracking vectors ---
  current_states <- init_mat  # n_combos x n matrix
  step_in_combo <- rep(1L, n_combos)  # 1-based position in ops string
  global_step   <- rep(1L, n_combos)  # row counter (already wrote step=1)
  nL <- integer(n_combos)
  nR <- integer(n_combos)
  nX <- integer(n_combos)
  alive <- rep(TRUE, n_combos)

  # Pre-build permutation matrices for each op type (reuse across steps)
  perm_matrices <- list()
  for (op in c("1", "2", "3")) {
    perm_matrices[[op]] <- matrix(build_permutation_matrix(op, n, k), nrow = n, ncol = n)
  }

  # --- Main loop: step by step until all combos return to start_state ---
  while (any(alive)) {
    alive_idx <- which(alive)

    # Current operation for each alive combo
    current_ops <- character(length(alive_idx))
    for (j in seq_along(alive_idx)) {
      i <- alive_idx[j]
      current_ops[j] <- ops_list[[i]][step_in_combo[i]]
    }

    # Apply operations grouped by type (GPU batch per group)
    for (op in unique(current_ops)) {
      mask <- (current_ops == op)
      sub_idx <- alive_idx[mask]
      sub_states <- current_states[sub_idx, , drop = FALSE]
      storage.mode(sub_states) <- "numeric"

      new_sub <- apply_operations_batch_gpu(sub_states, op, k)
      current_states[sub_idx, ] <- round(new_sub)
    }
    storage.mode(current_states) <- "integer"

    # Update coordinates (vectorized CPU)
    is_L <- (current_ops == "1")
    is_R <- (current_ops == "2")
    is_X <- (current_ops == "3")
    nL[alive_idx] <- nL[alive_idx] + as.integer(is_L)
    nR[alive_idx] <- nR[alive_idx] + as.integer(is_R)
    nX[alive_idx] <- nX[alive_idx] + as.integer(is_X)

    r <- sqrt(nL[alive_idx]^2 + nR[alive_idx]^2 + nX[alive_idx]^2)
    theta <- ifelse(r > 1e-10, acos(nX[alive_idx] / r), 0)
    phi <- atan2(nR[alive_idx], nL[alive_idx])
    omega <- r

    # Advance step counters
    global_step[alive_idx] <- global_step[alive_idx] + 1L
    step_in_combo[alive_idx] <- (step_in_combo[alive_idx] %% combo_lengths[alive_idx]) + 1L

    # Check completion: state == start_state
    just_finished <- logical(length(alive_idx))
    for (j in seq_along(alive_idx)) {
      i <- alive_idx[j]
      if (identical(current_states[i, ], start_state)) {
        just_finished[j] <- TRUE
        alive[i] <- FALSE
      }
    }

    still_alive_mask <- !just_finished
    finished_mask    <- just_finished

    # --- Write still-alive states: step=global_step, op=next_op ---
    if (any(still_alive_mask)) {
      sa_idx <- alive_idx[still_alive_mask]
      sa_n <- length(sa_idx)

      # Next op for still-alive combos
      next_ops_chr <- character(sa_n)
      for (j in seq_along(sa_idx)) {
        i <- sa_idx[j]
        next_ops_chr[j] <- ops_list[[i]][step_in_combo[i]]
      }
      next_ops_int <- match(next_ops_chr, c("1", "2", "3"))

      sa_mat <- current_states[sa_idx, , drop = FALSE]
      storage.mode(sa_mat) <- "integer"

      # Map from alive_idx positions to sa positions
      sa_alive_pos <- which(still_alive_mask)

      state_store_add_batch(
        store, sa_mat,
        step_vec     = global_step[sa_idx],
        combo_vec    = sa_idx,
        cycle_val    = cycle_val,
        op_vec       = as.integer(next_ops_int),
        nL_vec       = nL[sa_idx],
        nR_vec       = nR[sa_idx],
        nX_vec       = nX[sa_idx],
        theta_vec    = theta[sa_alive_pos],
        phi_vec      = phi[sa_alive_pos],
        omega_vec    = omega[sa_alive_pos]
      )
    }

    # --- Write finished states: step=NA, op=NA (0) ---
    if (any(finished_mask)) {
      fin_idx <- alive_idx[finished_mask]
      fin_n <- length(fin_idx)

      fin_mat <- current_states[fin_idx, , drop = FALSE]
      storage.mode(fin_mat) <- "integer"

      fin_alive_pos <- which(finished_mask)

      state_store_add_batch(
        store, fin_mat,
        step_vec     = rep(NA_integer_, fin_n),
        combo_vec    = fin_idx,
        cycle_val    = cycle_val,
        op_vec       = integer(fin_n),  # 0 = NA
        nL_vec       = nL[fin_idx],
        nR_vec       = nR[fin_idx],
        nX_vec       = nX[fin_idx],
        theta_vec    = theta[fin_alive_pos],
        phi_vec      = phi[fin_alive_pos],
        omega_vec    = omega[fin_alive_pos]
      )
    }
  }

  invisible(state_store_size(store))
}
