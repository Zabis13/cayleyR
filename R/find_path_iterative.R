#' Iterative Path Finder Between Permutation States
#'
#' Finds a path between two permutation states using iterative cycle
#' expansion. Generates random operation sequences, analyzes their cycles,
#' and looks for intersections between forward (from start) and backward
#' (from final) state sets. Uses bridge states to progressively narrow
#' the search space.
#'
#' @param start_state Integer vector, the starting permutation state
#' @param final_state Integer vector, the target permutation state
#' @param k Integer, parameter for reverse operations
#' @param moves Character vector, allowed operations (default c("1", "2", "3"))
#' @param combo_length Integer, length of random operation sequences (default 20)
#' @param n_samples Integer, number of random sequences to test per iteration (default 200)
#' @param n_top Integer, number of top sequences to analyze fully (default 10)
#' @param max_iterations Integer, maximum number of search iterations (default 10)
#' @param potc Numeric in (0,1], fraction of cycle states to keep (default 1)
#' @param ptr Integer, max intersections to process per iteration (default 10)
#' @param opd Logical, if TRUE filters states to only combos containing bridge state (default FALSE)
#' @param reuse_combos Logical, if TRUE generates random combos only once per side
#'   (cycle 1) and reuses them in subsequent cycles. Saves time but reduces diversity (default FALSE)
#' @param distance_method Character, method for comparing states during bridge
#'   selection. One of "manhattan" (sum of absolute differences) or "breakpoints"
#'   (number of adjacency violations). Default "manhattan".
#' @param verbose Logical, if TRUE prints progress messages (default TRUE)
#' @return List containing:
#'   \item{path}{Character vector of operations, or NULL if not found}
#'   \item{found}{Logical, whether a path was found}
#'   \item{cycles}{Number of iterations used}
#'   \item{selected_info}{Details about the selected intersection}
#'   \item{bridge_states_start}{List of forward bridge states}
#'   \item{bridge_states_final}{List of backward bridge states}
#' @export
#' @examples
#' # Small example
#' set.seed(42)
#' start <- 1:6
#' final <- c(3L, 1L, 2L, 6L, 4L, 5L)
#' # result <- find_path_iterative(start, final, k = 3, max_iterations = 5)
find_path_iterative <- function(start_state,
                                 final_state,
                                 k,
                                 moves = c("1", "2", "3"),
                                 combo_length = 20,
                                 n_samples = 200,
                                 n_top = 10,
                                 max_iterations = 10,
                                 potc = 1,
                                 ptr = 10,
                                 opd = FALSE,
                                 reuse_combos = FALSE,
                                 distance_method = "manhattan",
                                 verbose = TRUE) {

  n <- length(start_state)
  v_cols <- paste0("V", 1:n)

  start_state <- as.integer(start_state)
  names(start_state) <- NULL
  final_state <- as.integer(final_state)
  names(final_state) <- NULL

  states_list_start <- list()
  states_list_final <- list()
  current_start <- start_state
  current_final <- final_state

  bridge_states_start <- list(list(state = start_state, cycle = 0))
  bridge_states_final <- list(list(state = final_state, cycle = 0))

  cycle_num <- 0
  path_found <- FALSE
  final_path <- NULL
  selected_info <- NULL

  # Cached combos for reuse_combos mode
  cached_combos_start <- NULL
  cached_combos_final <- NULL

  # Auto-detect GPU capabilities
  gpu_ok <- .setup_gpu()

  if (verbose) {
    cat("\n=== Path search ===\n")
    if (gpu_ok) {
      cat("GPU: available (Vulkan)\n")
    } else {
      cat("GPU: not available\n")
    }
    cat("OpenMP threads:", openmp_threads(), "\n")
    cat("Start:", paste(start_state, collapse = " "), "\n")
    cat("Final:", paste(final_state, collapse = " "), "\n")
    cat("max_iterations:", max_iterations, "\n")
    cat("potc:", potc, "\n")
    cat("opd:", opd, "\n")
    cat("reuse_combos:", reuse_combos, "\n")
    flush.console()
  }

  while (cycle_num < max_iterations && !path_found) {
    cycle_num <- cycle_num + 1
    if (verbose) {
      cat("\n--- Cycle", cycle_num, "---\n")
      flush.console()
    }

    # --- Generate and analyze combos for START and FINAL ---
    # Helper to run one side (find_best + analyze + potc)
    .run_side <- function(current_state, cached_combos, side_label) {
      if (!reuse_combos || is.null(cached_combos)) {
        top_combos <- find_best_random_combinations(
          moves = moves, combo_length = combo_length, n_samples = n_samples,
          n_top = n_top, start_state = current_state, k = k
        )
        if (reuse_combos) cached_combos <- top_combos
      } else {
        top_combos <- cached_combos
      }

      new_states <- analyze_top_combinations(top_combos, current_state, k)
      if (inherits(new_states, "ArrowTabular")) {
        new_states <- as.data.frame(new_states)
      }

      list(new_states = new_states, cached_combos = cached_combos)
    }

    res_start <- .run_side(current_start, cached_combos_start, "START")
    res_final <- .run_side(current_final, cached_combos_final, "FINAL")

    new_states_start <- res_start$new_states
    if (reuse_combos) cached_combos_start <- res_start$cached_combos
    new_states_final <- res_final$new_states
    if (reuse_combos) cached_combos_final <- res_final$cached_combos

    if (potc < 1) {
      n_original <- nrow(new_states_start)
      n_keep <- floor(n_original * potc)
      if (n_keep > 0) {
        new_states_start <- new_states_start[1:n_keep, ]
        if (verbose) {
          cat("POTC START: kept", n_keep, "of", n_original, "rows\n")
          flush.console()
        }
      }
    }

    new_states_start$cycle <- cycle_num
    states_list_start[[cycle_num]] <- new_states_start

    if (potc < 1) {
      n_original <- nrow(new_states_final)
      n_keep <- floor(n_original * potc)
      if (n_keep > 0) {
        new_states_final <- new_states_final[1:n_keep, ]
        if (verbose) {
          cat("POTC FINAL: kept", n_keep, "of", n_original, "rows\n")
          flush.console()
        }
      }
    }

    new_states_final$cycle <- cycle_num
    states_list_final[[cycle_num]] <- new_states_final

    reachable_states_start <- do.call(rbind, states_list_start)
    reachable_states_final <- do.call(rbind, states_list_final)

    reachable_states_start <- add_state_keys(reachable_states_start, new_states_start, v_cols)
    reachable_states_final <- add_state_keys(reachable_states_final, new_states_final, v_cols)

    start_index <- create_hash_index(reachable_states_start)
    final_index <- create_hash_index(reachable_states_final)

    start_unique <- select_unique(reachable_states_start)
    final_unique <- select_unique(reachable_states_final)

    if (verbose) {
      cat("States start:", nrow(start_unique), "| final:", nrow(final_unique), "\n")
      flush.console()
    }

    duplicates <- check_duplicates(start_unique, final_unique)

    if (!is.null(duplicates)) {
      unique_states <- unique(duplicates[, v_cols])
      if (verbose) {
        cat("Found", nrow(unique_states), "intersections\n")
        flush.console()
      }

      n_to_process <- min(nrow(unique_states), ptr)
      sampled_idx <- sample(nrow(unique_states), n_to_process)
      unique_states <- unique_states[sampled_idx, , drop = FALSE]

      start_key <- paste(start_state, collapse = "_")
      final_key <- paste(final_state, collapse = "_")

      is_intersections_start <- apply(unique_states, 1, function(row) {
        paste(row, collapse = "_") == start_key
      })
      is_intersections_final <- apply(unique_states, 1, function(row) {
        paste(row, collapse = "_") == final_key
      })

      candidate_paths <- list()
      processed_count <- 0

      for (dup_idx in 1:nrow(unique_states)) {
        intersection_state <- as.integer(unique_states[dup_idx, ])
        names(intersection_state) <- NULL

        is_start <- is_intersections_start[dup_idx]
        is_final <- is_intersections_final[dup_idx]

        result <- NULL
        intersection_type <- NULL

        if (is_start && is_final) {
          intersection_type <- "START==FINAL"
          result <- list(
            path = character(0),
            info = list(start_combo = NA, start_step = NA, final_combo = NA, final_step = NA)
          )
        } else if (is_start) {
          intersection_type <- "START"
          result <- process_start_intersection(
            intersection_state, reachable_states_final, bridge_states_final,
            final_index, v_cols
          )
        } else if (is_final) {
          intersection_type <- "FINAL"
          result <- process_final_intersection(
            intersection_state, reachable_states_start, bridge_states_start,
            start_index, v_cols
          )
        } else {
          intersection_type <- "INTERMEDIATE"
          result <- process_intermediate_intersection(
            intersection_state, reachable_states_start, reachable_states_final,
            bridge_states_start, bridge_states_final, start_index, final_index, v_cols
          )
        }

        processed_count <- processed_count + 1

        if (!is.null(result)) {
          candidate_paths[[length(candidate_paths) + 1]] <- list(
            path = result$path,
            intersection_idx = dup_idx,
            state = intersection_state,
            type = intersection_type,
            info = result$info,
            path_length = length(result$path)
          )
        }
      }

      if (length(candidate_paths) > 0) {
        validated_paths <- list()
        valid_count <- 0

        for (idx in seq_along(candidate_paths)) {
          candidate <- candidate_paths[[idx]]
          validation <- validate_and_simplify_path(candidate$path, start_state, final_state, k)

          if (validation$valid) {
            valid_count <- valid_count + 1
            validated_paths[[length(validated_paths) + 1]] <- list(
              path = validation$path,
              intersection_idx = candidate$intersection_idx,
              state = candidate$state,
              type = candidate$type,
              info = candidate$info
            )
          }
        }

        if (verbose) {
          cat("Processed:", processed_count, "| Valid:", valid_count, "\n")
          flush.console()
        }

        if (length(validated_paths) > 0) {
          validated_lengths <- vapply(validated_paths, function(x) length(x$path), integer(1))
          min_idx <- which.min(validated_lengths)
          selected <- validated_paths[[min_idx]]
          final_path <- selected$path
          selected_info <- selected

          if (verbose) {
            cat("\nFound", valid_count, "valid paths\n")
            cat("Selected path of length", length(final_path), "operations\n")
            flush.console()
          }

          path_found <- TRUE
        } else {
          if (verbose) {
            cat("No valid paths, continuing...\n")
            flush.console()
          }

          .select_new_bridges(
            reachable_states_start, reachable_states_final,
            cycle_num, v_cols, current_start, current_final,
            bridge_states_start, bridge_states_final,
            states_list_start, states_list_final,
            opd, verbose,
            moves = moves, k = k, n = n,
            combo_length = combo_length, n_samples = n_samples, n_top = n_top,
            distance_method = distance_method,
            use_gpu = gpu_ok
          ) -> bridge_result

          current_start <- bridge_result$current_start
          current_final <- bridge_result$current_final
          bridge_states_start <- bridge_result$bridge_states_start
          bridge_states_final <- bridge_result$bridge_states_final
          states_list_start <- bridge_result$states_list_start
          states_list_final <- bridge_result$states_list_final
        }
      }

    } else {
      if (verbose) {
        cat("No intersections found, continuing...\n")
        flush.console()
      }

      .select_new_bridges(
        reachable_states_start, reachable_states_final,
        cycle_num, v_cols, current_start, current_final,
        bridge_states_start, bridge_states_final,
        states_list_start, states_list_final,
        opd, verbose,
        moves = moves, k = k, n = n,
        combo_length = combo_length, n_samples = n_samples, n_top = n_top,
        distance_method = distance_method,
        use_gpu = gpu_ok
      ) -> bridge_result

      current_start <- bridge_result$current_start
      current_final <- bridge_result$current_final
      bridge_states_start <- bridge_result$bridge_states_start
      bridge_states_final <- bridge_result$bridge_states_final
      states_list_start <- bridge_result$states_list_start
      states_list_final <- bridge_result$states_list_final
    }
  }

  if (verbose) {
    cat("\n=== Result ===\n")
    flush.console()
  }

  if (path_found) {
    if (verbose) {
      cat("Path found in", cycle_num, "cycles\n")
      cat("Path length:", length(final_path), "operations\n")

      result_test <- apply_operations(start_state, final_path, k)
      test_state <- result_test$state
      if (identical(as.integer(test_state), final_state)) {
        cat("Verification passed\n")
      } else {
        cat("VERIFICATION FAILED\n")
      }

      cat("\nPath:\n")
      cat(paste(final_path, collapse = " "), "\n")
      flush.console()
    }
  } else {
    if (verbose) {
      cat("Path not found in", max_iterations, "cycles\n")
      flush.console()
    }
  }

  return(list(
    path = final_path,
    found = path_found,
    cycles = cycle_num,
    selected_info = selected_info,
    bridge_states_start = bridge_states_start,
    bridge_states_final = bridge_states_final
  ))
}


# Internal helper for bridge state selection
.select_new_bridges <- function(reachable_states_start, reachable_states_final,
                                 cycle_num, v_cols, current_start, current_final,
                                 bridge_states_start, bridge_states_final,
                                 states_list_start, states_list_final,
                                 opd, verbose,
                                 moves = c("1", "2", "3"), k = NULL, n = NULL,
                                 combo_length = 20, n_samples = 200, n_top = 10,
                                 distance_method = "manhattan",
                                 use_gpu = FALSE) {

  start_all_current <- reachable_states_start[reachable_states_start$cycle == cycle_num, ]
  final_all_current <- reachable_states_final[reachable_states_final$cycle == cycle_num, ]

  start_filtered <- filter_middle_states(start_all_current, skip_first = 5, skip_last = 5)
  final_filtered <- filter_middle_states(final_all_current, skip_first = 5, skip_last = 5)

  if (nrow(start_filtered) == 0) start_filtered <- start_all_current
  if (nrow(final_filtered) == 0) final_filtered <- final_all_current

  # Bridge selection: pick best match by distance_method (no anti-loop)
  new_start_row <- find_best_match_state(current_final, start_filtered,
                                          method = distance_method, use_gpu = use_gpu)
  new_start <- as.integer(new_start_row[, v_cols])
  names(new_start) <- NULL

  new_final_row <- find_best_match_state(new_start, final_filtered,
                                          method = distance_method, use_gpu = use_gpu)
  new_final <- as.integer(new_final_row[, v_cols])
  names(new_final) <- NULL

  bridge_dist <- switch(
    distance_method,
    "manhattan" = manhattan_distance(new_start, new_final),
    "breakpoints" = breakpoint_distance(new_start, new_final)
  )
  if (verbose) {
    cat("Bridge", distance_method, "distance:", bridge_dist, "\n")
    flush.console()
  }

  bridge_states_start[[length(bridge_states_start) + 1]] <- list(
    state = new_start,
    cycle = cycle_num,
    theta = if ("theta" %in% colnames(new_start_row)) new_start_row$theta else NA,
    phi = if ("phi" %in% colnames(new_start_row)) new_start_row$phi else NA,
    omega_conformal = if ("omega_conformal" %in% colnames(new_start_row)) new_start_row$omega_conformal else NA
  )

  bridge_states_final[[length(bridge_states_final) + 1]] <- list(
    state = new_final,
    cycle = cycle_num,
    theta = if ("theta" %in% colnames(new_final_row)) new_final_row$theta else NA,
    phi = if ("phi" %in% colnames(new_final_row)) new_final_row$phi else NA,
    omega_conformal = if ("omega_conformal" %in% colnames(new_final_row)) new_final_row$omega_conformal else NA
  )

  if (opd && cycle_num >= 1) {
    if (!is.null(states_list_start[[cycle_num]])) {
      cycle_data <- states_list_start[[cycle_num]]
      state_keys <- apply(cycle_data[, v_cols, drop = FALSE], 1, function(r) paste(r, collapse = "_"))
      bridge_new_key <- paste(new_start, collapse = "_")

      combos_with_bridge <- unique(cycle_data$combo_number[state_keys == bridge_new_key])

      if (length(combos_with_bridge) > 0) {
        states_list_start[[cycle_num]] <- cycle_data[cycle_data$combo_number %in% combos_with_bridge, ]
        if (verbose) {
          cat("OPD: filtered START cycle", cycle_num, "to",
              nrow(states_list_start[[cycle_num]]), "rows\n")
          flush.console()
        }
      }
    }

    if (!is.null(states_list_final[[cycle_num]])) {
      cycle_data <- states_list_final[[cycle_num]]
      state_keys <- apply(cycle_data[, v_cols, drop = FALSE], 1, function(r) paste(r, collapse = "_"))
      bridge_new_key <- paste(new_final, collapse = "_")

      combos_with_bridge <- unique(cycle_data$combo_number[state_keys == bridge_new_key])

      if (length(combos_with_bridge) > 0) {
        states_list_final[[cycle_num]] <- cycle_data[cycle_data$combo_number %in% combos_with_bridge, ]
        if (verbose) {
          cat("OPD: filtered FINAL cycle", cycle_num, "to",
              nrow(states_list_final[[cycle_num]]), "rows\n")
          flush.console()
        }
      }
    }
  }

  list(
    current_start = new_start,
    current_final = new_final,
    bridge_states_start = bridge_states_start,
    bridge_states_final = bridge_states_final,
    states_list_start = states_list_start,
    states_list_final = states_list_final,
    new_start_cycle = new_start_row$cycle,
    new_start_combo = new_start_row$combo_number,
    new_final_cycle = new_final_row$cycle,
    new_final_combo = new_final_row$combo_number
  )
}
