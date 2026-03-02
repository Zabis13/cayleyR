#' Iterative Path Finder Between Permutation States
#'
#' Finds a path between two permutation states using iterative cycle
#' expansion. Generates random operation sequences, analyzes their cycles,
#' and looks for intersections between forward (from start) and backward
#' (from final) state sets. Uses bridge states to progressively narrow
#' the search space.
#'
#' Uses a compact C++ StateStore backend for O(1) incremental hash-indexed
#' state storage, eliminating quadratic memory growth from repeated rbind.
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
#' @param sort_by Character vector of sorting criteria for combo selection.
#'   See \code{\link{find_best_random_combinations}} for details.
#'   Default: c("longest", "most_unique").
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
                                 sort_by = c("longest", "most_unique"),
                                 verbose = TRUE) {

  n <- length(start_state)

  start_state <- as.integer(start_state)
  names(start_state) <- NULL
  final_state <- as.integer(final_state)
  names(final_state) <- NULL

  if (identical(start_state, final_state)) {
    if (verbose) cat("Start and final states are identical, no path needed.\n")
    return(list(
      path = character(0),
      found = TRUE,
      cycles = 0L,
      selected_info = NULL,
      bridge_states_start = list(),
      bridge_states_final = list()
    ))
  }

  # Create C++ StateStores (replaces states_list_start/final + rbind + hash)
  store_start <- create_state_store(n)
  store_final <- create_state_store(n)

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

  start_key <- paste(start_state, collapse = "_")
  final_key <- paste(final_state, collapse = "_")

  if (verbose) {
    cat("\n=== Path search ===\n")
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

    # --- Generate combos and analyze directly into store ---
    .run_side_store <- function(store, current_state, cached_combos) {
      if (!reuse_combos || is.null(cached_combos)) {
        top_combos <- find_best_random_combinations(
          moves = moves, combo_length = combo_length, n_samples = n_samples,
          n_top = n_top, start_state = current_state, k = k,
          sort_by = sort_by
        )
        if (reuse_combos) cached_combos <- top_combos
      } else {
        top_combos <- cached_combos
      }

      count_before <- state_store_size(store)
      store_analyze_combos(store, top_combos, current_state, k, cycle_num)
      count_after <- state_store_size(store)
      n_added <- count_after - count_before

      list(cached_combos = cached_combos, n_added = n_added)
    }

    res_start <- .run_side_store(store_start, current_start, cached_combos_start)
    res_final <- .run_side_store(store_final, current_final, cached_combos_final)

    if (reuse_combos) {
      cached_combos_start <- res_start$cached_combos
      cached_combos_final <- res_final$cached_combos
    }

    # POTC: not directly applicable with store (states are already added)
    # potc filtering would need to be done before adding to store,
    # but the current C++ analyze writes directly. For now potc is handled
    # at the combo level (fewer combos = fewer states). TODO: add potc to C++.

    if (verbose) {
      cat("States start:", state_store_unique_count(store_start),
          "| final:", state_store_unique_count(store_final), "\n")
      flush.console()
    }

    # --- Find intersections via hash (O(min(N,M))) ---
    intersection_keys <- store_find_intersections(store_start, store_final)

    if (length(intersection_keys) > 0) {
      if (verbose) {
        cat("Found", length(intersection_keys), "intersections\n")
        flush.console()
      }

      n_to_process <- min(length(intersection_keys), ptr)
      sampled_keys <- intersection_keys[sample(length(intersection_keys), n_to_process)]

      candidate_paths <- list()
      processed_count <- 0

      for (ikey in sampled_keys) {
        # Parse key back to state
        intersection_state <- as.integer(strsplit(ikey, "_")[[1]])

        is_start <- (ikey == start_key)
        is_final <- (ikey == final_key)

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
          result <- .process_intersection_store(
            intersection_state, store_final, bridge_states_final, "final"
          )
        } else if (is_final) {
          intersection_type <- "FINAL"
          result <- .process_intersection_store(
            intersection_state, store_start, bridge_states_start, "start"
          )
        } else {
          intersection_type <- "INTERMEDIATE"
          result <- .process_intermediate_store(
            intersection_state, store_start, store_final,
            bridge_states_start, bridge_states_final
          )
        }

        processed_count <- processed_count + 1

        if (!is.null(result)) {
          candidate_paths[[length(candidate_paths) + 1]] <- list(
            path = result$path,
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

          bridge_result <- .select_new_bridges_store(
            store_start, store_final,
            cycle_num, current_start, current_final,
            bridge_states_start, bridge_states_final,
            opd, verbose,
            distance_method = distance_method
          )

          current_start <- bridge_result$current_start
          current_final <- bridge_result$current_final
          bridge_states_start <- bridge_result$bridge_states_start
          bridge_states_final <- bridge_result$bridge_states_final
        }
      }

    } else {
      if (verbose) {
        cat("No intersections found, continuing...\n")
        flush.console()
      }

      bridge_result <- .select_new_bridges_store(
        store_start, store_final,
        cycle_num, current_start, current_final,
        bridge_states_start, bridge_states_final,
        opd, verbose,
        distance_method = distance_method
      )

      current_start <- bridge_result$current_start
      current_final <- bridge_result$current_final
      bridge_states_start <- bridge_result$bridge_states_start
      bridge_states_final <- bridge_result$bridge_states_final
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


# --- Internal: process intersection using StateStore ---

# For START or FINAL type intersections (one-sided path reconstruction)
.process_intersection_store <- function(intersection_state, store, bridge_states, side) {
  indices <- store_lookup(store, intersection_state)
  if (length(indices) == 0) return(NULL)

  idx <- indices[1]
  meta <- store_get_meta(store, idx)

  path_full <- store_reconstruct_path(
    store, bridge_states, intersection_state,
    meta$cycle, meta$combo_number
  )
  if (is.null(path_full)) return(NULL)

  # For "final" side: path goes from final_state to intersection, invert for forward direction
  if (side == "final") {
    path_candidate <- invert_path(path_full)
  } else {
    path_candidate <- path_full
  }

  list(
    path = path_candidate,
    info = list(
      start_combo = if (side == "start") meta$combo_number else NA,
      start_step = if (side == "start") meta$step else NA,
      final_combo = if (side == "final") meta$combo_number else NA,
      final_step = if (side == "final") meta$step else NA
    )
  )
}

# For INTERMEDIATE intersections (both sides)
.process_intermediate_store <- function(intersection_state,
                                         store_start, store_final,
                                         bridge_states_start, bridge_states_final) {
  indices_start <- store_lookup(store_start, intersection_state)
  indices_final <- store_lookup(store_final, intersection_state)

  if (length(indices_start) == 0 || length(indices_final) == 0) return(NULL)

  meta_start <- store_get_meta(store_start, indices_start[1])
  meta_final <- store_get_meta(store_final, indices_final[1])

  path_start_full <- store_reconstruct_path(
    store_start, bridge_states_start, intersection_state,
    meta_start$cycle, meta_start$combo_number
  )
  path_final_full <- store_reconstruct_path(
    store_final, bridge_states_final, intersection_state,
    meta_final$cycle, meta_final$combo_number
  )

  if (is.null(path_start_full) || is.null(path_final_full)) return(NULL)

  path_final_inverted <- invert_path(path_final_full)
  path_candidate <- c(path_start_full, path_final_inverted)

  list(
    path = path_candidate,
    info = list(
      start_combo = meta_start$combo_number, start_step = meta_start$step,
      final_combo = meta_final$combo_number, final_step = meta_final$step
    )
  )
}


# --- Internal: bridge selection using StateStore ---

.select_new_bridges_store <- function(store_start, store_final,
                                       cycle_num, current_start, current_final,
                                       bridge_states_start, bridge_states_final,
                                       opd, verbose,
                                       distance_method = "manhattan") {

  n <- state_store_perm_length(store_start)

  # Filter middle states for current cycle
  start_filtered <- store_filter_middle(store_start, cycle_num, skip_first = 5L, skip_last = 5L)
  final_filtered <- store_filter_middle(store_final, cycle_num, skip_first = 5L, skip_last = 5L)

  # Fallback: if no middle states, use all states for this cycle
  if (length(start_filtered) == 0) {
    start_filtered <- state_store_indices_for_cycle(store_start, cycle_num)
  }
  if (length(final_filtered) == 0) {
    final_filtered <- state_store_indices_for_cycle(store_final, cycle_num)
  }

  # Find best match: start side closest to current_final
  best_start_idx <- store_find_best_match(store_start, current_final, start_filtered)
  new_start <- store_get_state(store_start, best_start_idx)
  new_start <- as.integer(new_start)
  names(new_start) <- NULL

  # Find best match: final side closest to new_start
  best_final_idx <- store_find_best_match(store_final, new_start, final_filtered)
  new_final <- store_get_state(store_final, best_final_idx)
  new_final <- as.integer(new_final)
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

  # Get coords for bridge states
  meta_start <- store_get_meta(store_start, best_start_idx)
  meta_final <- store_get_meta(store_final, best_final_idx)

  bridge_states_start[[length(bridge_states_start) + 1]] <- list(
    state = new_start,
    cycle = cycle_num,
    theta = meta_start$theta,
    phi = meta_start$phi,
    omega_conformal = meta_start$omega_conformal
  )

  bridge_states_final[[length(bridge_states_final) + 1]] <- list(
    state = new_final,
    cycle = cycle_num,
    theta = meta_final$theta,
    phi = meta_final$phi,
    omega_conformal = meta_final$omega_conformal
  )

  # OPD filtering: restrict cycle to only combos containing bridge state
  if (opd && cycle_num >= 1) {
    # START side
    combos_start <- store_combos_for_state(store_start, new_start, cycle_num)
    if (length(combos_start) > 0) {
      store_set_opd(store_start, cycle_num, combos_start)
      if (verbose) {
        filtered_count <- length(state_store_indices_for_cycle(store_start, cycle_num))
        cat("OPD: фильтрация START cycle", cycle_num, "до", filtered_count, "строк\n")
        flush.console()
      }
    }

    # FINAL side
    combos_final <- store_combos_for_state(store_final, new_final, cycle_num)
    if (length(combos_final) > 0) {
      store_set_opd(store_final, cycle_num, combos_final)
      if (verbose) {
        filtered_count <- length(state_store_indices_for_cycle(store_final, cycle_num))
        cat("OPD: фильтрация FINAL cycle", cycle_num, "до", filtered_count, "строк\n")
        flush.console()
      }
    }
  }

  list(
    current_start = new_start,
    current_final = new_final,
    bridge_states_start = bridge_states_start,
    bridge_states_final = bridge_states_final
  )
}


# --- Internal: print bridge states ---

.print_bridge_states <- function(bridge_states, side_label) {
  cat("\nBridge states (", side_label, "):\n", sep = "")
  for (i in seq_along(bridge_states)) {
    bs <- bridge_states[[i]]
    state_str <- paste(bs$state, collapse = " ")
    cycle_str <- if (!is.null(bs$cycle)) paste0("cycle ", bs$cycle) else ""
    label <- if (!is.null(bs$label)) paste0(" [", bs$label, "]") else ""
    cat("  [", i, "] ", state_str, " (", cycle_str, ")", label, "\n", sep = "")
  }
  flush.console()
}
