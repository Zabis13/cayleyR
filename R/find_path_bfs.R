#' Find Path via BFS Highways
#'
#' Builds BFS highway trees from start and final states, finds the closest
#' pair of hub states (one from each highway), then uses find_path_iterative
#' to connect them. Assembles the full path:
#' bfs(start -> hub_s) + iterative(hub_s -> hub_f) + inverted_bfs(final -> hub_f)
#'
#' @param start_state Integer vector, the starting permutation state
#' @param final_state Integer vector, the target permutation state
#' @param k Integer, parameter for reverse operations
#' @param bfs_levels Integer, depth of sparse BFS from each side (default 500)
#' @param bfs_n_hubs Integer, top-degree nodes per BFS level (default 7)
#' @param bfs_n_random Integer, random nodes per BFS level (default 3)
#' @param distance_method Character, "manhattan" or "breakpoints" (default "manhattan")
#' @param verbose Logical, print progress (default TRUE)
#' @param ... Additional arguments passed to find_path_iterative
#' @return List with path, found, cycles, bfs_info
#' @export
find_path_bfs <- function(start_state, final_state, k,
                          bfs_levels = 500L,
                          bfs_n_hubs = 7L,
                          bfs_n_random = 3L,
                          distance_method = "manhattan",
                          verbose = TRUE,
                          ...) {

  start_state <- as.integer(start_state)
  final_state <- as.integer(final_state)
  names(start_state) <- NULL
  names(final_state) <- NULL

  # --- Step 1: Build BFS highways ---
  if (verbose) {
    cat("\n=== find_path_bfs ===\n")
    cat("Step 1: Building BFS highways (levels =", bfs_levels, ")\n")
    flush.console()
  }

  bfs_start <- sparse_bfs_cpp(start_state, k, bfs_n_hubs, bfs_n_random, bfs_levels)
  bfs_final <- sparse_bfs_cpp(final_state, k, bfs_n_hubs, bfs_n_random, bfs_levels)

  if (verbose) {
    cat("  BFS start:", nrow(bfs_start), "edges\n")
    cat("  BFS final:", nrow(bfs_final), "edges\n")
    flush.console()
  }

  # --- Step 2: Find closest hub pair (manhattan) ---
  if (verbose) {
    cat("Step 2: Finding closest hub pair\n")
    flush.console()
  }

  start_keys <- unique(bfs_start$child_key)
  final_keys <- unique(bfs_final$child_key)

  # Parse keys to matrices
  start_mat <- do.call(rbind, lapply(strsplit(start_keys, "_"), as.integer))
  final_mat <- do.call(rbind, lapply(strsplit(final_keys, "_"), as.integer))

  # Find closest pair
  # Sample if too many for speed
  max_compare <- 500L
  s_idx <- if (nrow(start_mat) > max_compare) sample.int(nrow(start_mat), max_compare) else seq_len(nrow(start_mat))
  f_idx <- if (nrow(final_mat) > max_compare) sample.int(nrow(final_mat), max_compare) else seq_len(nrow(final_mat))

  gpu_ok <- distance_method == "manhattan" &&
    tryCatch(cayley_gpu_available(), error = function(e) FALSE)

  if (gpu_ok) {
    # GPU path: compute full pairwise manhattan distance matrix at once
    tryCatch({
      .setup_gpu()
      dist_mat <- manhattan_distance_matrix_gpu(start_mat[s_idx, , drop = FALSE],
                                                 final_mat[f_idx, , drop = FALSE])
      min_pos <- which.min(dist_mat)
      # Convert linear index to row/col (matrix is nrow=length(s_idx), ncol=length(f_idx))
      nr <- length(s_idx)
      min_row <- ((min_pos - 1L) %% nr) + 1L
      min_col <- ((min_pos - 1L) %/% nr) + 1L
      best_s_idx <- s_idx[min_row]
      best_f_idx <- f_idx[min_col]
      best_dist <- dist_mat[min_row, min_col]
    }, error = function(e) {
      gpu_ok <<- FALSE
    })
  }

  if (!gpu_ok) {
    # CPU fallback
    distance_func <- switch(
      distance_method,
      "manhattan" = function(v1, v2) sum(abs(v1 - v2)),
      "breakpoints" = function(v1, v2) breakpoint_distance(v1, v2),
      stop("Unsupported method: ", distance_method)
    )

    best_dist <- Inf
    best_s_idx <- 1L
    best_f_idx <- 1L

    for (si in s_idx) {
      for (fi in f_idx) {
        d <- distance_func(start_mat[si, ], final_mat[fi, ])
        if (d < best_dist) {
          best_dist <- d
          best_s_idx <- si
          best_f_idx <- fi
        }
      }
    }
  }

  hub_s <- start_mat[best_s_idx, ]
  hub_f <- final_mat[best_f_idx, ]
  hub_s_key <- start_keys[best_s_idx]
  hub_f_key <- final_keys[best_f_idx]

  if (verbose) {
    cat("  Hub start:", hub_s_key, "\n")
    cat("  Hub final:", hub_f_key, "\n")
    cat("  Distance:", best_dist, "\n")
    flush.console()
  }

  # --- Step 3: Check if hubs are the same ---
  if (hub_s_key == hub_f_key) {
    if (verbose) {
      cat("  Hubs match! Direct BFS connection.\n")
      flush.console()
    }
    path_fwd <- .bfs_ops_to_digits(reconstruct_bfs_path(bfs_start, hub_s_key))
    path_bwd <- invert_path(.bfs_ops_to_digits(reconstruct_bfs_path(bfs_final, hub_f_key)))
    full_path <- c(path_fwd, path_bwd)

    validation <- validate_and_simplify_path(full_path, start_state, final_state, k)
    if (validation$valid) {
      if (verbose) {
        cat("  Path validated! Length:", length(validation$path), "\n")
      }
      return(list(
        path = validation$path, found = TRUE, cycles = 0,
        bfs_info = list(type = "BFS_DIRECT", hub_s = hub_s_key, hub_f = hub_f_key, distance = 0)
      ))
    }
  }

  # --- Step 4: Run find_path_iterative between hubs ---
  if (verbose) {
    cat("Step 3: Running find_path_iterative between hubs\n")
    flush.console()
  }

  mid_result <- find_path_iterative(
    start_state = as.integer(hub_s),
    final_state = as.integer(hub_f),
    k = k,
    verbose = verbose,
    ...
  )

  if (!mid_result$found) {
    if (verbose) cat("  Middle path not found.\n")
    return(list(
      path = NULL, found = FALSE, cycles = mid_result$cycles,
      bfs_info = list(type = "FAILED", hub_s = hub_s_key, hub_f = hub_f_key, distance = best_dist)
    ))
  }

  # --- Step 5: Assemble full path ---
  if (verbose) {
    cat("Step 4: Assembling full path\n")
    flush.console()
  }

  # BFS path: start -> hub_s
  start_key_root <- paste(start_state, collapse = "_")
  if (hub_s_key == start_key_root) {
    path_start_to_hub <- character(0)
  } else {
    path_start_to_hub <- .bfs_ops_to_digits(reconstruct_bfs_path(bfs_start, hub_s_key))
  }

  # BFS path: final -> hub_f (inverted = hub_f -> final)
  final_key_root <- paste(final_state, collapse = "_")
  if (hub_f_key == final_key_root) {
    path_hub_to_final <- character(0)
  } else {
    path_hub_to_final <- invert_path(.bfs_ops_to_digits(reconstruct_bfs_path(bfs_final, hub_f_key)))
  }

  full_path <- c(path_start_to_hub, mid_result$path, path_hub_to_final)

  validation <- validate_and_simplify_path(full_path, start_state, final_state, k)

  if (validation$valid) {
    if (verbose) {
      cat("\n=== Result ===\n")
      cat("Full path length:", length(validation$path), "\n")
      cat("  BFS start->hub:", length(path_start_to_hub), "\n")
      cat("  Iterative hub->hub:", length(mid_result$path), "\n")
      cat("  BFS hub->final:", length(path_hub_to_final), "\n")
      cat("Verification passed\n")
      flush.console()
    }

    final_result_path <- validation$path

    if (verbose) {
      cat("\nPath:\n")
      cat(paste(final_result_path, collapse = " "), "\n")
      flush.console()
    }

    return(list(
      path = final_result_path, found = TRUE, cycles = mid_result$cycles,
      bfs_info = list(
        type = "BFS_HIGHWAY", hub_s = hub_s_key, hub_f = hub_f_key,
        distance = best_dist,
        bfs_start_len = length(path_start_to_hub),
        mid_len = length(mid_result$path),
        bfs_final_len = length(path_hub_to_final)
      )
    ))
  } else {
    if (verbose) cat("VERIFICATION FAILED\n")
    return(list(
      path = full_path, found = FALSE, cycles = mid_result$cycles,
      bfs_info = list(type = "VERIFICATION_FAILED")
    ))
  }
}

# Convert BFS ops (L/R/X) to digit format (1/2/3)
.bfs_ops_to_digits <- function(ops) {
  mapping <- c("L" = "1", "R" = "2", "X" = "3")
  unname(mapping[ops])
}
