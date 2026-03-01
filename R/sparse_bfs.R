#' Sparse BFS with Look-ahead and Hybrid Selection
#'
#' @param start_state Integer vector — starting permutation
#' @param k Integer — parameter for reverse_prefix operation
#' @param n_hubs Number of top-degree candidates to keep per level (exploitation)
#' @param n_random Number of random candidates to keep per level (exploration)
#' @param max_levels Maximum BFS depth (default 1000)
#' @return data.frame with columns: parent_key, child_key, operation, level
#' @export
sparse_bfs <- function(start_state, k, n_hubs = 7L, n_random = 3L,
                       max_levels = 1000L) {
  sparse_bfs_cpp(
    as.integer(start_state),
    as.integer(k),
    as.integer(n_hubs),
    as.integer(n_random),
    as.integer(max_levels)
  )
}

#' Reconstruct path from sparse BFS result
#'
#' Traces back from target_key to the root (start state) using the
#' parent_key/child_key edges in the BFS result.
#'
#' @param bfs_result data.frame returned by sparse_bfs()
#' @param target_key Character string — state key to trace back from
#' @return Character vector of operations from start to target
#' @export
reconstruct_bfs_path <- function(bfs_result, target_key) {
  # Build lookup: child_key -> row index
  idx <- match(target_key, bfs_result$child_key)
  if (is.na(idx)) {
    stop("target_key not found in BFS result")
  }

  ops <- character(0)
  current <- target_key

  while (TRUE) {
    row_idx <- match(current, bfs_result$child_key)
    if (is.na(row_idx)) break
    ops <- c(bfs_result$operation[row_idx], ops)
    current <- bfs_result$parent_key[row_idx]
  }

  ops
}
