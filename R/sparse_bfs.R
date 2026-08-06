#' Sparse BFS with Look-ahead and Hybrid Selection
#'
#' Builds a sparse "highway" through the state space: at each level only a few
#' candidates are kept, the highest-degree ones plus a random sample, so the
#' search reaches deep without materialising the whole frontier.
#'
#' Runs over any \code{\link{perm_group}}. Operations are reported under the
#' group's own move names, so a cube highway is labelled with face turns.
#'
#' @param start_state Integer vector — starting permutation
#' @param k Integer — parameter for reverse_prefix operation. Ignored when
#'   `group` is given.
#' @param n_hubs Number of top-degree candidates to keep per level (exploitation)
#' @param n_random Number of random candidates to keep per level (exploration)
#' @param max_levels Maximum BFS depth (default 1000)
#' @param moves Allowed operations, naming a subset of the group's alphabet
#'   (default: all of it)
#' @param group A \code{\link{perm_group}}. When `NULL` (default) the arguments
#'   describe TopSpin.
#' @return data.frame with columns: parent_key, child_key, operation, level
#' @export
#' @seealso \code{\link{perm_group}}, \code{\link{reconstruct_bfs_path}}
#' @examples
#' set.seed(1)
#' head(sparse_bfs(1:10, k = 4, max_levels = 5))
#'
#' # the same highway through the cube
#' g <- cube_group(3)
#' head(sparse_bfs(group_identity(g), group = g, max_levels = 3))
sparse_bfs <- function(start_state, k = NULL, n_hubs = 7L, n_random = 3L,
                       max_levels = 1000L, moves = NULL, group = NULL) {
  start_state <- as.integer(start_state)
  res <- resolve_group(group, length(start_state), k, moves)
  sparse_bfs_cpp(
    start_state,
    res$group$ptr,
    res$moves,
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
