#' Shorten Path via Greedy BFS Hopping
#'
#' @param path Character vector of operations ("1"/"2"/"3" or "L"/"R"/"X")
#' @param start_state Integer vector, the starting permutation state
#' @param k Integer, parameter for reverse_prefix operation
#' @param n_hits Integer, number of path points to find in BFS cloud (default 5)
#' @return List with path (shortened), original_length, new_length, savings
#' @export
short_path_bfs <- function(path, start_state, k, n_hits = 5L) {
  mapping <- c("1" = "L", "2" = "R", "3" = "X")
  path_normalized <- ifelse(path %in% names(mapping), mapping[path], path)
  names(path_normalized) <- NULL

  result <- short_path_bfs_cpp(
    as.integer(start_state),
    as.character(path_normalized),
    as.integer(k),
    as.integer(n_hits)
  )

  back_mapping <- c("L" = "1", "R" = "2", "X" = "3")
  result$path <- unname(back_mapping[result$path])

  result
}
