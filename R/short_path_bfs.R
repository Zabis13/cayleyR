#' Shorten Path via Depth-Limited BFS Hopping
#'
#' For each position along the path, explores all reachable states within
#' \code{depth} BFS steps. If any of those states appear later in the original
#' path (beyond current position + BFS steps taken), the algorithm "jumps"
#' to the farthest such match, replacing the skipped segment with the shorter
#' BFS route. States are indexed in a hash map supporting duplicate entries
#' to catch the farthest possible jumps in paths with repeated states.
#'
#' @param path Character vector of operations ("1"/"2"/"3" or "L"/"R"/"X")
#' @param start_state Integer vector, the starting permutation state
#' @param k Integer, parameter for reverse_prefix operation
#' @param depth Integer, BFS exploration depth (default 5)
#' @return List with path (shortened), original_length, new_length, savings
#' @export
short_path_bfs <- function(path, start_state, k, depth = 5L) {
  mapping <- c("1" = "L", "2" = "R", "3" = "X")
  path_normalized <- ifelse(path %in% names(mapping), mapping[path], path)
  names(path_normalized) <- NULL

  result <- short_path_bfs_cpp(
    as.integer(start_state),
    as.character(path_normalized),
    as.integer(k),
    as.integer(depth)
  )

  back_mapping <- c("L" = "1", "R" = "2", "X" = "3")
  result$path <- unname(back_mapping[result$path])

  result
}
