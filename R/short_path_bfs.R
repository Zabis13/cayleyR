#' Shorten Path via Depth-Limited BFS Hopping
#'
#' For each position along the path, explores all reachable states within
#' \code{depth} BFS steps. If any of those states appear later in the original
#' path (beyond current position + BFS steps taken), the algorithm "jumps"
#' to the farthest such match, replacing the skipped segment with the shorter
#' BFS route. States are indexed in a hash map supporting duplicate entries
#' to catch the farthest possible jumps in paths with repeated states.
#'
#' Works over any \code{\link{perm_group}}: the shortener only needs to apply
#' moves and compare states, so a cube solution shortens the same way a TopSpin
#' path does. The result is spelled in the same move names as the input.
#'
#' @param path Move sequence to shorten, in the group's own spelling. For
#'   TopSpin that is "1"/"2"/"3" or "L"/"R"/"X" as before; for a cube, names
#'   such as "R" or "M'".
#' @param start_state Integer vector, the starting permutation state
#' @param k Integer, parameter for reverse_prefix operation. Ignored when
#'   `group` is given.
#' @param depth Integer, BFS exploration depth (default 5)
#' @param moves Allowed operations for the rewrite, naming a subset of the
#'   group's alphabet (default: all of it)
#' @param group A \code{\link{perm_group}}. When `NULL` (default) the arguments
#'   describe TopSpin.
#' @return List with path (shortened), original_length, new_length, savings
#' @export
#' @seealso \code{\link{perm_group}}
#' @examples
#' # a path with an obvious detour: L then R cancel
#' short_path_bfs(c("1", "2", "1", "3"), 1:8, k = 4)$path
#'
#' # the same on a cube: R R' is a no-op
#' g <- cube_group(3)
#' short_path_bfs(c("R", "R'", "U"), group_identity(g), group = g)$path
short_path_bfs <- function(path, start_state, k = NULL, depth = 5L,
                           moves = NULL, group = NULL) {
  start_state <- as.integer(start_state)
  res <- resolve_group(group, length(start_state), k, moves)
  g <- res$group

  result <- short_path_bfs_cpp(
    start_state,
    group_move_index(g, path),
    g$ptr,
    res$moves,
    as.integer(depth)
  )

  # report in the group's own spelling, matching how the path came in
  result$path <- g$moves[result$path]
  result
}
