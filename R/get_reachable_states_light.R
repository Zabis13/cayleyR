#' Find Cycle Length (Lightweight Version)
#'
#' Fast version of cycle detection that only returns cycle length and unique
#' state count without storing all intermediate states. Useful for testing
#' many operation sequences efficiently. Implemented in C++ for performance.
#'
#' @param start_state Integer vector, the initial permutation state
#' @param allowed_positions Sequence of operations to repeat, in the group's
#'   own move names
#' @param k Integer, parameter for reverse operations. Ignored when `group` is
#'   given.
#' @param group A \code{\link{perm_group}}. When `NULL` (default) the
#'   arguments describe TopSpin.
#' @param max_moves Integer, cap on how far the word is spun before giving up
#'   (default 1e7)
#' @return List containing:
#'   \item{total_moves}{Total number of moves to return to start state}
#'   \item{unique_states_count}{Number of unique states in the cycle}
#' @export
#' @examples
#' result <- get_reachable_states_light(1:10, c("1", "3"), k = 4)
#' cat("Cycle length:", result$total_moves, "\n")
#' cat("Unique states:", result$unique_states_count, "\n")
#'
#' # the order of the sexy move, the long way round
#' g <- cube_group(3)
#' get_reachable_states_light(group_identity(g), "R U R' U'", group = g)
get_reachable_states_light <- function(start_state, allowed_positions, k = NULL,
                                       group = NULL, max_moves = 10000000L) {
  start_state <- as.integer(start_state)
  res <- resolve_group(group, length(start_state), k, NULL)
  get_reachable_states_light_cpp(
    start_state,
    group_move_index(res$group, allowed_positions),
    res$group$ptr,
    as.integer(max_moves)
  )
}
