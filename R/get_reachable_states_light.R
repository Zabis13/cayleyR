#' Find Cycle Length (Lightweight Version)
#'
#' Fast version of cycle detection that only returns cycle length and unique
#' state count without storing all intermediate states. Useful for testing
#' many operation sequences efficiently. Implemented in C++ for performance.
#'
#' @param start_state Integer vector, the initial permutation state
#' @param allowed_positions Character vector, sequence of operations to repeat
#' @param k Integer, parameter for reverse operations
#' @return List containing:
#'   \item{total_moves}{Total number of moves to return to start state}
#'   \item{unique_states_count}{Number of unique states in the cycle}
#' @export
#' @examples
#' result <- get_reachable_states_light(1:10, c("1", "3"), k = 4)
#' cat("Cycle length:", result$total_moves, "\n")
#' cat("Unique states:", result$unique_states_count, "\n")
get_reachable_states_light <- function(start_state, allowed_positions, k) {
  get_reachable_states_light_cpp(
    as.integer(start_state),
    as.character(allowed_positions),
    as.integer(k)
  )
}
