#' Find Cycle Length (Lightweight Version)
#'
#' Fast version of cycle detection that only returns cycle length and unique
#' state count without storing all intermediate states. Useful for testing
#' many operation sequences efficiently.
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
  current_state <- as.integer(start_state)
  visited_keys <- paste(current_state, collapse = "_")
  unique_states_count <- 1
  total_moves <- 0

  repeat {
    for (op in allowed_positions) {
      result <- apply_operations(current_state, op, k, NULL)
      current_state <- result$state

      total_moves <- total_moves + 1

      state_key <- paste(current_state, collapse = "_")
      if (!state_key %in% visited_keys) {
        visited_keys <- c(visited_keys, state_key)
        unique_states_count <- unique_states_count + 1
      }

      if (identical(current_state, as.integer(start_state)) && total_moves > 0) {
        return(list(
          total_moves = total_moves,
          unique_states_count = unique_states_count
        ))
      }
    }
  }
}
