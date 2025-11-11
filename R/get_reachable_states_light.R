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
#' \dontrun{
#' # Quick cycle length check
#' result <- get_reachable_states_light(1:20, c("L", "X", "L"), k = 4)
#' cat("Cycle length:", result$total_moves, "\n")
#' cat("Unique states:", result$unique_states_count, "\n")
#'
#' # Compare multiple sequences
#' seq1 <- get_reachable_states_light(1:20, c("1", "3"), k = 4)
#' seq2 <- get_reachable_states_light(1:20, c("2", "3"), k = 4)
#' cat("Sequence 1 cycle:", seq1$total_moves, "\n")
#' cat("Sequence 2 cycle:", seq2$total_moves, "\n")
#' }
get_reachable_states_light <- function(start_state, allowed_positions, k) {
  current_state <- start_state
  visited <- new.env(hash = TRUE)
  unique_states_count <- 1
  total_moves <- 0

  state_hash <- digest::digest(current_state, algo = "md5")
  visited[[state_hash]] <- TRUE

  repeat {
    for (op in allowed_positions) {
      # Apply one operation
      current_state <- apply_operations(current_state, op, k)

      total_moves <- total_moves + 1

      state_hash <- digest::digest(current_state, algo = "md5")
      if (!exists(state_hash, envir = visited)) {
        visited[[state_hash]] <- TRUE
        unique_states_count <- unique_states_count + 1
      }

      if (identical(current_state, start_state) && total_moves > 0) {
        return(list(
          total_moves = total_moves,
          unique_states_count = unique_states_count
        ))
      }
    }
  }
}
