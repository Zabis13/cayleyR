#' Find Cycle in Permutation Group
#'
#' Explores the Cayley graph starting from an initial state and applying
#' a sequence of operations repeatedly until returning to the start state.
#' Returns detailed information about all visited states and the cycle structure.
#'
#' @param start_state Integer vector, the initial permutation state
#' @param allowed_positions Character vector, sequence of operations to repeat
#' @param k Integer, parameter for reverse operations
#' @param verbose Logical; if TRUE, prints progress and cycle information messages (default FALSE)
#' @return List containing:
#'   \item{states}{List of all visited states}
#'   \item{reachable_states_df}{Data frame with states, operations, and step numbers}
#'   \item{operations}{Vector of operations applied}
#'   \item{total_moves}{Total number of moves in the cycle}
#'   \item{unique_states_count}{Number of unique states visited}
#'   \item{cycle_info}{Summary string with cycle statistics}
#' @export
#' @examples
#' # Simple example with letter codes
#' result <- get_reachable_states(1:20, c("L", "X"), k = 4)
#' writeLines(result$cycle_info)
#'
#' # Example with numeric codes
#' n <- 20
#' k <- 4
#' start_state <- 1:n
#' allowed_positions <- c("1", "3", "2")
#' result <- get_reachable_states(start_state, allowed_positions, k)
#' writeLines(result$cycle_info)
#' head(result$reachable_states_df)
get_reachable_states <- function(start_state, allowed_positions, k, verbose = FALSE) {
  current_state <- start_state
  visited <- new.env(hash = TRUE)
  states_list <- list(current_state)
  operations_list <- c()
  unique_states_count <- 1
  total_moves <- 0

  state_hash <- digest::digest(current_state, algo = "md5")
  visited[[state_hash]] <- TRUE

  repeat {
    for (op in allowed_positions) {
      current_state <- apply_operations(current_state, op, k)

      total_moves <- total_moves + 1
      states_list[[length(states_list) + 1]] <- current_state
      operations_list <- c(operations_list, op)

      state_hash <- digest::digest(current_state, algo = "md5")
      if (!exists(state_hash, envir = visited)) {
        visited[[state_hash]] <- TRUE
        unique_states_count <- unique_states_count + 1
      }

      if (identical(current_state, start_state) && total_moves > 0) {
        n <- length(start_state)
        ops_for_table <- c(operations_list, NA)
        steps_for_table <- c(seq_len(length(states_list) - 1), NA)

        reachable_states <- data.frame(matrix(unlist(states_list), ncol = n, byrow = TRUE))
        colnames(reachable_states) <- paste0("V", 1:n)
        reachable_states$operation <- ops_for_table
        reachable_states$step <- steps_for_table

        main_info <- paste0(
          "Cycle analysis for n = ", n, ", k = ", k, ", allowed_positions = [",
          paste(allowed_positions, collapse = ", "), "]\n",
          "Total moves in cycle: ", total_moves, "\n",
          "Number of unique states: ", unique_states_count, "\n",
          "Repeated states: ", (total_moves + 1) - unique_states_count, "\n",
          "Cycle length: ", total_moves, "\n"
        )

        if (verbose) {
          message(main_info)
        }


        return(list(
          states = states_list,
          reachable_states_df = reachable_states,
          operations = operations_list,
          total_moves = total_moves,
          unique_states_count = unique_states_count,
          cycle_info = main_info
        ))
      }
    }
  }
}
