#' Find Cycle in Permutation Group
#'
#' Explores the Cayley graph starting from an initial state and applying
#' a sequence of operations repeatedly until returning to the start state.
#' Returns detailed information about all visited states, the cycle structure,
#' and celestial LRX coordinates.
#'
#' @param start_state Integer vector, the initial permutation state
#' @param allowed_positions Character vector, sequence of operations to repeat
#' @param k Integer, parameter for reverse operations
#' @param verbose Logical; if TRUE, prints progress and cycle information (default FALSE)
#' @return List containing:
#'   \item{states}{List of all visited states}
#'   \item{reachable_states_df}{Data frame with states, operations, steps, and celestial coordinates}
#'   \item{operations}{Vector of operations applied}
#'   \item{coords}{List of celestial coordinate objects per step}
#'   \item{nL_total}{Total left shifts}
#'   \item{nR_total}{Total right shifts}
#'   \item{nX_total}{Total reverse operations}
#'   \item{total_moves}{Total number of moves in the cycle}
#'   \item{unique_states_count}{Number of unique states visited}
#'   \item{cycle_info}{Summary string with cycle statistics}
#' @export
#' @examples
#' result <- get_reachable_states(1:10, c("1", "3"), k = 4)
#' writeLines(result$cycle_info)
get_reachable_states <- function(start_state, allowed_positions, k, verbose = FALSE) {
  n <- length(start_state)
  current_state <- as.integer(start_state)

  states_list <- list(current_state)
  operations_list <- character(0)
  coords_list <- list()

  visited_keys <- paste(current_state, collapse = "_")
  unique_states_count <- 1
  total_moves <- 0

  nL_total <- 0
  nR_total <- 0
  nX_total <- 0

  repeat {
    for (op in allowed_positions) {
      if (op == "1" || op == "L") nL_total <- nL_total + 1
      if (op == "2" || op == "R") nR_total <- nR_total + 1
      if (op == "3" || op == "X") nX_total <- nX_total + 1

      result <- apply_operations(current_state, op, k, NULL)
      current_state <- result$state

      current_coords <- convert_LRX_to_celestial(nL_total, nR_total, nX_total)

      total_moves <- total_moves + 1
      states_list[[length(states_list) + 1]] <- current_state
      operations_list <- c(operations_list, op)
      coords_list[[length(coords_list) + 1]] <- current_coords

      state_key <- paste(current_state, collapse = "_")
      if (!state_key %in% visited_keys) {
        visited_keys <- c(visited_keys, state_key)
        unique_states_count <- unique_states_count + 1
      }

      if (identical(current_state, as.integer(start_state)) && total_moves > 0) {
        states_mat <- do.call(rbind, states_list)
        ops_final <- c(operations_list, NA_character_)
        steps_final <- c(seq_len(length(states_list) - 1), NA_integer_)

        extract_safe <- function(coord_obj, field) {
          val <- coord_obj[[field]]
          if (is.null(val) || length(val) == 0 || is.na(val)) return(NA_real_)
          return(as.numeric(val))
        }

        theta_vec <- vapply(coords_list, function(x) extract_safe(x, "theta"), numeric(1))
        phi_vec <- vapply(coords_list, function(x) extract_safe(x, "phi"), numeric(1))
        omega_vec <- vapply(coords_list, function(x) extract_safe(x, "omega_conformal"), numeric(1))

        theta_vec <- c(NA_real_, theta_vec)
        phi_vec <- c(NA_real_, phi_vec)
        omega_vec <- c(NA_real_, omega_vec)

        main_info <- paste0(
          "Cycle analysis n=", n, ", k=", k, ", ops=[",
          paste(allowed_positions, collapse = ","), "]\n",
          "Total moves: ", total_moves, "\n",
          "Unique states: ", unique_states_count, "\n",
          "Final LRX: (", nL_total, ",", nR_total, ",", nX_total, ")")

        if (verbose) cat(main_info, "\n")

        reachable_states <- as.data.frame(states_mat)
        colnames(reachable_states) <- paste0("V", 1:n)
        reachable_states$operation <- ops_final
        reachable_states$step <- steps_final
        reachable_states$theta <- theta_vec
        reachable_states$phi <- phi_vec
        reachable_states$omega_conformal <- omega_vec

        return(list(
          states = states_list,
          reachable_states_df = reachable_states,
          operations = operations_list,
          coords = coords_list,
          nL_total = nL_total, nR_total = nR_total, nX_total = nX_total,
          total_moves = total_moves,
          unique_states_count = unique_states_count,
          cycle_info = main_info
        ))
      }
    }
  }
}
