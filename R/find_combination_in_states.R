#' Find a State in Reachable States Table
#'
#' Searches for a specific permutation state in a reachable states table
#' and returns the first matching row with metadata.
#'
#' @param reachable_states_start Data frame with V-columns and metadata
#' @param search_state Integer vector, the state to search for
#' @return Data frame row with state and metadata columns, or NULL if not found
#' @export
#' @examples
#' df <- data.frame(V1 = c(1, 2), V2 = c(2, 1), operation = c("1", "2"),
#'                  step = c(1, 2), combo_number = c(1, 1))
#' find_combination_in_states(df, c(2, 1))
find_combination_in_states <- function(reachable_states_start, search_state) {
  if (inherits(reachable_states_start, "ArrowTabular")) {
    reachable_states_start <- reachable_states_start$to_data_frame()
  }

  n <- length(search_state)
  state_columns <- paste0("V", 1:n)

  matching_rows <- apply(reachable_states_start[, state_columns, drop = FALSE], 1, function(row) {
    all(row == search_state)
  })

  if (any(matching_rows)) {
    first_match_index <- which(matching_rows)[1]
    meta_cols <- intersect(c(state_columns, "operation", "step", "combo_number"), colnames(reachable_states_start))
    result <- reachable_states_start[first_match_index, meta_cols, drop = FALSE]
    return(result)
  }

  return(NULL)
}
