#' Add State Keys to Data Frame
#'
#' Computes paste-based state keys for V-columns and adds a `state_key`
#' column. If keys already exist, only computes for new rows.
#'
#' @param states_df Data frame or Arrow Table
#' @param new_states Data frame of newly added states (used when state_key
#'   column already exists to compute keys only for new rows)
#' @param v_cols Character vector of V-column names
#' @return Data frame with `state_key` column
#' @keywords internal
add_state_keys <- function(states_df, new_states, v_cols) {
  if (inherits(states_df, "ArrowTabular")) {
    states_df <- states_df$to_data_frame()
  }
  if (inherits(new_states, "ArrowTabular")) {
    new_states <- new_states$to_data_frame()
  }

  if (!"state_key" %in% colnames(states_df)) {
    mat <- as.matrix(states_df[, v_cols, drop = FALSE])
    states_df$state_key <- apply(mat, 1L, function(x) paste(x, collapse = "_"))
  } else {
    mat_new <- as.matrix(new_states[, v_cols, drop = FALSE])
    new_keys <- apply(mat_new, 1L, function(x) paste(x, collapse = "_"))
    start_idx <- nrow(states_df) - nrow(new_states) + 1L
    states_df$state_key[start_idx:nrow(states_df)] <- new_keys
  }

  states_df
}

#' Create Hash Index from State Keys
#'
#' Builds a hash environment mapping state_key strings to row indices
#' for fast lookup.
#'
#' @param states_df Data frame with a `state_key` column
#' @return Environment (hash table) mapping keys to integer vectors of row indices
#' @keywords internal
create_hash_index <- function(states_df) {
  index <- new.env(hash = TRUE)
  for (i in 1:nrow(states_df)) {
    key <- states_df$state_key[i]
    if (is.null(index[[key]])) {
      index[[key]] <- i
    } else {
      index[[key]] <- c(index[[key]], i)
    }
  }
  return(index)
}
