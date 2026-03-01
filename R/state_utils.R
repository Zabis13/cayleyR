#' Convert String to Integer Vector of Digits
#'
#' Parses a string of digits or space-separated numbers into an integer vector.
#' Useful for converting operation sequences or state representations.
#'
#' @param s Character string. Either a string of single digits (e.g., "123")
#'   or space-separated numbers (e.g., "1 2 3" or "10 11 12").
#' @return Integer vector of parsed numbers
#' @export
#' @examples
#' convert_digits("123")
#' convert_digits("1 5 4 3 2")
#' convert_digits("10 11 12 13")
convert_digits <- function(s) {
  s <- as.character(s)

  # If input is a pure digit string, split into single digits
  if (nchar(s) > 0 && all(grepl("^[0-9]+$", s))) {
    return(as.integer(strsplit(s, "")[[1]]))
  }

  # Otherwise split by whitespace

  s <- gsub("^\\s+|\\s+$", "", s)
  s <- gsub("\\s+", " ", s)
  numbers <- strsplit(s, " ")[[1]]
  return(as.integer(numbers))
}

#' Generate Reachable Random State
#'
#' Generates a random state reachable from 1:n by applying random
#' operations (L, R, X). Guarantees the result is in the same
#' connected component as the starting state.
#'
#' @param n Integer, the size of the permutation
#' @param k Integer, parameter for reverse_prefix operation
#' @param n_moves Integer, number of random operations to apply (default 25)
#' @param moves Character vector, allowed operations (default c("1", "2", "3"))
#' @return Integer vector representing a reachable permutation state
#' @export
#' @examples
#' set.seed(42)
#' generate_state(10, k = 4)
#' generate_state(10, k = 4, n_moves = 100)
generate_state <- function(n, k = n, n_moves = 25L, moves = c("1", "2", "3")) {
  state <- as.integer(1:n)
  ops <- sample(moves, size = n_moves, replace = TRUE)
  result <- apply_operations(state, ops, as.integer(k))
  as.integer(result$state)
}

#' Generate Data Frame of Unique Random States
#'
#' Generates a data frame with unique random permutation states.
#'
#' @param n Integer, size of each permutation state
#' @param n_rows Integer, number of unique states to generate
#' @return Data frame with n_rows rows and columns V1, V2, ..., Vn
#' @export
#' @examples
#' set.seed(42)
#' df <- generate_unique_states_df(5, 10)
#' head(df)
generate_unique_states_df <- function(n, n_rows) {
  combos <- replicate(n_rows, sample(1:n, size = n, replace = FALSE))
  df <- as.data.frame(t(combos))
  colnames(df) <- paste0("V", 1:n)
  unique(df)
}

#' Manhattan Distance Between Two States
#'
#' Computes the sum of absolute differences between corresponding elements
#' of two permutation states.
#'
#' @param start_state Integer vector, first state
#' @param target_state Integer vector, second state
#' @return Numeric, the Manhattan distance
#' @export
#' @examples
#' manhattan_distance(1:5, 5:1)
#' manhattan_distance(1:5, 1:5)
manhattan_distance <- function(start_state, target_state) {
  sum(abs(start_state - target_state))
}

#' Breakpoint Distance Between Two States
#'
#' Counts the number of positions where consecutive elements differ by
#' more than 1 (breakpoints). Particularly effective for TopSpin puzzles
#' where operations shift blocks and flip prefixes.
#'
#' @param start_state Integer vector, first state
#' @param target_state Integer vector, second state
#' @return Integer, the number of breakpoints
#' @export
#' @examples
#' breakpoint_distance(1:5, 5:1)
#' breakpoint_distance(1:5, 1:5)
breakpoint_distance <- function(start_state, target_state) {
  relative <- target_state[order(start_state)]
  sum(diff(relative) != 1L)
}
