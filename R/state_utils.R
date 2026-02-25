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

#' Generate Random Permutation State
#'
#' Generates a random permutation of integers from 1 to n.
#'
#' @param n Integer, the size of the permutation
#' @return Integer vector representing a random permutation of 1:n
#' @export
#' @examples
#' set.seed(42)
#' generate_state(10)
generate_state <- function(n) {
  sample(1:n, size = n, replace = FALSE)
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
