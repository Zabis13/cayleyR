#' Reverse Prefix
#'
#' Reverses the first k elements of the state vector, analogous to the
#' turnstile operation in the TopSpin puzzle.
#'
#' @param state Integer vector representing the current permutation state
#' @param k Integer, the number of leading elements to reverse
#' @return Integer vector with the first k elements reversed
#' @export
#' @examples
#' # Basic example
#' reverse_prefix(1:10, 4)  # Returns c(4, 3, 2, 1, 5, 6, 7, 8, 9, 10)
#'
#' # With variables
#' n <- 10
#' k <- 4
#' start_state <- 1:n
#' demo <- reverse_prefix(start_state, k)
#' print(demo)
reverse_prefix <- function(state, k) {
  c(rev(state[1:k]), state[(k+1):length(state)])
}
