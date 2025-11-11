#' Shift State Right
#'
#' Performs a cyclic right shift on the state vector, moving the last element
#' to the beginning.
#'
#' @param state Integer vector representing the current permutation state
#' @return Integer vector with elements shifted right by one position
#' @export
#' @examples
#' # Simple example
#' shift_right(1:5)
#'
#' # With variable assignment
#' start_state <- 1:20
#' result <- shift_right(start_state)
#' print(result)
shift_right <- function(state) {
  c(state[length(state)], state[1:(length(state)-1)])
}
