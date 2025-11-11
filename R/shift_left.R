#' Shift State Left
#'
#' Performs a cyclic left shift on the state vector, moving the first element
#' to the end.
#'
#' @param state Integer vector representing the current permutation state
#' @return Integer vector with elements shifted left by one position
#' @export
#' @examples
#' # Basic shift operation
#' shift_left(1:5)
#'
#' # With variables
#' start_state <- 1:20
#' result <- shift_left(start_state)
#' print(result)
shift_left <- function(state) {
  c(state[2:length(state)], state[1])
}

