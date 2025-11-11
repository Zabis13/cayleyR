#' Apply Operation Sequence
#'
#' Applies a sequence of operations to a permutation state. Operations can be
#' specified as "L"/"1" (shift left), "R"/"2" (shift right), or "X"/"3"
#' (reverse first k elements).
#'
#' @param state Integer vector representing the initial permutation state
#' @param operations Character vector of operations to apply sequentially
#' @param k Integer, the parameter for reverse_prefix operations
#' @return Integer vector representing the state after all operations
#' @export
#' @examples
#' # Basic usage with numeric codes
#' apply_operations(1:5, c("1", "3"), k = 3)
#'
#' # TopSpin puzzle example
#' start_state <- 1:20
#' operations <- c("1", "3", "2")  # Left, Reverse(4), Right
#' result <- apply_operations(start_state, operations, k = 4)
#' print(result)
#'
#' # Using letter codes
#' apply_operations(1:5, c("L", "X", "R"), k = 3)
apply_operations <- function(state, operations, k) {
  current_state <- state

  for (op in operations) {
    if (op == "L" || op == "1") {
      current_state <- shift_left(current_state)
    } else if (op == "R" || op == "2") {
      current_state <- shift_right(current_state)
    } else if (op == "X" || op == "3") {
      current_state <- reverse_prefix(current_state, k)
    } else {
      stop(paste("Unknown operation:", op))
    }
  }

  return(current_state)
}
