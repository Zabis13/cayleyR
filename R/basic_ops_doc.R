#' Shift State Left (with Coordinates)
#'
#' Performs a cyclic left shift on the state vector, moving the first element
#' to the end. Tracks celestial coordinates (LRX momentum).
#'
#' @param state Integer vector representing the current permutation state
#' @param coords Optional list of current celestial coordinates. If NULL,
#'   starts from zero coordinates.
#' @return List with components:
#'   \item{state}{Integer vector with elements shifted left by one position}
#'   \item{coords}{List of updated celestial coordinates (nL, nR, nX, theta, phi, omega_conformal)}
#' @export
#' @examples
#' result <- shift_left(1:5)
#' result$state
#' result$coords
#'
#' # Chain operations using coords
#' r1 <- shift_left(1:5)
#' r2 <- shift_left(r1$state, r1$coords)
#' r2$coords$nL
#' @name shift_left
NULL

#' Shift State Right (with Coordinates)
#'
#' Performs a cyclic right shift on the state vector, moving the last element
#' to the front. Tracks celestial coordinates (LRX momentum).
#'
#' @param state Integer vector representing the current permutation state
#' @param coords Optional list of current celestial coordinates. If NULL,
#'   starts from zero coordinates.
#' @return List with components:
#'   \item{state}{Integer vector with elements shifted right by one position}
#'   \item{coords}{List of updated celestial coordinates (nL, nR, nX, theta, phi, omega_conformal)}
#' @export
#' @examples
#' result <- shift_right(1:5)
#' result$state
#' @name shift_right
NULL

#' Reverse First k Elements (with Coordinates)
#'
#' Reverses the first k elements of the state vector (turnstile operation).
#' Tracks celestial coordinates (LRX momentum).
#'
#' @param state Integer vector representing the current permutation state
#' @param k Integer, number of elements to reverse from the beginning
#' @param coords Optional list of current celestial coordinates. If NULL,
#'   starts from zero coordinates.
#' @return List with components:
#'   \item{state}{Integer vector with first k elements reversed}
#'   \item{coords}{List of updated celestial coordinates (nL, nR, nX, theta, phi, omega_conformal)}
#' @export
#' @examples
#' result <- reverse_prefix(1:10, k = 4)
#' result$state
#' @name reverse_prefix
NULL

#' Shift State Left (Simple)
#'
#' Simple cyclic left shift without coordinate tracking.
#'
#' @param state Integer vector representing the current permutation state
#' @return Integer vector with elements shifted left by one position
#' @export
#' @examples
#' shift_left_simple(1:5)
#' @name shift_left_simple
NULL

#' Shift State Right (Simple)
#'
#' Simple cyclic right shift without coordinate tracking.
#'
#' @param state Integer vector representing the current permutation state
#' @return Integer vector with elements shifted right by one position
#' @export
#' @examples
#' shift_right_simple(1:5)
#' @name shift_right_simple
NULL

#' Reverse First k Elements (Simple)
#'
#' Simple prefix reversal without coordinate tracking.
#'
#' @param state Integer vector representing the current permutation state
#' @param k Integer, number of elements to reverse from the beginning
#' @return Integer vector with first k elements reversed
#' @export
#' @examples
#' reverse_prefix_simple(1:10, k = 4)
#' @name reverse_prefix_simple
NULL

#' Apply Sequence of Operations
#'
#' Applies a sequence of shift and reverse operations to a permutation state.
#' Operations can be specified as "1"/"L" (shift left), "2"/"R" (shift right),
#' or "3"/"X" (reverse prefix). Tracks celestial coordinates.
#'
#' @param state Integer vector representing the current permutation state
#' @param operations Character vector of operations ("1"/"L", "2"/"R", "3"/"X")
#' @param k Integer, parameter for reverse operations
#' @param coords Optional list of current celestial coordinates. If NULL,
#'   starts from zero coordinates.
#' @return List with components:
#'   \item{state}{Integer vector after all operations applied}
#'   \item{coords}{List of final celestial coordinates (nL, nR, nX, theta, phi, omega_conformal)}
#' @export
#' @examples
#' result <- apply_operations(1:10, c("1", "3", "2"), k = 4)
#' result$state
#'
#' # Using letter codes
#' result <- apply_operations(1:20, c("L", "X", "R"), k = 4)
#' result$state
#' @name apply_operations
NULL
