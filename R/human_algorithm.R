#' Solve a State with the Human TopSpin Algorithm
#'
#' Reproduces the way a person solves TopSpin by hand. A sorted run is grown one
#' value at a time: for each new value \code{m} the ring is manoeuvred until
#' \code{m} sits exactly \code{k} positions after \code{m-1}, so that a single
#' reverse-prefix drops \code{m} directly behind \code{m-1}. Auxiliary flips are
#' restricted to windows lying wholly inside the unsorted arc, so the run is
#' never disturbed.
#'
#' Once the tail is down to eight tiles the insertion move no longer fits, and
#' the tail is finished with two local 3-cycles built from the same operations
#' (\code{XLXLXRX} and \code{LXRXLXLX}, each with a compensating rotation). Their
#' conjugates generate the full alternating group on the tail, so any even
#' arrangement is reachable by table lookup rather than search.
#'
#' Because 3-cycles are even permutations, odd tail arrangements are out of
#' their reach. In that case a single flip is fired across the block boundary
#' and the run is rebuilt: moving one tile between block and tail changes the
#' parity of the split, after which the table applies.
#'
#' @param start_state Integer vector, the starting permutation state
#' @param final_state Integer vector, the target state. Defaults to
#'   \code{1:n}, i.e. plain sorting.
#' @param k Integer, length of the reverse-prefix (flipper) operation
#' @param simplify Logical, run \code{short_position()} on the result
#' @return List with components:
#'   \item{found}{Logical, whether the target was reached}
#'   \item{path}{Character vector of operations ("1"/"2"/"3")}
#'   \item{length}{Integer, number of operations}
#' @export
#' @examples
#' set.seed(1)
#' s <- generate_state(20, k = 4, n_moves = 50)
#' res <- human_algorithm(s, k = 4)
#' res$found
human_algorithm <- function(start_state, final_state = NULL, k = 4L,
                            simplify = TRUE) {
  start_state <- as.integer(start_state)
  n <- length(start_state)
  k <- as.integer(k)

  if (is.null(final_state)) final_state <- seq_len(n)
  final_state <- as.integer(final_state)

  if (length(final_state) != n) {
    stop("human_algorithm: start_state and final_state must have equal length")
  }

  solve_one <- function(st) {
    res <- human_algorithm_cpp(st, k, 1e6, TRUE)
    res$path <- unname(c("L" = "1", "R" = "2", "X" = "3")[res$path])
    res
  }

  a <- solve_one(start_state)
  if (!a$found) {
    return(list(found = FALSE, path = NULL, length = NA_integer_))
  }

  if (identical(final_state, seq_len(n))) {
    path <- a$path
  } else {
    b <- solve_one(final_state)
    if (!b$found) {
      return(list(found = FALSE, path = NULL, length = NA_integer_))
    }
    path <- c(a$path, invert_path(b$path))
  }

  if (simplify) {
    chk <- validate_and_simplify_path(path, start_state, final_state, k)
    if (chk$valid) path <- chk$path
  }

  ok <- identical(
    as.integer(apply_operations(start_state, path, k, compute_coords = FALSE)$state),
    final_state
  )

  list(found = ok, path = path, length = length(path))
}
