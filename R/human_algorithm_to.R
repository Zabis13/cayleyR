#' Human Algorithm Path Between Two Arbitrary States
#'
#' Finds a path from \code{start_state} to \code{target_state} with the same
#' human TopSpin method as \code{\link{human_algorithm}}, but without routing
#' through the identity state.
#'
#' \code{human_algorithm} reaches an arbitrary target by solving both endpoints
#' to \code{1:n} and concatenating the first path with the inverse of the
#' second, so the word is roughly twice as long as it needs to be. Here the
#' problem is instead relabelled: because the three operations permute
#' \emph{positions} and treat the values as inert labels, renaming every value
#' \code{v} to its position in \code{target_state} turns "reach
#' \code{target_state}" into "reach \code{1:n}". The solver then runs once on
#' the relabelled state and the resulting word applies unchanged to the
#' original one.
#'
#' Formally, with \code{inv} the inverse permutation of the target
#' (\code{inv[target_state] == seq_len(n)}), the relabelled state is
#' \code{inv[start_state]}. Any word \code{P} sorting it satisfies
#' \code{P(inv[start_state]) == 1:n}, and since \code{P} acts on positions it
#' commutes with the elementwise relabelling, giving
#' \code{P(start_state) == target_state}.
#'
#' @param start_state Integer vector, the starting permutation state
#' @param target_state Integer vector, the target permutation state. Defaults
#'   to \code{1:n}, in which case this is exactly \code{\link{human_algorithm}}.
#' @param k Integer, length of the reverse-prefix (flipper) operation
#' @param simplify Logical, run \code{short_position()} on the result
#' @return List with components:
#'   \item{found}{Logical, whether the target was reached}
#'   \item{path}{Character vector of operations ("1"/"2"/"3")}
#'   \item{length}{Integer, number of operations}
#' @export
#' @seealso \code{\link{human_algorithm}}
#' @examples
#' set.seed(1)
#' s <- generate_state(20, k = 4, n_moves = 50)
#' t <- generate_state(20, k = 4, n_moves = 50)
#' res <- human_algorithm_to(s, t, k = 4)
#' res$found
human_algorithm_to <- function(start_state, target_state = NULL, k = 4L,
                               simplify = TRUE) {
  start_state <- as.integer(start_state)
  n <- length(start_state)
  k <- as.integer(k)

  if (is.null(target_state)) target_state <- seq_len(n)
  target_state <- as.integer(target_state)

  if (length(target_state) != n) {
    stop("human_algorithm_to: start_state and target_state must have equal length")
  }
  if (!setequal(start_state, seq_len(n)) || !setequal(target_state, seq_len(n))) {
    stop("human_algorithm_to: both states must be permutations of 1:n")
  }

  # Relabel: rename each value to its position in the target, so that
  # reaching target_state becomes reaching 1:n.
  inv <- integer(n)
  inv[target_state] <- seq_len(n)
  relabelled <- inv[start_state]

  res <- human_algorithm_cpp(relabelled, k, 1e6, TRUE)
  if (!res$found) {
    return(list(found = FALSE, path = NULL, length = NA_integer_))
  }
  path <- unname(c("L" = "1", "R" = "2", "X" = "3")[res$path])

  if (simplify) {
    chk <- validate_and_simplify_path(path, start_state, target_state, k)
    if (chk$valid) path <- chk$path
  }

  ok <- identical(
    as.integer(apply_operations(start_state, path, k, compute_coords = FALSE)$state),
    target_state
  )

  list(found = ok, path = path, length = length(path))
}
