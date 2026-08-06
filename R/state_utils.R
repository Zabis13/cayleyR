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
#' Generates a random state by walking away from the identity with random
#' moves. The walk stays inside the group, so the result is always reachable
#' from where it started --- which a random permutation of \code{1:n} would not
#' be. On the cube that distinction is the whole point: of the
#' \eqn{54!} arrangements of 54 stickers only about \eqn{4.3 \times 10^{19}}
#' are positions a cube can actually reach.
#'
#' Given a \code{group}, moves are drawn from that group's alphabet. Without
#' one the arguments describe TopSpin, as they always have, and \code{moves}
#' names its operations \code{"1"}/\code{"2"}/\code{"3"}.
#'
#' @param n Integer, the size of the permutation. Optional when \code{group} is
#'   given, since the group fixes it; required otherwise.
#' @param k Integer, parameter for reverse_prefix operation (TopSpin only)
#' @param n_moves Integer, number of random operations to apply (default 25)
#' @param moves Moves to draw from. With a \code{group}, a subset of its
#'   alphabet (default all of it); without one, TopSpin's operations
#'   (default \code{c("1", "2", "3")}).
#' @param group A \code{\link{perm_group}} to walk in. When absent the
#'   arguments describe TopSpin.
#' @param max_attempts Integer, maximum attempts to generate a non-identity state (default 100)
#' @return Integer vector representing a reachable permutation state
#' @export
#' @seealso \code{\link{cube_group}}, for the cube's own moves and
#'   also reports the moves it used
#' @examples
#' set.seed(42)
#' generate_state(10, k = 4)
#' generate_state(10, k = 4, n_moves = 100)
#'
#' # in an arbitrary group: n comes from the group
#' g <- cube_group(3)
#' s <- generate_state(group = g, n_moves = 20)
#' length(s)
#'
#' # the centres of a real cube never move, and this state is a real one
#' all(s[c(5, 14, 23, 32, 41, 50)] == c(5, 14, 23, 32, 41, 50))
generate_state <- function(n = NULL, k = NULL, n_moves = 25L, moves = NULL,
                           group = NULL, max_attempts = 100L) {
  if (!is.null(group)) {
    if (!is_perm_group(group)) stop("generate_state: group must be a perm_group")
    if (!is.null(n) && n != group$n) {
      stop("generate_state: n is ", n, " but group '", group$name,
           "' works on length ", group$n)
    }
    idx <- group_move_index(group, moves)
    if (length(idx) == 0L) stop("generate_state: no moves to draw from")

    identity <- group_identity(group)
    for (i in seq_len(max_attempts)) {
      word <- idx[sample.int(length(idx), size = n_moves, replace = TRUE)]
      state <- group_apply(group, identity, word)
      if (!identical(state, identity)) return(state)
    }
    stop("generate_state: failed to produce non-identity state in ",
         max_attempts, " attempts")
  }

  # No group: the arguments describe TopSpin, as they always have.
  if (is.null(n)) stop("generate_state: n is required when group is not given")
  if (is.null(k)) k <- n
  if (is.null(moves)) moves <- c("1", "2", "3")

  identity <- as.integer(1:n)
  for (i in seq_len(max_attempts)) {
    ops <- sample(moves, size = n_moves, replace = TRUE)
    result <- apply_operations(identity, ops, as.integer(k))
    state <- as.integer(result$state)
    if (!identical(state, identity)) return(state)
  }
  stop("generate_state: failed to produce non-identity state in ", max_attempts, " attempts")
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
