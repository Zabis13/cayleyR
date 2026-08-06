#' Invert a Path of Operations
#'
#' Reverses a sequence of operations and replaces each by its inverse, giving
#' the path that undoes the original. For TopSpin that means "1" (shift left)
#' becomes "2" (shift right) and vice versa, while "3" (reverse) is its own
#' inverse; for any other \code{\link{perm_group}} the group says which move
#' undoes which.
#'
#' @param path Character vector of operations
#' @param group A \code{\link{perm_group}}. When `NULL` (default) the path is
#'   read as TopSpin's, as it always has been.
#' @return Character vector of inverted operations in reverse order
#' @export
#' @seealso \code{\link{group_inverse_seq}}
#' @examples
#' invert_path(c("1", "3", "2"))
#' invert_path(c("1", "1", "3"))
#'
#' invert_path(c("R", "U", "U", "F'"), group = cube_group(3))
invert_path <- function(path, group = NULL) {
  if (length(path) == 0) return(character(0))

  if (!is.null(group)) return(group_inverse_seq(group, path, as_names = TRUE))

  inverted <- sapply(rev(path), function(op) {
    if (op == "1") return("2")
    if (op == "2") return("1")
    if (op == "3") return("3")
    return(op)
  })

  names(inverted) <- NULL
  return(inverted)
}

#' Simplify Operation Path
#'
#' Removes redundant operations from a path: cancels inverse pairs ("1"+"2",
#' "3"+"3"), reduces chains of shifts modulo n, and simplifies blocks
#' between reverses.
#'
#' @param allowed_positions Character vector of operations to simplify
#' @param n Integer, size of the permutation ring (used for modular reduction)
#' @return Character vector of simplified operations
#' @export
#' @examples
#' short_position(c("1", "2"), n = 5)
#' short_position(c("3", "3"), n = 5)
#' short_position(c("1", "1", "1", "1", "1"), n = 5)
short_position <- function(allowed_positions, n) {
  if (length(allowed_positions) == 0) return(allowed_positions)

  repeat {
    old_len <- length(allowed_positions)

    # Step 0: reduce chains of identical shift operations
    opt_list <- vector("list", length(allowed_positions))
    opt_idx <- 0L
    i <- 1
    while (i <= length(allowed_positions)) {
      op <- allowed_positions[i]

      if (op %in% c("1", "2")) {
        count <- 1
        while (i + count <= length(allowed_positions) &&
               allowed_positions[i + count] == op) {
          count <- count + 1
        }

        residual <- count %% n
        if (residual > n / 2) {
          residual <- n - residual
          op <- if (op == "1") "2" else "1"
        }

        if (residual != 0) {
          opt_idx <- opt_idx + 1L
          opt_list[[opt_idx]] <- rep(op, residual)
        }
        i <- i + count
      } else {
        opt_idx <- opt_idx + 1L
        opt_list[[opt_idx]] <- op
        i <- i + 1
      }
    }
    allowed_positions <- unlist(opt_list[seq_len(opt_idx)])
    if (is.null(allowed_positions)) allowed_positions <- character(0)

    if (length(allowed_positions) == 0) break

    # Step 1: remove adjacent inverse pairs
    i <- 1
    len <- length(allowed_positions)
    clean_vec <- character(len)
    clean_idx <- 0L
    while (i <= len) {
      if (i < len) {
        curr <- allowed_positions[i]
        next_op <- allowed_positions[i + 1]

        if ((curr == "1" && next_op == "2") ||
            (curr == "2" && next_op == "1") ||
            (curr == "3" && next_op == "3")) {
          i <- i + 2
          next
        }
      }
      clean_idx <- clean_idx + 1L
      clean_vec[clean_idx] <- allowed_positions[i]
      i <- i + 1
    }
    allowed_positions <- if (clean_idx > 0L) clean_vec[seq_len(clean_idx)] else character(0)

    if (length(allowed_positions) == 0) break

    # Step 2: split into blocks between reverses
    blocks <- list()
    start_idx <- 1
    for (i in seq_len(length(allowed_positions))) {
      if (allowed_positions[i] == "3") {
        if (start_idx <= i - 1) {
          blocks[[length(blocks) + 1]] <- allowed_positions[start_idx:(i - 1)]
        }
        blocks[[length(blocks) + 1]] <- "3"
        start_idx <- i + 1
      }
    }
    if (start_idx <= length(allowed_positions)) {
      blocks[[length(blocks) + 1]] <- allowed_positions[start_idx:length(allowed_positions)]
    }

    # Step 3: simplify blocks
    new_blocks <- list()
    i <- 1
    while (i <= length(blocks)) {
      block <- blocks[[i]]

      if (is.character(block) && length(block) == 1 && block == "3") {
        count_3 <- 1
        while (i + count_3 <= length(blocks)) {
          next_block <- blocks[[i + count_3]]
          if (is.character(next_block) && length(next_block) == 1 && next_block == "3") {
            count_3 <- count_3 + 1
          } else {
            break
          }
        }
        mod_3 <- count_3 %% 2
        if (mod_3 == 1) {
          new_blocks[[length(new_blocks) + 1]] <- "3"
        }
        i <- i + count_3
      } else {
        shift_vals <- sapply(block, function(x) if (x == "1") 1 else if (x == "2") -1 else 0)
        total_shift <- sum(shift_vals)
        residual_shift <- total_shift %% n

        if (residual_shift > n / 2) {
          residual_shift <- residual_shift - n
        }

        if (residual_shift > 0) {
          new_blocks[[length(new_blocks) + 1]] <- rep("1", residual_shift)
        } else if (residual_shift < 0) {
          new_blocks[[length(new_blocks) + 1]] <- rep("2", abs(residual_shift))
        }

        i <- i + 1
      }
    }

    # unlist() on an empty list yields NULL, which downstream C++ rejects;
    # a fully cancelled path is an empty character vector, not NULL.
    allowed_positions <- unlist(new_blocks)
    if (is.null(allowed_positions)) allowed_positions <- character(0)

    if (length(allowed_positions) == old_len) break
  }

  return(allowed_positions)
}

#' Validate and Simplify a Path
#'
#' Verifies that a candidate path correctly transforms start_state into
#' final_state, then attempts to simplify it. Returns the simplified path
#' if it remains valid, otherwise the original.
#'
#' Simplification is TopSpin's: \code{\link{short_position}} reduces runs of
#' shifts modulo the ring and cancels reverses, which are facts about L, R and
#' X rather than about permutations in general. For any other group the path is
#' verified but returned as it came in --- checking is universal, this
#' particular rewriting is not. \code{\link{cycle_shortcut}} and
#' \code{\link{short_path_bfs}} shorten paths for every group.
#'
#' @param path_candidate Character vector of operations
#' @param start_state Integer vector, start state
#' @param final_state Integer vector, target state
#' @param k Integer, parameter for reverse operations. Ignored when `group` is
#'   given.
#' @param group A \code{\link{perm_group}}. When `NULL` (default) the path is
#'   read as TopSpin's.
#' @return List with components:
#'   \item{valid}{Logical, whether the path is valid}
#'   \item{path}{Simplified or original path, or NULL if invalid}
#' @export
#' @examples
#' res <- validate_and_simplify_path(c("1", "3"), 1:5, c(5, 2, 3, 4, 1), k = 2)
#' res$valid
#'
#' g <- cube_group(3)
#' s <- group_apply(g, group_identity(g), "R U")
#' validate_and_simplify_path(c("R", "U"), group_identity(g), s, group = g)$valid
validate_and_simplify_path <- function(path_candidate, start_state, final_state,
                                       k = NULL, group = NULL) {
  if (is.null(path_candidate)) {
    return(list(valid = FALSE, path = NULL))
  }

  if (length(path_candidate) == 0) {
    return(list(valid = TRUE, path = path_candidate))
  }

  start_state <- as.integer(start_state)
  n <- length(start_state)
  res <- resolve_group(group, n, k, NULL)
  g <- res$group

  reached <- function(p) {
    tryCatch(identical(group_apply(g, start_state, p),
                       as.integer(final_state)),
             error = function(e) FALSE)
  }

  if (!reached(path_candidate)) {
    return(list(valid = FALSE, path = NULL))
  }

  # short_position() knows TopSpin's algebra specifically; leave other groups'
  # paths alone rather than rewrite them by rules that do not hold there.
  if (!is.null(group) && !identical(sort(g$moves), sort(c("1", "2", "3"))) &&
      !identical(sort(g$moves), sort(c("L", "R", "X")))) {
    return(list(valid = TRUE, path = path_candidate))
  }

  path_simplified <- short_position(path_candidate, n)
  if (is.null(path_simplified)) path_simplified <- character(0)

  final_path <- if (reached(path_simplified)) path_simplified else path_candidate

  return(list(valid = TRUE, path = final_path))
}
