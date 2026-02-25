#' Invert a Path of Operations
#'
#' Reverses and inverts a sequence of operations. "1" (shift left) becomes
#' "2" (shift right) and vice versa. "3" (reverse) stays the same.
#'
#' @param path Character vector of operations
#' @return Character vector of inverted operations in reverse order
#' @export
#' @examples
#' invert_path(c("1", "3", "2"))
#' invert_path(c("1", "1", "3"))
invert_path <- function(path) {
  if (length(path) == 0) return(character(0))

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
    optimized <- character(0)
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
          optimized <- c(optimized, rep(op, residual))
        }
        i <- i + count
      } else {
        optimized <- c(optimized, op)
        i <- i + 1
      }
    }
    allowed_positions <- optimized

    if (length(allowed_positions) == 0) break

    # Step 1: remove adjacent inverse pairs
    i <- 1
    cleaned <- character(0)
    while (i <= length(allowed_positions)) {
      if (i < length(allowed_positions)) {
        curr <- allowed_positions[i]
        next_op <- allowed_positions[i + 1]

        if ((curr == "1" && next_op == "2") ||
            (curr == "2" && next_op == "1") ||
            (curr == "3" && next_op == "3")) {
          i <- i + 2
          next
        }
      }
      cleaned <- c(cleaned, allowed_positions[i])
      i <- i + 1
    }
    allowed_positions <- cleaned

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

    allowed_positions <- unlist(new_blocks)

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
#' @param path_candidate Character vector of operations
#' @param start_state Integer vector, start state
#' @param final_state Integer vector, target state
#' @param k Integer, parameter for reverse operations
#' @return List with components:
#'   \item{valid}{Logical, whether the path is valid}
#'   \item{path}{Simplified or original path, or NULL if invalid}
#' @export
#' @examples
#' res <- validate_and_simplify_path(c("1", "3"), 1:5, c(5, 2, 3, 4, 1), k = 2)
#' res$valid
validate_and_simplify_path <- function(path_candidate, start_state, final_state, k) {
  if (is.null(path_candidate)) {
    return(list(valid = FALSE, path = NULL))
  }

  if (length(path_candidate) == 0) {
    return(list(valid = TRUE, path = path_candidate))
  }

  n <- length(start_state)

  test_before <- tryCatch({
    result <- apply_operations(start_state, path_candidate, k)
    test_state <- result$state
    identical(as.integer(test_state), as.integer(final_state))
  }, error = function(e) FALSE)

  if (!test_before) {
    return(list(valid = FALSE, path = NULL))
  }

  path_simplified <- short_position(path_candidate, n)

  test_after <- tryCatch({
    result <- apply_operations(start_state, path_simplified, k)
    test_state <- result$state
    identical(as.integer(test_state), as.integer(final_state))
  }, error = function(e) FALSE)

  final_path <- if (test_after) path_simplified else path_candidate

  return(list(valid = TRUE, path = final_path))
}
