#' Find Best Random Operation Sequences
#'
#' Generates random sequences of operations and evaluates their cycle lengths
#' to find sequences that produce the longest cycles in the Cayley graph.
#' Useful for discovering interesting operation sequences in permutation puzzles.
#'
#' @param moves Character vector of allowed operation symbols (e.g., c("1", "2", "3") or c("L", "R", "X"))
#' @param combo_length Integer, length of each operation sequence to test
#' @param n_samples Integer, number of random sequences to generate and test
#' @param n_top Integer, number of top results to return (sorted by cycle length)
#' @param start_state Integer vector, initial permutation state
#' @param k Integer, parameter for reverse operations
#' @details
#' The returned data frame \code{reachable_states_df} has the following structure:
#' \preformatted{
#' step | state           | operation
#' -----|-----------------|----------
#' 1    | 1 2 3 4 5...    | 1         <- INITIAL, operation="1" (next operation)
#' 2    | 2 3 4 5 6...    | 1         <- after applying op1 from row 1
#' 3    | 3 4 5 6 7...    | 2         <- after applying op1 from row 2
#' 4    | 4 5 6 7 8...    | 3         <- after applying op2 from row 3
#' 5    | 5 6 7 8 9...    | 1         <- after applying op3 from row 4
#' ...
#' 20   | 20 1 2 3 4...   | 1         <- after applying all ops from rows 1-19
#' NA   | 1 2 3 4 5...    | NA        <- after applying op1 from row 20 = initial
#' }
#' @return Data frame with columns:
#'   \item{combination}{String representation of the operation sequence}
#'   \item{total_moves}{Cycle length for this sequence}
#'   \item{unique_states_count}{Number of unique states visited in the cycle}
#' @export
#' @examples
#' # Find top 10 sequences from 100 random samples
#' best <- find_best_random_combinations(
#'   moves = c("1", "2", "3"),
#'   combo_length = 10,
#'   n_samples = 100,
#'   n_top = 10,
#'   start_state = 1:10,
#'   k = 4
#' )
#' print(best)
#'
#' # Quick search with letter codes
#' top5 <- find_best_random_combinations(
#'   moves = c("L", "R", "X"),
#'   combo_length = 5,
#'   n_samples = 100,
#'   n_top = 5,
#'   start_state = 1:10,
#'   k = 3
#' )
#' print(top5)
find_best_random_combinations <- function(moves,
                                          combo_length,
                                          n_samples,
                                          n_top,
                                          start_state,
                                          k) {
  results <- data.frame(
    combination = character(0),
    total_moves = integer(0),
    unique_states_count = integer(0),
    stringsAsFactors = FALSE
  )
  unique_combos <- new.env(hash = TRUE)
  count <- 0
  max_iter <- n_samples * 10

  while (count < n_samples && max_iter > 0) {
    combo <- sample(moves, size = combo_length, replace = TRUE)
    key <- paste(combo, collapse = "")

    if (exists(key, envir = unique_combos)) {
      max_iter <- max_iter - 1
      next
    }
    unique_combos[[key]] <- TRUE

    res <- tryCatch({
      get_reachable_states_light(start_state = start_state,
                                 allowed_positions = combo,
                                 k = k)
    }, error = function(e) {
      message(sprintf("Error for combination %s: %s", key, e$message))
      return(NULL)
    })

    if (!is.null(res)) {
      results[nrow(results) + 1, ] <- list(
        combination = key,
        total_moves = as.integer(res$total_moves),
        unique_states_count = as.integer(res$unique_states_count)
      )
      count <- count + 1
    }
    max_iter <- max_iter - 1
  }

  if (nrow(results) == 0) {
    warning("No successful results found.")
    return(results)
  }

  results_sorted <- results[order(-results$total_moves, -results$unique_states_count), ]
  top_n <- min(n_top, nrow(results_sorted))
  top_results <- results_sorted[1:top_n, , drop = FALSE]

  return(top_results)
}
