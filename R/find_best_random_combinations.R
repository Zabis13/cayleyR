#' Find Best Random Operation Sequences
#'
#' Generates random sequences of operations and evaluates their cycle lengths
#' to find sequences that produce the longest cycles in the Cayley graph.
#' Uses C++ with OpenMP for parallel evaluation of combinations.
#'
#' @param moves Character vector of allowed operation symbols (e.g., c("1", "2", "3") or c("L", "R", "X"))
#' @param combo_length Integer, length of each operation sequence to test
#' @param n_samples Integer, number of random sequences to generate and test
#' @param n_top Integer, number of top results to return (sorted by cycle length)
#' @param start_state Integer vector, initial permutation state
#' @param k Integer, parameter for reverse operations
#' @return Data frame with columns:
#'   \item{combo_number}{Integer sequence number}
#'   \item{combination}{String representation of the operation sequence}
#'   \item{total_moves}{Cycle length for this sequence}
#'   \item{unique_states_count}{Number of unique states visited in the cycle}
#' @export
#' @examples
#' best <- find_best_random_combinations(
#'   moves = c("1", "2", "3"),
#'   combo_length = 10,
#'   n_samples = 50,
#'   n_top = 5,
#'   start_state = 1:10,
#'   k = 4
#' )
#' print(best)
find_best_random_combinations <- function(moves,
                                          combo_length,
                                          n_samples,
                                          n_top,
                                          start_state,
                                          k) {

  res <- find_best_random_combinations_cpp(
    as.integer(start_state),
    as.integer(k),
    as.character(moves),
    as.integer(combo_length),
    as.integer(n_samples)
  )

  if (length(res$combination) == 0) {
    warning("No successful results found.")
    return(data.frame(
      combo_number = integer(0),
      combination = character(0),
      total_moves = integer(0),
      unique_states_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- data.frame(
    combo_number = seq_along(res$combination),
    combination = as.character(res$combination),
    total_moves = as.integer(res$total_moves),
    unique_states_count = as.integer(res$unique_states_count),
    stringsAsFactors = FALSE
  )

  results <- results[order(-results$total_moves, -results$unique_states_count), ]
  top_results <- results[seq_len(min(n_top, nrow(results))), , drop = FALSE]
  rownames(top_results) <- NULL
  return(top_results)
}
