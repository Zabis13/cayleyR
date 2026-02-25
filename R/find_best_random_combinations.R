#' Find Best Random Operation Sequences
#'
#' Generates random sequences of operations and evaluates their cycle lengths
#' to find sequences that produce the longest cycles in the Cayley graph.
#' Uses data.table for efficient storage.
#'
#' @param moves Character vector of allowed operation symbols (e.g., c("1", "2", "3") or c("L", "R", "X"))
#' @param combo_length Integer, length of each operation sequence to test
#' @param n_samples Integer, number of random sequences to generate and test
#' @param n_top Integer, number of top results to return (sorted by cycle length)
#' @param start_state Integer vector, initial permutation state
#' @param k Integer, parameter for reverse operations
#' @return data.table with columns:
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
  results <- data.table::data.table(
    combo_number = integer(0),
    combination = character(0),
    total_moves = integer(0),
    unique_states_count = integer(0)
  )

  unique_combos <- character(0)
  count <- 0
  max_iter <- n_samples * 10

  while (count < n_samples && max_iter > 0) {
    combo <- sample(moves, size = combo_length, replace = TRUE)
    key <- paste(combo, collapse = "")

    if (key %in% unique_combos) {
      max_iter <- max_iter - 1
      next
    }
    unique_combos <- c(unique_combos, key)

    res <- tryCatch({
      get_reachable_states_light(start_state, combo, k)
    }, error = function(e) NULL)

    if (!is.null(res)) {
      results <- data.table::rbindlist(list(results,
        data.table::data.table(
          combo_number = count + 1L,
          combination = key,
          total_moves = as.integer(res$total_moves),
          unique_states_count = as.integer(res$unique_states_count)
        )), use.names = TRUE)
      count <- count + 1
    }
    max_iter <- max_iter - 1
  }

  if (nrow(results) == 0) {
    warning("No successful results found.")
    return(results)
  }

  top_results <- results[order(-total_moves, -unique_states_count)][1:min(n_top, .N)]

  return(top_results)
}
