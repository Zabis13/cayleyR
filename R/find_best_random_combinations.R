#' Find Best Random Operation Sequences
#'
#' Generates random sequences of operations and evaluates their cycle lengths
#' to find sequences that produce the best cycles in the Cayley graph.
#' Uses C++ with OpenMP for parallel evaluation of combinations.
#'
#' @param moves Character vector of allowed operation symbols (e.g., c("1", "2", "3") or c("L", "R", "X"))
#' @param combo_length Integer, length of each operation sequence to test
#' @param n_samples Integer, number of random sequences to generate and test
#' @param n_top Integer, number of top results to return
#' @param start_state Integer vector, initial permutation state
#' @param k Integer, parameter for reverse operations
#' @param sort_by Character vector of sorting criteria, applied in order.
#'   Available criteria:
#'   \describe{
#'     \item{"longest"}{Prefer longer cycles (descending total_moves)}
#'     \item{"shortest"}{Prefer shorter cycles (ascending total_moves)}
#'     \item{"most_unique"}{Prefer more unique states (descending unique_states_count)}
#'     \item{"least_unique"}{Prefer fewer unique states (ascending unique_states_count)}
#'     \item{"most_repeated"}{Prefer higher repetition ratio total_moves/unique_states_count (descending)}
#'     \item{"least_repeated"}{Prefer lower repetition ratio (ascending)}
#'   }
#'   Default: \code{c("longest", "most_unique")} (original behavior).
#' @return Data frame with columns:
#'   \item{combo_number}{Integer sequence number}
#'   \item{combination}{String representation of the operation sequence}
#'   \item{total_moves}{Cycle length for this sequence}
#'   \item{unique_states_count}{Number of unique states visited in the cycle}
#'   \item{repetition_ratio}{Ratio total_moves / unique_states_count}
#' @export
#' @examples
#' # Default: longest cycles
#' best <- find_best_random_combinations(
#'   moves = c("1", "2", "3"),
#'   combo_length = 10,
#'   n_samples = 50,
#'   n_top = 5,
#'   start_state = 1:10,
#'   k = 4
#' )
#'
#' # Short cycles with many unique states
#' best2 <- find_best_random_combinations(
#'   moves = c("1", "2", "3"),
#'   combo_length = 10,
#'   n_samples = 50,
#'   n_top = 5,
#'   start_state = 1:10,
#'   k = 4,
#'   sort_by = c("shortest", "most_unique")
#' )
find_best_random_combinations <- function(moves,
                                          combo_length,
                                          n_samples,
                                          n_top,
                                          start_state,
                                          k,
                                          sort_by = c("longest", "most_unique")) {

  valid_criteria <- c("longest", "shortest", "most_unique", "least_unique",
                       "most_repeated", "least_repeated")
  bad <- setdiff(sort_by, valid_criteria)
  if (length(bad) > 0) {
    stop("Unknown sort_by criteria: ", paste(bad, collapse = ", "),
         ". Valid: ", paste(valid_criteria, collapse = ", "))
  }

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
      repetition_ratio = numeric(0),
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
  results$repetition_ratio <- results$total_moves / pmax(results$unique_states_count, 1L)

  # Build sort order from criteria vector
  sort_args <- lapply(sort_by, function(criterion) {
    switch(criterion,
      "longest"        = -results$total_moves,
      "shortest"       =  results$total_moves,
      "most_unique"    = -results$unique_states_count,
      "least_unique"   =  results$unique_states_count,
      "most_repeated"  = -results$repetition_ratio,
      "least_repeated" =  results$repetition_ratio
    )
  })

  ord <- do.call(order, sort_args)
  results <- results[ord, ]
  top_results <- results[seq_len(min(n_top, nrow(results))), , drop = FALSE]
  rownames(top_results) <- NULL
  return(top_results)
}
