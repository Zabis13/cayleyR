# Suppress R CMD check NOTEs for data.table NSE variables
utils::globalVariables(c("state_key", "source_n", ".", "total_moves", "unique_states_count",
                         "source", ".N", ".SD"))

#' Calculate Manhattan Distances for All States
#'
#' Computes the Manhattan distance from a reference state to every row in
#' a table of reachable states, adds a `difference` column, and sorts by it.
#'
#' @param start_state Integer vector, the reference state
#' @param reachable_states_start Data frame or Arrow Table with V-columns
#' @param method Character, distance method (currently only "manhattan")
#' @param use_gpu Logical, use GPU acceleration via ggmlR if available (default FALSE)
#' @return Data frame sorted by difference (ascending)
#' @export
#' @examples
#' df <- data.frame(V1 = c(1, 2), V2 = c(2, 1))
#' calculate_differences(c(1, 2), df)
calculate_differences <- function(start_state, reachable_states_start,
                                  method = "manhattan", use_gpu = FALSE) {
  if (inherits(reachable_states_start, "ArrowTabular")) {
    reachable_states_start <- reachable_states_start$to_data_frame()
  }

  v_columns <- grep("^V", names(reachable_states_start), value = TRUE)
  start_state <- as.integer(start_state)
  mat <- as.matrix(reachable_states_start[, v_columns, drop = FALSE])

  if (use_gpu && cayley_gpu_available()) {
    differences <- calculate_differences_gpu(start_state, mat)
  } else {
    distance_func <- switch(
      method,
      "manhattan" = function(v1, v2) sum(abs(v1 - v2)),
      "breakpoints" = function(v1, v2) breakpoint_distance(v1, v2),
      stop("Unsupported method: ", method)
    )
    differences <- apply(mat, 1L, function(row) {
      distance_func(start_state, as.integer(row))
    })
  }

  result <- reachable_states_start
  result$difference <- differences
  result <- result[order(result$difference), ]
  result
}

#' Select Unique States by V-columns
#'
#' Removes duplicate rows based on state columns (V1, V2, ..., Vn).
#'
#' @param df Data frame, data.table, or Arrow Table
#' @return Object of same type with unique states
#' @export
#' @examples
#' df <- data.frame(V1 = c(1, 1, 2), V2 = c(2, 2, 1), op = c("a", "b", "c"))
#' select_unique(df)
select_unique <- function(df) {
  is_arrow <- inherits(df, "ArrowTabular")
  if (is_arrow) df <- as.data.frame(df)

  dt <- data.table::as.data.table(df)
  v_columns <- grep("^V", names(dt), value = TRUE)
  unique_dt <- unique(dt, by = v_columns)

  if (is_arrow) arrow::as_arrow_table(unique_dt) else unique_dt
}

#' Find Duplicate States Between Two Tables
#'
#' Identifies states that appear in both tables by comparing V-columns.
#' Used for finding intersections between forward and backward searches.
#'
#' @param df1 Data frame or Arrow Table (first set of states)
#' @param df2 Data frame or Arrow Table (second set of states)
#' @return Data frame of duplicate states with a `source` column, or NULL if none
#' @export
#' @examples
#' df1 <- data.frame(V1 = c(1, 2), V2 = c(2, 1))
#' df2 <- data.frame(V1 = c(2, 3), V2 = c(1, 2))
#' check_duplicates(df1, df2)
check_duplicates <- function(df1, df2) {
  if (inherits(df1, "ArrowTabular")) df1 <- df1$to_data_frame()
  if (inherits(df2, "ArrowTabular")) df2 <- df2$to_data_frame()

  dt1 <- data.table::as.data.table(df1)
  dt2 <- data.table::as.data.table(df2)

  v_columns <- grep("^V", names(dt1), value = TRUE)

  dt1[, source := "start"]
  dt2[, source := "finish"]

  merged_dt <- data.table::rbindlist(list(dt1, dt2), use.names = TRUE, fill = TRUE)

  merged_dt[, state_key := apply(.SD, 1L, function(row) paste(row, collapse = "_")),
            .SDcols = v_columns]

  state_counts <- merged_dt[, .(source_n = data.table::uniqueN(source)), by = state_key]
  true_keys <- state_counts[source_n >= 2L, state_key]

  if (length(true_keys) == 0L) return(NULL)

  result <- merged_dt[state_key %in% true_keys]
  if (nrow(result) == 0L) NULL else as.data.frame(result)
}

#' Save Bridge States to CSV
#'
#' Writes a list of bridge states (each with `state` and `cycle` fields)
#' to a CSV file.
#'
#' @param bridge_states List of lists, each containing `state` (integer vector)
#'   and `cycle` (integer)
#' @param filename Character, output CSV file path
#' @return Invisible NULL. Side effect: writes a CSV file.
#' @export
#' @examples
#' bs <- list(
#'   list(state = 1:5, cycle = 0),
#'   list(state = c(2, 1, 3, 4, 5), cycle = 1)
#' )
#' # save_bridge_states(bs, tempfile(fileext = ".csv"))
save_bridge_states <- function(bridge_states, filename) {
  output_all <- data.frame()

  for (i in seq_along(bridge_states)) {
    current_state <- bridge_states[[i]]$state
    current_cycle <- bridge_states[[i]]$cycle

    output <- as.data.frame(matrix(current_state, nrow = 1))
    names(output) <- paste0("V", 1:ncol(output))
    output$cycle <- current_cycle

    output_all <- rbind(output_all, output)
  }

  write.table(output_all, file = filename, sep = ",", row.names = FALSE)
  invisible(NULL)
}

#' Filter Middle States
#'
#' Removes the first and last steps from each combo within cycle data,
#' keeping only middle states.
#'
#' @param data Data frame or Arrow Table with step and combo_number columns
#' @param skip_first Integer, number of initial steps to skip per combo
#' @param skip_last Integer, number of final steps to skip per combo
#' @return Data frame with filtered states
#' @keywords internal
filter_middle_states <- function(data, skip_first = 2, skip_last = 2) {
  if (nrow(data) == 0) return(data)

  if (inherits(data, "ArrowTabular")) {
    data <- data$to_data_frame()
  }

  has_step <- !is.na(data$step)
  max_steps <- tapply(data$step[has_step], data$combo_number[has_step], max)

  mask <- has_step &
    data$step > skip_first &
    data$step <= (max_steps[as.character(data$combo_number)] - skip_last)

  data[mask, ]
}

#' Find Best Match State
#'
#' Finds the state in a table that has the minimum Manhattan distance to
#' a target state. If multiple states tie, selects the one with the
#' smallest step number.
#'
#' @param target_state Integer vector, the target state
#' @param reachable_states Data frame or Arrow Table with V-columns
#' @return Single-row data frame of the best matching state
#' @keywords internal
find_best_match_state <- function(target_state, reachable_states, method = "manhattan",
                                  use_gpu = FALSE) {
  if (inherits(reachable_states, "ArrowTabular")) {
    reachable_states <- as.data.frame(reachable_states)
  }

  if (is.environment(reachable_states)) {
    stop("reachable_states cannot be an environment. Pass a data.frame or Arrow Table.")
  }

  if (!is.data.frame(reachable_states)) {
    reachable_states <- as.data.frame(reachable_states)
  }

  v_cols <- grep("^V[0-9]+$", colnames(reachable_states), value = TRUE)

  distance_func <- switch(
    method,
    "manhattan" = function(v1, v2) sum(abs(v1 - v2)),
    "breakpoints" = function(v1, v2) breakpoint_distance(v1, v2),
    stop("Unsupported method: ", method)
  )

  if (use_gpu && method == "manhattan" &&
      tryCatch(cayley_gpu_available(), error = function(e) FALSE)) {
    mat <- as.matrix(reachable_states[, v_cols, drop = FALSE])
    dist_per_row <- tryCatch(
      calculate_differences_gpu(as.integer(target_state), mat),
      error = function(e) NULL
    )
  } else {
    dist_per_row <- NULL
  }

  if (is.null(dist_per_row)) {
    dist_per_row <- apply(reachable_states[, v_cols, drop = FALSE], 1, function(row) {
      distance_func(target_state, as.integer(row))
    })
  }

  reachable_states$distance <- dist_per_row
  min_dist <- min(dist_per_row)
  best_states <- reachable_states[dist_per_row == min_dist, , drop = FALSE]

  if (nrow(best_states) > 1) {
    best_state <- best_states[which.min(best_states$step), , drop = FALSE]
  } else {
    best_state <- best_states[1, , drop = FALSE]
  }

  return(best_state)
}

#' Select New Bridge State
#'
#' Selects a new state from candidate states that is close to an opposite
#' state (by Manhattan distance). Randomly picks from the top 10 closest.
#'
#' @param target_all Data frame of candidate states
#' @param opposite_state Integer vector, the state to be close to
#' @return Integer vector of the selected state
#' @keywords internal
select_new_state <- function(target_all, opposite_state, method = "manhattan") {
  if (nrow(target_all) == 0) {
    stop("No states available for selection")
  }

  result_manhattan <- calculate_differences(opposite_state, target_all, method = method)

  n_top <- min(10, nrow(result_manhattan))
  top10 <- result_manhattan[order(result_manhattan$difference), ][1:n_top, , drop = FALSE]

  random_row <- top10[sample(1:nrow(top10), 1), , drop = FALSE]

  v_cols <- grep("^V[0-9]+$", colnames(random_row), value = TRUE)
  new_state <- as.integer(random_row[, v_cols])
  names(new_state) <- NULL

  return(new_state)
}
