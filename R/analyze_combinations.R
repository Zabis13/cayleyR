#' Analyze Top Operation Combinations
#'
#' For each combination in a data frame of top results, runs a full cycle
#' analysis and collects all states with their celestial coordinates into
#' a single Arrow Table.
#'
#' @param top_combos Data frame or data.table with a `combination` column
#'   (string of operation digits, e.g., "132")
#' @param start_state Integer vector, the initial permutation state
#' @param k Integer, parameter for reverse operations
#' @return Arrow Table with columns V1..Vn, operation, step, combo_number,
#'   nL, nR, nX, theta, phi, omega_conformal
#' @export
#' @examples
#' combos <- data.frame(combination = c("13", "23"), stringsAsFactors = FALSE)
#' # result <- analyze_top_combinations(combos, 1:10, k = 4)
analyze_top_combinations <- function(top_combos, start_state, k) {
  n <- length(start_state)

  all_states <- data.table::data.table()

  for (i in 1:nrow(top_combos)) {
    allowed_positions <- unlist(strsplit(top_combos$combination[i], ""))
    current_state <- as.integer(start_state)
    current_coords <- NULL

    states_list <- list(current_state)
    coords_list <- list(list(nL = 0, nR = 0, nX = 0, theta = 0, phi = 0, omega_conformal = 0))
    operations_list <- character(0)
    step <- 0

    repeat {
      for (op in allowed_positions) {
        result <- apply_operations(current_state, op, k, current_coords)
        current_state <- result$state
        current_coords <- result$coords

        states_list[[length(states_list) + 1]] <- current_state
        coords_list[[length(coords_list) + 1]] <- current_coords
        operations_list <- c(operations_list, op)
        step <- step + 1
        if (identical(current_state, as.integer(start_state)) && step > 0) break
      }
      if (identical(current_state, as.integer(start_state)) && step > 0) break
    }

    states_mat <- do.call(rbind, states_list)
    ops_final <- c(operations_list, NA_character_)
    steps_final <- c(seq_len(step), NA_integer_)

    nL_vec <- vapply(coords_list, function(x) as.numeric(x$nL), numeric(1))
    nR_vec <- vapply(coords_list, function(x) as.numeric(x$nR), numeric(1))
    nX_vec <- vapply(coords_list, function(x) as.numeric(x$nX), numeric(1))
    theta_vec <- vapply(coords_list, function(x) as.numeric(x$theta), numeric(1))
    phi_vec <- vapply(coords_list, function(x) as.numeric(x$phi), numeric(1))
    omega_vec <- vapply(coords_list, function(x) as.numeric(x$omega_conformal), numeric(1))

    temp_dt <- data.table::as.data.table(states_mat)
    colnames(temp_dt)[1:n] <- paste0("V", 1:n)
    temp_dt[, `:=`(
      operation = ops_final,
      step = steps_final,
      combo_number = i,
      nL = nL_vec,
      nR = nR_vec,
      nX = nX_vec,
      theta = theta_vec,
      phi = phi_vec,
      omega_conformal = omega_vec
    )]

    all_states <- data.table::rbindlist(list(all_states, temp_dt), use.names = TRUE, fill = TRUE)
  }

  v_cols <- paste0("V", 1:n)
  data.table::setcolorder(all_states, c(v_cols, "operation", "step", "combo_number",
                                         "nL", "nR", "nX", "theta", "phi", "omega_conformal"))

  arrow::as_arrow_table(all_states)
}
