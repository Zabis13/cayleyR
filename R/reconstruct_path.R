#' Reconstruct Full Path Through Cycle Chain
#'
#' Traces back through a chain of cycles to build the full operation path
#' from the initial state to a target state.
#'
#' @param reachable_states Data frame or Arrow Table of all explored states
#' @param start_state Integer vector, the root start state
#' @param target_state Integer vector, the target state
#' @param target_cycle Integer, cycle number containing the target
#' @param target_combo Integer, combo number within the target cycle
#' @param v_cols Character vector of V-column names
#' @return Character vector of operations, or NULL on error
#' @keywords internal
reconstruct_full_path <- function(reachable_states,
                                   start_state,
                                   target_state,
                                   target_cycle,
                                   target_combo,
                                   v_cols) {
  if (inherits(reachable_states, "ArrowTabular")) {
    reachable_states <- as.data.frame(reachable_states)
  }

  if (target_cycle == 0) {
    return(character(0))
  }

  full_path <- character(0)

  for (cyc in 1:target_cycle) {
    cycle_data <- reachable_states[reachable_states$cycle == cyc, ]

    initial_rows <- cycle_data[is.na(cycle_data$step), , drop = FALSE]
    if (nrow(initial_rows) == 0) {
      initial_rows <- cycle_data[!is.na(cycle_data$step) & cycle_data$step == 1, , drop = FALSE]
    }

    if (nrow(initial_rows) == 0) {
      message("Error: no initial state for cycle ", cyc)
      return(NULL)
    }

    cycle_initial_state <- as.integer(unlist(initial_rows[1, v_cols, drop = FALSE]))
    names(cycle_initial_state) <- NULL

    if (cyc > 1) {
      prev_cycle_data <- reachable_states[reachable_states$cycle == (cyc - 1), ]

      matching_rows <- apply(prev_cycle_data[, v_cols, drop = FALSE], 1, function(row) {
        all(row == cycle_initial_state)
      })

      if (!any(matching_rows)) {
        message("Error: start of cycle ", cyc, " not found in cycle ", cyc - 1)
        return(NULL)
      }

      match_row <- prev_cycle_data[which(matching_rows)[1], , drop = FALSE]
      combo_num <- match_row$combo_number
      target_step_val <- match_row$step

      combo_states <- prev_cycle_data[prev_cycle_data$combo_number == combo_num, ]
      combo_states <- combo_states[order(combo_states$step), ]

      if (!is.na(target_step_val)) {
        operations <- combo_states$operation[!is.na(combo_states$step) & combo_states$step < target_step_val]
      } else {
        operations <- character(0)
      }
      operations <- operations[!is.na(operations)]
      full_path <- c(full_path, operations)
    }

    if (cyc == target_cycle) {
      target_matches <- which(
        cycle_data$combo_number == target_combo &
          apply(cycle_data[, v_cols, drop = FALSE], 1, function(row) all(row == target_state))
      )

      if (length(target_matches) == 0) {
        message("Error: target_state not found in cycle ", target_cycle, " combo ", target_combo)
        return(NULL)
      }

      target_step_val <- cycle_data[target_matches[1], "step"]

      if (is.na(target_step_val)) {
        return(full_path)
      }

      combo_states <- cycle_data[cycle_data$combo_number == target_combo, ]
      combo_states <- combo_states[order(combo_states$step), ]

      operations <- combo_states$operation[!is.na(combo_states$step) & combo_states$step < target_step_val]
      operations <- operations[!is.na(operations)]
      full_path <- c(full_path, operations)
    }
  }

  return(full_path)
}

#' Process Start-type Intersection
#'
#' Handles an intersection where the meeting state equals the original start state.
#' Reconstructs path through the final (backward) search tree.
#'
#' @param intersection_state Integer vector, the intersecting state
#' @param reachable_states_final Data frame of backward-search states
#' @param bridge_states_final List of bridge states for backward search
#' @param final_index Hash index for backward states
#' @param v_cols Character vector of V-column names
#' @return List with `path` and `info`, or NULL
#' @keywords internal
process_start_intersection <- function(intersection_state, reachable_states_final,
                                        bridge_states_final, final_index, v_cols) {
  intersection_key <- paste(intersection_state, collapse = "_")
  match_indices <- final_index[[intersection_key]]
  if (is.null(match_indices)) return(NULL)

  match_in_final <- match_indices[1]
  info_final <- reachable_states_final[match_in_final, ]

  final_state <- bridge_states_final[[1]]$state

  path_full <- reconstruct_full_path(
    reachable_states_final,
    final_state,
    intersection_state,
    info_final$cycle,
    info_final$combo_number,
    v_cols
  )

  if (is.null(path_full)) return(NULL)

  path_candidate <- invert_path(path_full)

  return(list(
    path = path_candidate,
    info = list(
      start_combo = NA, start_step = NA,
      final_combo = info_final$combo_number, final_step = info_final$step
    )
  ))
}

#' Process Final-type Intersection
#'
#' Handles an intersection where the meeting state equals the original final state.
#' Reconstructs path through the start (forward) search tree.
#'
#' @param intersection_state Integer vector, the intersecting state
#' @param reachable_states_start Data frame of forward-search states
#' @param bridge_states_start List of bridge states for forward search
#' @param start_index Hash index for forward states
#' @param v_cols Character vector of V-column names
#' @return List with `path` and `info`, or NULL
#' @keywords internal
process_final_intersection <- function(intersection_state, reachable_states_start,
                                        bridge_states_start, start_index, v_cols) {
  intersection_key <- paste(intersection_state, collapse = "_")
  match_indices <- start_index[[intersection_key]]
  if (is.null(match_indices)) return(NULL)

  match_in_start <- match_indices[1]
  info_start <- reachable_states_start[match_in_start, ]

  start_state <- bridge_states_start[[1]]$state

  path_candidate <- reconstruct_full_path(
    reachable_states_start,
    start_state,
    intersection_state,
    info_start$cycle,
    info_start$combo_number,
    v_cols
  )

  if (is.null(path_candidate)) return(NULL)

  return(list(
    path = path_candidate,
    info = list(
      start_combo = info_start$combo_number, start_step = info_start$step,
      final_combo = NA, final_step = NA
    )
  ))
}

#' Process Intermediate Intersection
#'
#' Handles a general intersection found in both forward and backward
#' search trees. Combines paths from both directions.
#'
#' @param intersection_state Integer vector, the intersecting state
#' @param reachable_states_start Data frame of forward-search states
#' @param reachable_states_final Data frame of backward-search states
#' @param bridge_states_start List of bridge states for forward search
#' @param bridge_states_final List of bridge states for backward search
#' @param start_index Hash index for forward states
#' @param final_index Hash index for backward states
#' @param v_cols Character vector of V-column names
#' @return List with `path` and `info`, or NULL
#' @keywords internal
process_intermediate_intersection <- function(intersection_state,
                                              reachable_states_start, reachable_states_final,
                                              bridge_states_start, bridge_states_final,
                                              start_index, final_index, v_cols) {
  intersection_key <- paste(intersection_state, collapse = "_")
  match_indices_start <- start_index[[intersection_key]]
  match_indices_final <- final_index[[intersection_key]]

  if (is.null(match_indices_start) || is.null(match_indices_final)) return(NULL)

  match_in_start <- match_indices_start[1]
  match_in_final <- match_indices_final[1]

  info_start <- reachable_states_start[match_in_start, ]
  info_final <- reachable_states_final[match_in_final, ]

  start_state <- bridge_states_start[[1]]$state
  final_state <- bridge_states_final[[1]]$state

  path_start_full <- reconstruct_full_path(
    reachable_states_start,
    start_state,
    intersection_state,
    info_start$cycle,
    info_start$combo_number,
    v_cols
  )

  path_final_full <- reconstruct_full_path(
    reachable_states_final,
    final_state,
    intersection_state,
    info_final$cycle,
    info_final$combo_number,
    v_cols
  )

  if (is.null(path_start_full) || is.null(path_final_full)) return(NULL)

  path_final_inverted <- invert_path(path_final_full)
  path_candidate <- c(path_start_full, path_final_inverted)

  return(list(
    path = path_candidate,
    info = list(
      start_combo = info_start$combo_number, start_step = info_start$step,
      final_combo = info_final$combo_number, final_step = info_final$step
    )
  ))
}
