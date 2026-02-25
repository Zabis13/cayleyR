#' Bidirectional BFS Shortest Path
#'
#' Finds the shortest path between two permutation states using
#' bidirectional breadth-first search. Expands from both the start
#' and goal states simultaneously, meeting in the middle.
#'
#' @param n Integer, size of the permutation
#' @param state1 Integer vector, start state
#' @param state2 Integer vector, goal state
#' @param max_level Integer, maximum BFS depth in each direction
#' @param moves Character vector, allowed operations (e.g., c("1", "2", "3"))
#' @param k Integer, parameter for reverse operations
#' @return Character vector of operations forming the shortest path,
#'   or NULL if no path found within max_level
#' @export
#' @examples
#' # Find path between two small states
#' path <- bidirectional_bfs(5, 1:5, c(2, 3, 4, 5, 1), max_level = 5,
#'                           moves = c("1", "2", "3"), k = 3)
#' path
bidirectional_bfs <- function(n, state1, state2, max_level, moves, k) {
  state_key <- function(s) paste0(s, collapse = "_")

  invert_move <- function(op) {
    if (op == "1") return("2")
    if (op == "2") return("1")
    if (op == "3") return("3")
    return(op)
  }

  reconstruct_path <- function(visited, end_key) {
    path <- character()
    cur <- end_key
    while (!is.null(visited[[cur]]$move)) {
      path <- c(visited[[cur]]$move, path)
      cur <- visited[[cur]]$parent
    }
    path
  }

  fwd_visited <- new.env(hash = TRUE)
  bwd_visited <- new.env(hash = TRUE)

  start_key <- state_key(state1)
  goal_key <- state_key(state2)

  if (start_key == goal_key) return(character(0))

  fwd_visited[[start_key]] <- list(parent = NULL, move = NULL, state = state1)
  bwd_visited[[goal_key]] <- list(parent = NULL, move = NULL, state = state2)

  fwd_queue <- list(list(state = state1, key = start_key))
  bwd_queue <- list(list(state = state2, key = goal_key))

  fwd_level <- 1
  bwd_level <- 1

  repeat {
    if (length(fwd_queue) == 0 && length(bwd_queue) == 0) break
    if (fwd_level > max_level && bwd_level > max_level) break

    # Expand forward
    if (length(fwd_queue) > 0 && fwd_level <= max_level) {
      next_fwd <- list()
      for (i in seq_along(fwd_queue)) {
        node <- fwd_queue[[i]]
        for (move in moves) {
          result <- tryCatch(apply_operations(node$state, move, k), error = function(e) NULL)
          if (is.null(result)) next
          new_state <- result$state
          new_key <- state_key(new_state)

          if (!exists(new_key, envir = fwd_visited)) {
            fwd_visited[[new_key]] <- list(parent = node$key, move = move, state = new_state)
            next_fwd[[length(next_fwd) + 1]] <- list(state = new_state, key = new_key)
          }
        }
      }
      fwd_queue <- next_fwd
      fwd_level <- fwd_level + 1

      for (new_node in fwd_queue) {
        if (exists(new_node$key, envir = bwd_visited)) {
          meet_key <- new_node$key
          path_fwd <- reconstruct_path(fwd_visited, meet_key)
          path_bwd <- reconstruct_path(bwd_visited, meet_key)
          path_bwd_inv <- vapply(rev(path_bwd), invert_move, character(1))
          result <- c(path_fwd, path_bwd_inv)
          names(result) <- NULL
          return(result)
        }
      }
    }

    # Expand backward
    if (length(bwd_queue) > 0 && bwd_level <= max_level) {
      next_bwd <- list()
      for (i in seq_along(bwd_queue)) {
        node <- bwd_queue[[i]]
        for (move in moves) {
          result <- tryCatch(apply_operations(node$state, move, k), error = function(e) NULL)
          if (is.null(result)) next
          new_state <- result$state
          new_key <- state_key(new_state)

          if (!exists(new_key, envir = bwd_visited)) {
            bwd_visited[[new_key]] <- list(parent = node$key, move = move, state = new_state)
            next_bwd[[length(next_bwd) + 1]] <- list(state = new_state, key = new_key)
          }
        }
      }
      bwd_queue <- next_bwd
      bwd_level <- bwd_level + 1

      for (new_node in bwd_queue) {
        if (exists(new_node$key, envir = fwd_visited)) {
          meet_key <- new_node$key
          path_fwd <- reconstruct_path(fwd_visited, meet_key)
          path_bwd <- reconstruct_path(bwd_visited, meet_key)
          path_bwd_inv <- vapply(rev(path_bwd), invert_move, character(1))
          result <- c(path_fwd, path_bwd_inv)
          names(result) <- NULL
          return(result)
        }
      }
    }
  }

  message("Path not found within max_level = ", max_level)
  return(NULL)
}
