#' Bidirectional BFS Shortest Path
#'
#' Finds the shortest path between two permutation states using
#' bidirectional breadth-first search. Expands from both the start
#' and goal states simultaneously, meeting in the middle.
#'
#' The backward half of the search walks moves in reverse, so the path it
#' returns may name moves outside `moves`: searching with left shifts alone
#' still reports the right shift that undoes one. Inverses are taken from the
#' group's whole alphabet for that reason, and a move the group cannot undo at
#' all is an error rather than a silently wrong path.
#'
#' @param n Integer, size of the permutation
#' @param state1 Integer vector, start state
#' @param state2 Integer vector, goal state
#' @param max_level Integer, maximum BFS depth in each direction
#' @param moves Allowed operations, naming a subset of the group's alphabet
#'   (default: all of it)
#' @param k Integer, parameter for reverse operations. Ignored when `group` is
#'   given.
#' @param group A \code{\link{perm_group}}. When `NULL` (default) the arguments
#'   describe TopSpin.
#' @return Character vector of operations forming the shortest path,
#'   or NULL if no path found within max_level
#' @export
#' @seealso \code{\link{perm_group}}
#' @examples
#' # Find path between two small states
#' path <- bidirectional_bfs(5, 1:5, c(2, 3, 4, 5, 1), max_level = 5,
#'                           moves = c("1", "2", "3"), k = 3)
#' path
#'
#' # the same search on a cube: undo a three-move scramble
#' g <- cube_group(3)
#' s <- group_apply(g, group_identity(g), "R U F")
#' bidirectional_bfs(54, s, group_identity(g), max_level = 4, group = g)
bidirectional_bfs <- function(n, state1, state2, max_level, moves = NULL,
                              k = NULL, group = NULL) {
  state1 <- as.integer(state1)
  state2 <- as.integer(state2)

  # The group is built over its full alphabet even when the search is
  # restricted, because the backward half needs inverses that `moves` may not
  # itself contain.
  res <- resolve_group(group, length(state1), k, NULL)
  g <- res$group
  moves <- if (is.null(moves)) g$moves else g$moves[group_move_index(g, moves)]

  # The backward search needs each move it uses to be undoable somewhere in the
  # group, not necessarily within the subset being searched: a search over left
  # shifts alone still has to spell the right shift that reverses one.
  inv_idx <- g$inverse_of[res$moves]
  if (anyNA(inv_idx)) {
    stop("bidirectional_bfs: move(s) ",
         paste(moves[is.na(inv_idx)], collapse = ", "),
         " have no inverse anywhere in group '", g$name,
         "'; the backward search cannot run")
  }
  inv_name <- stats::setNames(g$moves[inv_idx], moves)

  state_key <- function(s) paste0(s, collapse = "_")

  invert_move <- function(op) unname(inv_name[[op]])

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
          new_state <- group_apply(g, node$state, move)
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
          new_state <- group_apply(g, node$state, move)
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
