#' Shorten a Path by Cutting Across Cycles
#'
#' Shortens an existing path by looking for cycles that leave it and rejoin it
#' further along. A cycle here is a combo word (say \code{"1231"}) applied round
#' and round until the state returns to where the cycle started -- the same
#' construction \code{\link{find_path_iterative}} searches with. If some state
#' along that loop also occurs later in the path, the stretch between the two
#' meeting points can be replaced by the stretch of the loop, and whenever the
#' loop covers it in fewer operations the path gets shorter.
#'
#' This complements \code{\link{short_path_bfs}} rather than replacing it. That
#' one sweeps the BFS neighbourhood a few steps deep, so it finds cuts between
#' nearby points; a cycle wanders hundreds of steps from where it started and
#' reaches rejoin points BFS depth cannot see. Running one after the other is
#' reasonable.
#'
#' Points are processed one at a time, and a cut is applied the moment it is
#' found: every later point is then searched against the already-shortened
#' path. This is why the points are carried as states rather than as positions
#' -- a cut renumbers everything downstream of it, but a state is either still
#' somewhere in the path or it is not. Points swallowed by an earlier cut are
#' simply skipped.
#'
#' @param path Character vector of operations ("1"/"2"/"3")
#' @param start_state Integer vector, the state the path starts from
#' @param k Integer, length of the reverse-prefix (flipper) operation
#' @param n_points Integer, how many places along the path to try. Points are
#'   spread evenly with a random jitter; cost grows linearly with this, path
#'   coverage does not (default 20)
#' @param moves Character vector, operations the combos are drawn from
#'   (default c("1", "2", "3"))
#' @param combo_length Integer, length of each sampled combo word (default 20)
#' @param n_samples Integer, combos sampled per point before ranking (default 200)
#' @param n_top Integer, top-ranked combos actually spun into cycles per point
#'   (default 5)
#' @param sort_by Character vector, ranking criteria for the combos, applied in
#'   order. One or more of "longest", "shortest", "most_unique",
#'   "least_unique", "most_repeated", "least_repeated", as in
#'   \code{\link{find_best_random_combinations}}. Pass \code{NULL} to skip
#'   ranking altogether and simply take the first \code{n_top} combos as drawn:
#'   ranking has to unroll every one of the \code{n_samples} candidates to
#'   score it, so turning it off is markedly cheaper, at the price of picking
#'   blind. Default c("longest", "most_unique")
#' @param max_cycle_len Integer, cap on how far a single cycle is unrolled.
#'   Some combos close only after hundreds of thousands of steps. Capped at the
#'   current path length regardless: a detour longer than the whole path can
#'   never win (default 20000)
#' @param n_threads Integer, OpenMP threads used to score and search combos.
#'   Defaults to two below the core count, leaving room for the rest of the
#'   machine; pass a number to fix it, or \code{NA} to use whatever OpenMP
#'   would pick by itself. Points remain sequential whatever this is set to --
#'   each searches the path the previous one shortened.
#' @param verbose Logical, print progress (default FALSE)
#' @return List with components:
#'   \item{path}{Character vector, the shortened path (the original if nothing
#'     was found or verification failed)}
#'   \item{original_length}{Integer, length of the input path}
#'   \item{new_length}{Integer, length of the returned path}
#'   \item{savings}{Integer, operations removed}
#'   \item{n_cuts}{Integer, how many cuts were applied}
#'   \item{cuts}{Data frame of the applied cuts, in the order they were applied:
#'     \code{from}, \code{to}, \code{cycle_len}, \code{gain} (positions refer to
#'     the path as it stood when that cut was made)}
#' @export
#' @seealso \code{\link{short_path_bfs}}, \code{\link{human_algorithm_to}},
#'   \code{\link{find_path_iterative}}
#' @examples
#' set.seed(1)
#' s <- generate_state(20, k = 4, n_moves = 300)
#' res <- human_algorithm_to(s, k = 4, simplify = FALSE)
#' cut <- cycle_shortcut(res$path, s, k = 4, n_points = 5)
#' cut$savings
cycle_shortcut <- function(path, start_state, k,
                           n_points = 20L,
                           moves = c("1", "2", "3"),
                           combo_length = 20L,
                           n_samples = 200L,
                           n_top = 5L,
                           sort_by = c("longest", "most_unique"),
                           max_cycle_len = 20000L,
                           n_threads = NULL,
                           verbose = FALSE) {
  path <- as.character(path)
  start_state <- as.integer(start_state)
  k <- as.integer(k)
  n_points <- as.integer(n_points)
  N <- length(path)

  empty_cuts <- data.frame(from = integer(0), to = integer(0),
                           cycle_len = integer(0), gain = integer(0))
  unchanged <- list(path = path, original_length = N, new_length = N,
                    savings = 0L, n_cuts = 0L, cuts = empty_cuts)

  if (N < 3L || n_points < 1L) return(unchanged)

  # Points are picked once, on the original path, evenly spread with a jitter
  # bounded by the grid step so they stay spread out instead of clumping.
  step <- N / (n_points + 1L)
  pos <- as.integer(round(step * seq_len(n_points) +
                            stats::runif(n_points, -step / 2, step / 2)))
  pos <- unique(sort(pos[pos >= 1L & pos <= N - 2L]))
  if (length(pos) == 0L) return(unchanged)

  sort_codes <- c(longest = 1L, shortest = 2L, most_unique = 3L,
                  least_unique = 4L, most_repeated = 5L, least_repeated = 6L)
  sort_ids <- if (is.null(sort_by)) integer(0) else {
    unname(sort_codes[as.character(sort_by)])
  }
  if (anyNA(sort_ids)) {
    stop("cycle_shortcut: unknown sort_by criteria: ",
         paste(setdiff(as.character(sort_by), names(sort_codes)), collapse = ", "),
         ". Valid: ", paste(names(sort_codes), collapse = ", "))
  }

  if (is.null(n_threads)) {
    n_threads <- max(1L, openmp_threads() - 2L)
  } else if (is.na(n_threads)) {
    n_threads <- openmp_threads()
  } else {
    n_threads <- max(1L, as.integer(n_threads))
  }

  codes <- c("1" = 1L, "2" = 2L, "3" = 3L, "L" = 1L, "R" = 2L, "X" = 3L)
  path_codes <- unname(codes[path])
  if (anyNA(path_codes)) stop("cycle_shortcut: path holds unknown operations")
  move_codes <- unique(unname(codes[as.character(moves)]))
  if (anyNA(move_codes)) stop("cycle_shortcut: moves holds unknown operations")

  res <- cycle_shortcut_cpp(
    start_state, as.integer(path_codes), k, as.integer(pos),
    as.integer(move_codes), as.integer(combo_length), as.integer(n_samples),
    as.integer(n_top), as.integer(sort_ids), as.integer(max_cycle_len),
    as.integer(n_threads), isTRUE(verbose)
  )

  new_path <- unname(c("1", "2", "3")[res$path])

  # A cut rewrites the middle of the path, and a silently broken path is worse
  # than one that was never shortened.
  reached <- as.integer(
    apply_operations(start_state, new_path, k, compute_coords = FALSE)$state
  )
  expected <- as.integer(
    apply_operations(start_state, path, k, compute_coords = FALSE)$state
  )
  if (!identical(reached, expected)) {
    warning("cycle_shortcut: shortened path failed verification, returning original")
    return(unchanged)
  }

  list(
    path = new_path,
    original_length = res$original_length,
    new_length = res$new_length,
    savings = res$savings,
    n_cuts = length(res$cut_from),
    cuts = data.frame(
      from = res$cut_from,
      to = res$cut_to,
      cycle_len = res$cut_len,
      gain = res$cut_gain
    )
  )
}
