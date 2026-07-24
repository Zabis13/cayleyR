#' Distance Methods for Bridge Selection
#'
#' The registry of methods \code{\link{find_path_iterative}} may use to score
#' candidate states when picking bridges. Each method is a function
#' \code{f(states, target, k)} returning one score per row of \code{states};
#' lower is better. The search takes the lowest-scoring candidate, breaking
#' ties on the smaller step number.
#'
#' \describe{
#'   \item{\code{manhattan}}{Sum of absolute differences to the target. The
#'     default, and the only behaviour available before methods became
#'     pluggable.}
#'   \item{\code{breakpoints}}{Number of adjacency violations relative to the
#'     target -- positions where consecutive values are not consecutive in the
#'     target's frame. Often a better guide than raw displacement.}
#'   \item{\code{human}}{How far the state is from being solved \emph{the way a
#'     person solves it}: the number of tiles still missing from the sorted
#'     run. Scores against the identity \code{1:n} and ignores \code{target},
#'     so it applies only to searches heading for the sorted ring. Ties break
#'     on the gap phase 1 works to close. See
#'     \code{\link{find_best_match_human}}.}
#' }
#'
#' Every method is a function of \code{(states, target, k)}: a matrix with one
#' candidate state per row, the state being approached, and the flipper width
#' (used by \code{human}, ignored by the rest). It returns one score per row,
#' lower being better, with \code{NA} marking a candidate it rejects. Those are
#' the arguments of the returned method, not of the two functions here.
#'
#' @return \code{cayley_distance_methods()} returns the registered names;
#'   \code{cayley_distance()} returns the method itself, as a function.
#' @name distance_methods
#' @seealso \code{\link{find_path_iterative}}, \code{\link{human_phase1_rank}}
#' @examples
#' m <- rbind(c(1L, 2L, 3L, 4L), c(4L, 3L, 2L, 1L))
#' cayley_distance_methods()          # registered method names
#' cayley_distance("manhattan")(m, 1:4, 4)
NULL

#' @rdname distance_methods
#' @export
cayley_distance_methods <- function() {
  names(.distance_registry)
}

#' @rdname distance_methods
#' @param method Character, name of a registered method
#' @export
cayley_distance <- function(method) {
  fn <- .distance_registry[[method]]
  if (is.null(fn)) {
    stop("Unknown distance method '", method, "'. Available: ",
         paste(cayley_distance_methods(), collapse = ", "))
  }
  fn
}

# --- Method implementations -------------------------------------------------

.distance_manhattan <- function(states, target, k) {
  target <- as.integer(target)
  rowSums(abs(states - matrix(target, nrow = nrow(states),
                              ncol = length(target), byrow = TRUE)))
}

.distance_breakpoints <- function(states, target, k) {
  target <- as.integer(target)
  apply(states, 1L, function(s) breakpoint_distance(as.integer(s), target))
}

#' Score Candidate States the Way a Person Solves
#'
#' Distance is how much of the sorted run is still missing: \code{n -
#' run_length}. Within a run length, ties break on the gap phase 1 works to
#' close, so among states with an equal run the one closest to placing its next
#' value scores lower.
#'
#' Scoring is against the identity \code{1:n} and \code{target} is ignored.
#' That is deliberate, not an oversight: \code{run_length} only means something
#' when the goal is the sorted ring. Relabelling into an arbitrary target's
#' frame was tried and performed worse -- the side of a two-ended search that
#' grows from an unstructured state ends up judged against a goal phase 1
#' cannot read. Use this method for searches heading for \code{1:n}, as the
#' tail search after phase 1 does.
#'
#' Registered as distance method \code{"human"}; see
#' \code{\link{distance_methods}}.
#'
#' Scoring runs in C++ (\code{human_distance_cpp}); the whole candidate set is
#' scored in one call.
#'
#' @param states Integer matrix, one candidate state per row
#' @param target Integer vector, ignored; accepted for interface uniformity
#' @param k Integer, flipper width
#' @return Numeric vector of scores, one per row; lower is better
#' @export
#' @seealso \code{\link{distance_methods}}, \code{\link{human_phase1_rank}}
find_best_match_human <- function(states, target, k) {
  if (!is.matrix(states)) states <- matrix(as.integer(states), nrow = 1L)
  storage.mode(states) <- "integer"
  human_distance_cpp(states, as.integer(target), as.integer(k))
}

.distance_registry <- list(
  manhattan   = .distance_manhattan,
  breakpoints = .distance_breakpoints,
  human       = find_best_match_human
)
