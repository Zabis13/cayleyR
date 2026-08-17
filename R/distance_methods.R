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
#'   \item{\code{cube4_model}}{Moves still to play on a 4x4x4, read off a
#'     trained model rather than counted. The other methods score how far two
#'     states are \emph{as arrays}, which on a cube says little about how far
#'     they are as \emph{cubes}: two states one quarter turn apart can differ in
#'     most of their stickers. This one asks a model that was trained to predict
#'     exactly that number. Like \code{human} it scores against the solved cube
#'     and ignores \code{target}, so it belongs to searches heading for the
#'     solved state. See \code{\link{find_best_match_cube4_model}}.}
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

#' Score Candidate States With a Trained 4x4x4 Model
#'
#' Distance is the model's own answer to "how many quarter turns from solved":
#' it scores the 24 moves of a state and the smallest of them is taken as the
#' state's value. The whole candidate set goes through in one call, because the
#' model batches and a per-row call would cost far more than the scoring.
#'
#' Scoring is against the solved cube and \code{target} is ignored, the same
#' arrangement \code{\link{find_best_match_human}} uses and for the same reason:
#' the model was trained towards one particular state and cannot be asked about
#' distance to an arbitrary one. Use it for searches heading for the solved
#' cube; on a search heading elsewhere the numbers do not describe the pair
#' being compared.
#'
#' The value is a prediction, not a count. It is close enough to guide a choice
#' between candidates -- which is all bridge selection needs -- but it carries no
#' guarantee, and a search that needs an exact answer near the solved cube
#' should get it from the group, not from here.
#'
#' The model is read once and kept for the session. Set \code{CUBE4_ARCHIVE} to
#' the directory holding \code{model/model.pth}; \code{ggmlR} must be installed.
#'
#' Registered as distance method \code{"cube4_model"}; see
#' \code{\link{distance_methods}}.
#'
#' @param states Integer matrix, one candidate 4x4x4 state per row, in sticker
#'   numbers as the rest of the package uses
#' @param target Integer vector, ignored; accepted for interface uniformity
#' @param k Integer, ignored; accepted for interface uniformity
#' @return Numeric vector of scores, one per row; lower is better
#' @export
#' @seealso \code{\link{distance_methods}}, \code{\link{find_best_match_human}}
find_best_match_cube4_model <- function(states, target, k) {
  if (!is.matrix(states)) states <- matrix(as.integer(states), nrow = 1L)
  storage.mode(states) <- "integer"
  if (ncol(states) != 96L) {
    stop("find_best_match_cube4_model: states must have 96 columns ",
         "(a 4x4x4); got ", ncol(states))
  }

  model <- .cube4_model()

  # The package works in sticker numbers; the model wants colours in the face
  # order the competition uses. One conversion per row, then one call.
  mstates <- t(apply(states, 1L, function(s)
    cube_santa_state_out(cube_colours(as.integer(s), 4L), 4L)))

  q <- ggmlR::pt_forward(model, mstates)
  if (is.null(dim(q))) q <- matrix(q, nrow = 1L)
  score <- apply(q, 1L, min)

  # The model scores the 24 moves OUT of a state, so a solved cube gets the
  # value of leaving it -- about 3.9, worse than a cube two turns away that has
  # a good move to show. Left alone, bridge selection would walk past the
  # answer. The solved cube is the one state whose distance needs no model.
  solved <- apply(mstates, 1L, function(s)
    all(vapply(0:5, function(f) length(unique(s[f * 16L + 1:16])) == 1L,
               logical(1))))
  score[solved] <- 0
  score
}

# The model is large and reading it is slow, so it is read once and kept in the
# package environment rather than passed through every layer of the search.
.cube4_model_env <- new.env(parent = emptyenv())

.cube4_model <- function() {
  if (!is.null(.cube4_model_env$model)) return(.cube4_model_env$model)

  if (!requireNamespace("ggmlR", quietly = TRUE)) {
    stop("distance method \"cube4_model\" needs the ggmlR package")
  }
  archive <- Sys.getenv("CUBE4_ARCHIVE", "")
  if (!nzchar(archive)) {
    stop("distance method \"cube4_model\" needs CUBE4_ARCHIVE set to the ",
         "directory holding model/model.pth")
  }
  path <- file.path(archive, "model", "model.pth")
  if (!file.exists(path)) stop("no model at ", path)

  .cube4_model_env$model <- ggmlR::pt_transformer_load(path)
  .cube4_model_env$model
}

.distance_registry <- list(
  manhattan   = .distance_manhattan,
  breakpoints = .distance_breakpoints,
  human       = find_best_match_human,
  cube4_model = find_best_match_cube4_model
)
