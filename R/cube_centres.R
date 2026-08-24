#' Solve the Centres of a 4x4x4 Cube
#'
#' The first stage of reduction. On a 3x3x3 a centre is one sticker and cannot
#' move; from 4x4x4 up each face carries four centre pieces, all twenty-four in
#' a single orbit, and they have to be gathered before the cube can be treated
#' as a 3x3x3 at all.
#'
#' The method is Stefan Pochmann's layer-by-layer centres --- the same author
#' as the M2 method in \code{\link{cube_solve_m2}}. One centre is built, turned
#' to the left, and the layer beside it is filled; then that layer goes to the
#' bottom and the remaining pieces are shot down from the top one at a time.
#' Two things are computed rather than searched for: which move carries a given
#' piece where, and how far to turn U first so the shot has something to fire.
#'
#' Whatever the shooting cannot reach is finished by commutators --- a swap
#' between two faces, or a 3-cycle among three --- with the cube turned to meet
#' each one rather than a variant of each kept per orientation. A 4-cycle is
#' not a separate case: it is a 3-cycle followed by a swap.
#'
#' @param state Integer vector of 96 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(4)}.
#' @return List with components:
#'   \item{path}{Character vector of moves ("R", "1x'", ...)}
#'   \item{found}{Logical, whether all six centres ended built}
#'   \item{stages}{data.frame of stages: \code{name}, \code{detail},
#'     \code{n_moves}. The moves themselves are on the \code{"moves"}
#'     attribute, one character vector per row.}
#'   \item{states}{List of integer vectors, the cube after each stage}
#' @export
#' @seealso \code{\link{cube_group}}, \code{\link{generate_state}},
#'   \code{\link{cube_is_colour_solved}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(4), n_moves = 40)
#' \donttest{
#' res <- cube_solve_centres(s)
#' res$found
#' length(res$path)
#' }
cube_solve_centres <- function(state) {
  cube_centres_cpp(as.integer(state))
}

#' Reduce a 4x4x4, Trying Every Starting Face
#'
#' \code{\link{cube_reduce_cpp}} builds its first centre on one face and works
#' from there. Which face that is was written into the pipeline as L; this runs
#' it once per face and keeps the shortest reduction that actually reduces.
#'
#' @section Why the face matters:
#' The method does not search for a short reduction, it follows a schedule --- a
#' centre, the layer beside it, the rest shot down from the top, then the edges
#' --- and the schedule runs whatever the cube arrived like. So the cost is
#' decided by how the scramble happens to sit relative to the face the schedule
#' starts on, and that is a property of the pairing, not of the cube.
#'
#' Measured over 200 scrambles of depth 2 to 20, six faces each, every path
#' verified by replaying it (\code{inst/examples/} carries the survey):
#'
#' \itemize{
#'   \item the default face is the best of the six on 22% of cubes;
#'   \item the median cube reduces 26% shorter by picking the best face, and
#'     30038 moves over the whole set become 19202 --- 64% of the default;
#'   \item no face wins often enough to be a better default: U leads at 53 of
#'     200 and L, the old default, still wins 28. The gain is in trying them.
#' }
#'
#' The saving holds at every depth measured, from 94% at depth 2 down to about
#' 18% at depth 20, where the default already costs some 190 moves.
#'
#' @section What it costs:
#' Six reductions instead of one: about 0.32 seconds against 0.053 on the
#' machine the survey ran on. Nothing is shared between the six --- each starts
#' from the state as given.
#'
#' @param state Integer vector of 96 stickers.
#' @param faces Which starting faces to try, 0 to 5 in the order U R F D L B.
#'   Defaults to all six.
#' @param verify Whether to replay each path and keep only those that leave the
#'   cube reduced. On by default: a face that reports success without reducing
#'   would otherwise win by being short.
#' @return The result of \code{\link{cube_reduce_cpp}} for the best face, with
#'   two components added: \code{face}, the face it started from, and
#'   \code{tried}, a \code{data.frame} of every face with columns \code{face},
#'   \code{found}, \code{n_moves} and \code{verified}. If no face reduces the
#'   cube, the result of the first face tried is returned with \code{found}
#'   left as it came.
#' @export
#' @seealso \code{\link{cube_reduce_cpp}}, \code{\link{cube_solve4}},
#'   \code{\link{cube_is_reduced}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(4), n_moves = 12)
#' \donttest{
#' res <- cube_reduce_best(s)
#' res$face
#' res$tried
#' length(res$path) <= length(cube_reduce_cpp(s)$path)
#' }
cube_reduce_best <- function(state, faces = 0:5, verify = TRUE) {
  state <- as.integer(state)
  faces <- as.integer(faces)
  if (!length(faces) || any(is.na(faces)) || any(faces < 0L | faces > 5L))
    stop("cube_reduce_best: faces must be numbers from 0 to 5", call. = FALSE)

  moves <- cube_moves(4L)
  names(moves) <- cube_move_names(4L)
  replay <- function(s, path) {
    for (m in path) s <- s[moves[[m]]]
    s
  }

  runs <- lapply(faces, function(f) cube_reduce_cpp(state, f))

  # A path counts only if it reduces the cube. The solver says whether it
  # thinks it finished; replaying is what checks, and the shortest word is
  # picked among the ones that pass -- never among what was merely reported.
  ok <- vapply(runs, function(r) isTRUE(r$found), logical(1))
  verified <- ok
  if (verify) {
    for (i in which(ok))
      verified[i] <- isTRUE(cube_is_reduced(replay(state, runs[[i]]$path), 4L))
  }

  n_moves <- vapply(runs, function(r) length(r$path), integer(1))
  tried <- data.frame(face = faces, found = ok, n_moves = n_moves,
                      verified = verified, stringsAsFactors = FALSE)

  usable <- which(verified)
  if (!length(usable)) {
    out <- runs[[1]]
    out$face <- faces[1]
    out$tried <- tried
    return(out)
  }

  best <- usable[which.min(n_moves[usable])]
  out <- runs[[best]]
  out$face <- faces[best]
  out$tried <- tried
  out
}

#' Count the Centre Pieces That Are Home
#'
#' How many centre pieces show the colour of the face they sit on, counted per
#' face. A face is finished when its count is the number of centres a face has
#' at that size --- four on a 4x4x4, nine on a 5x5x5, one on a 3x3x3.
#'
#' The count compares a piece's colour against the face it began on, so it is
#' only meaningful while the cube has not been turned as a whole. That is the
#' state \code{\link{generate_state}} hands over and the one
#' \code{\link{cube_solve_centres}} is given.
#'
#' @param state Integer vector of \eqn{6n^2} stickers.
#' @param n Side of the cube. Inferred from the length of \code{state} when
#'   absent.
#' @param by_orbit Whether to count each centre orbit separately. A 5x5x5 has
#'   three --- the corner centres, the plus centres and the fixed six --- and a
#'   method that solves them in turn needs to see them apart.
#' @return With \code{by_orbit = FALSE}, an integer vector of six counts in
#'   face order U R F D L B. With \code{by_orbit = TRUE}, a
#'   \code{data.frame} of \code{orbit}, \code{face}, \code{home} and
#'   \code{of} --- how many are home out of how many.
#' @export
#' @seealso \code{\link{cube_solve_centres}},
#'   \code{\link{cube_centre_structure}}
#' @examples
#' cube_centre_counts(cube_identity(4))
#' cube_centre_counts(cube_identity(5))
#'
#' # the three orbits of a 5x5x5, seen apart
#' cube_centre_counts(cube_identity(5), by_orbit = TRUE)
cube_centre_counts <- function(state, n = NULL, by_orbit = FALSE) {
  state <- as.integer(state)

  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != round(n) || n < 2)
      stop("cube_centre_counts: a state of ", length(state),
           " stickers is no cube; give n if it cannot be inferred",
           call. = FALSE)
    n <- as.integer(round(n))
  }
  n <- as.integer(n)
  if (length(state) != 6L * n * n)
    stop("cube_centre_counts: a ", n, "x", n, "x", n, " state has ",
         6L * n * n, " stickers, got ", length(state), call. = FALSE)

  # The 4x4x4 path stays with the C++ it has always used, so nothing that
  # depends on it changes shape or speed.
  if (n == 4L && !by_orbit) return(cube_centre_counts_cpp(state))

  cs <- cube_centre_structure(n)
  face_size <- n * n
  # A sticker's value is where it began and stickers are numbered face by face,
  # so its colour is the block that value falls in.
  home <- (state[cs$sticker] - 1L) %/% face_size == cs$face

  if (!by_orbit)
    return(vapply(0:5, function(f) sum(home[cs$face == f]), integer(1)))

  grid <- expand.grid(face = 0:5, orbit = sort(unique(cs$orbit)))
  out <- data.frame(
    orbit = grid$orbit,
    face  = grid$face,
    home  = mapply(function(f, ob) sum(home[cs$face == f & cs$orbit == ob]),
                   grid$face, grid$orbit),
    of    = mapply(function(f, ob) sum(cs$face == f & cs$orbit == ob),
                   grid$face, grid$orbit),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$orbit, out$face), ]
  rownames(out) <- NULL
  out
}
