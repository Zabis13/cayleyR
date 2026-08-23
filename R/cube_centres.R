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
