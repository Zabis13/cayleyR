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
#' How many of a 4x4x4's twenty-four centre pieces show the colour of the face
#' they sit on, counted per face. A face is finished when its count is four.
#'
#' The count compares a piece's colour against the face it began on, so it is
#' only meaningful while the cube has not been turned as a whole. That is the
#' state \code{\link{generate_state}} hands over and the one
#' \code{\link{cube_solve_centres}} is given.
#'
#' @param state Integer vector of 96 stickers.
#' @return Integer vector of six counts, in face order U R F D L B.
#' @export
#' @seealso \code{\link{cube_solve_centres}}
#' @examples
#' cube_centre_counts(cube_identity(4))
cube_centre_counts <- function(state) {
  cube_centre_counts_cpp(as.integer(state))
}
