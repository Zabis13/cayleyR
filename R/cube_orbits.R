#' Piece Orbits of a Cube of Any Size
#'
#' "Corners and edges" is a sentence about a 3x3x3. A 4x4x4 has no edges in
#' that sense --- what looks like one is two wings that never swap --- and its
#' centres move. So the vocabulary has to come from the geometry, and these
#' functions derive it rather than tabulating it.
#'
#' @section Pieces:
#' A piece is a cubie: a position carrying one sticker per face it touches, and
#' how many faces it touches is the whole classification. Three stickers is a
#' corner, and there are always eight of them whatever the size. Two is an edge
#' piece --- the twelve edges of a 3x3x3, splitting into wings on larger cubes.
#' One is a centre, fixed on an odd cube only at the middle of each face.
#'
#' @section Orbits:
#' Two pieces are in the same orbit when some sequence of moves takes one to
#' the other, and the point of an orbit is what it forbids: no word, however
#' long, moves a piece out of its own. A method may therefore treat each orbit
#' separately, which is most of what solving a large cube consists of.
#'
#' Orbits are computed by walking the cube move by move from one piece and
#' collecting everywhere it can go, so they are orbits by definition rather
#' than by a rule that might be wrong. The labels only name what the walk
#' found. A label reads \code{f3:0,0,0} for the corners, \code{f2:0,0,k} for an
#' edge orbit and \code{f1:0,j,k} for a centre orbit, where the numbers are
#' each coordinate's distance to the nearer end of its range, sorted.
#'
#' Where a class of centres holds two orbits that are mirror images of each
#' other, the labels carry a trailing \code{+} or \code{-}. This happens on
#' some sizes and not others --- a 6x6x6 splits \code{f1:0,1,2} and a 5x5x5
#' does not --- which is why the orbits are walked rather than guessed.
#'
#' Every orbit holds 24 pieces except three cases: the 8 corners, and on an
#' odd cube the 6 fixed centres and 12 middle edges, all of which sit on a
#' symmetry axis.
#'
#' @name cube_orbits_family
#' @seealso \code{\link{cube_orbits}}, \code{\link{cube_pieces}},
#'   \code{\link{cube_progress}}, \code{\link{cube_group}}
NULL

#' Orbits of an N x N x N Cube
#'
#' One row per orbit: what kind of piece it holds, how many pieces are in it,
#' and the depths and mirror sign that name it. See
#' \code{\link{cube_orbits_family}} for what an orbit is and why it matters.
#'
#' @param n Side of the cube, 2 or more.
#' @return A \code{data.frame} with one row per orbit and columns
#'   \code{orbit} (index), \code{label}, \code{kind} (\code{"corner"},
#'   \code{"edge"} or \code{"centre"}), \code{stickers_per_piece},
#'   \code{n_pieces}, \code{depth_a}, \code{depth_b} and \code{chirality}
#'   (\code{-1}, \code{0} or \code{1}; non-zero only for one of a mirror pair).
#' @export
#' @seealso \code{\link{cube_pieces}}, \code{\link{cube_progress}}
#' @examples
#' cube_orbits(3)   # corners, edges, fixed centres
#' cube_orbits(4)   # centres now move, edges are wings
#' cube_orbits(6)   # a mirror pair of centre orbits, labelled + and -
cube_orbits <- function(n) {
  cube_orbits_cpp(as.integer(n))
}

#' Pieces of an N x N x N Cube
#'
#' One row per piece: where it sits, which orbit it belongs to, and which
#' stickers of the state vector it carries.
#'
#' @param n Side of the cube, 2 or more.
#' @return A \code{data.frame} with one row per piece and columns
#'   \code{piece}, \code{x}, \code{y}, \code{z}, \code{orbit}, \code{label},
#'   \code{n_stickers} and \code{stickers} --- the last a comma-separated list
#'   of 1-based sticker indices.
#' @export
#' @seealso \code{\link{cube_orbits}}, \code{\link{cube_progress}}
#' @examples
#' p <- cube_pieces(3)
#' nrow(p)                       # 26: 8 corners, 12 edges, 6 centres
#' head(p[p$n_stickers == 3, ])  # the corners
cube_pieces <- function(n) {
  cube_pieces_cpp(as.integer(n))
}

#' How Much of Each Orbit Is Solved
#'
#' A measure of progress that works at any size. One number for a whole cube
#' says too little --- the orbits are solved at different times, and a method
#' that has finished the centres and not the wings is in a quite different
#' position from one that has done neither.
#'
#' A piece counts as home when every sticker in its slot shows the colour that
#' slot has on a solved cube. That is a statement about colours rather than
#' about sticker numbers, so a cube solved relative to its centres counts as
#' solved --- which is what a real cube looks like, and what the blindfold
#' methods mean by the word.
#'
#' The size is read from the state: a vector of 96 entries is a 4x4x4 and there
#' is nothing else it could be.
#'
#' @param state Integer vector of \eqn{6n^2} stickers.
#' @return A \code{data.frame} with one row per orbit and columns
#'   \code{orbit}, \code{label}, \code{kind}, \code{solved}, \code{total} and
#'   \code{fraction}.
#' @export
#' @seealso \code{\link{cube_orbits}}, \code{\link{cube_pieces_home}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(4), n_moves = 30)
#' cube_progress(s)
cube_progress <- function(state) {
  cube_progress_cpp(as.integer(state))
}

#' Pieces Home on a Cube
#'
#' The blunt summary of \code{\link{cube_progress}}: how many pieces are home
#' out of how many there are.
#'
#' @param state Integer vector of \eqn{6n^2} stickers.
#' @return A named integer vector with elements \code{home} and \code{total}.
#' @export
#' @seealso \code{\link{cube_progress}}
#' @examples
#' cube_pieces_home(cube_identity(4))   # all of them
cube_pieces_home <- function(state) {
  cube_pieces_home_cpp(as.integer(state))
}
