#' Cubes of Any Size
#'
#' The 3x3x3 is one member of a family, and the family is easier to describe
#' than any single member of it. A move is an axis, a layer along that axis and
#' a quarter turn; that vocabulary covers every cube, and what changes from one
#' size to the next is only how many layers there are.
#'
#' On a 3x3x3 the three layers of an axis are the two faces and the slice
#' between them --- \code{L}, \code{M}, \code{R} going along x --- so the slice
#' turns come out of the same rule as the face turns rather than being added
#' separately. On a 4x4x4 there are two inner layers per axis, and on a cube of
#' side \eqn{n} there are \eqn{n-2}.
#'
#' @section Stickers:
#' A state is an integer vector of length \eqn{6n^2}, one entry per sticker,
#' numbered face by face in the order U R F D L B and within a face left to
#' right, top to bottom as seen from outside. For \eqn{n = 3} this is exactly
#' the layout \code{\link{cube_nnn}} describes, and \code{cube_group(3)} agrees
#' with \code{\link{cube_group}} move for move on the twelve face turns.
#'
#' @section The alphabet:
#' Three axes, \eqn{n} layers, two directions: \eqn{6n} moves. Half turns are
#' \emph{not} in it --- \code{U2} is the word \code{"U U"}. This is the
#' quarter-turn metric, and the choice matters because it is what a shortest
#' path is counted in: the 3x3x3 face group has diameter 20 counting half turns
#' as one move and 26 counting them as two.
#'
#' Outer layers keep the letters the literature uses. The 3x3x3 slices are
#' \code{M}, \code{E} and \code{S}, in their usual senses. Inner layers of
#' larger cubes have no standard letter and are written axis-and-index, so
#' \code{"1x"} is layer 1 along x.
#'
#' @section What "solved" means:
#' Once slices are in the alphabet the centres move, and a cube turned bodily
#' in space is solved without its stickers being back where they started.
#' \code{\link{cube_is_colour_solved}} is the test that allows for this;
#' comparing against \code{\link{cube_identity}} is the stricter one that does
#' not.
#'
#' @name cube_nnn
#' @seealso \code{\link{cube_group}}, \code{\link{cube_moves}},
#'   \code{\link{cube_nnn}} for the 3x3x3 in particular
NULL

#' Move Table of an N x N x N Cube
#'
#' The \eqn{6n} quarter turns of a cube of side \eqn{n}, as permutations of its
#' \eqn{6n^2} stickers. Generated from the geometry rather than tabulated, so
#' the same rule gives the 2x2x2, the 3x3x3 and anything larger.
#'
#' @param n Integer, the side of the cube (at least 2)
#' @return Named list of integer vectors, each a permutation of
#'   \code{1:(6*n^2)} read as \code{new[i] <- state[perm[i]]}
#' @export
#' @seealso \code{\link{cube_group}}, \code{\link{cube_nnn}}
#' @examples
#' m <- cube_moves(3)
#' names(m)
#'
#' # the twelve face turns agree with the hand-written 3x3x3 table
#' identical(m[["R"]], cube_moves(3)[["R"]])
#'
#' # a 4x4x4 has two inner layers per axis
#' names(cube_moves(4))
cube_moves <- function(n) {
  cube_moves_cpp(as.integer(n))
}

#' Move Names of an N x N x N Cube
#'
#' @param n Integer, the side of the cube (at least 2)
#' @return Character vector of \eqn{6n} move names, in table order
#' @export
#' @seealso \code{\link{cube_moves}}
#' @examples
#' cube_move_names(2)
#' cube_move_names(3)
#' cube_move_names(4)
cube_move_names <- function(n) {
  cube_move_names_cpp(as.integer(n))
}

#' A Single Layer Turn, by Axis and Layer
#'
#' The generator's own vocabulary, for when naming a layer is less convenient
#' than pointing at it --- on a 5x5x5, say, where the inner layers have no
#' letters anyone agrees on.
#'
#' @param n Integer, the side of the cube
#' @param axis Integer, 1, 2 or 3 for the x, y and z axes. x runs L to R, y
#'   runs D to U, z runs B to F.
#' @param layer Integer in \code{1:n}, which layer along that axis turns.
#'   Layer 1 is at the negative end, layer \code{n} at the positive one.
#' @param turns Integer, quarter turns clockwise seen from the positive end of
#'   the axis: 1, 2 or 3. Half turns are available here even though they are
#'   not in the alphabet.
#' @return Integer vector of length \eqn{6n^2}, the permutation
#' @export
#' @seealso \code{\link{cube_moves}}
#' @examples
#' # on a 3x3x3, layer 3 about y is the U face
#' identical(cube_layer_move(3, axis = 2, layer = 3, turns = 3),
#'           cube_moves(3)[["U"]])
#'
#' # layer 2 about x is the M slice
#' identical(cube_layer_move(3, axis = 1, layer = 2, turns = 1),
#'           cube_moves(3)[["M"]])
cube_layer_move <- function(n, axis, layer, turns = 1L) {
  cube_layer_move_cpp(as.integer(n), as.integer(axis),
                      as.integer(layer), as.integer(turns))
}

#' Solved State of an N x N x N Cube
#'
#' @param n Integer, the side of the cube
#' @return Integer vector \code{1:(6*n^2)}
#' @export
#' @seealso \code{\link{cube_is_colour_solved}}
#' @examples
#' length(cube_identity(3))
#' length(cube_identity(4))
cube_identity <- function(n) {
  cube_identity_cpp(as.integer(n))
}

#' Test Whether Every Face of a Cube is a Single Colour
#'
#' A sticker's colour is the face it started on, so this asks whether each face
#' now carries stickers of one colour --- not whether they are back in their
#' original positions.
#'
#' The distinction only appears once slice turns are in play. Face turns leave
#' the centres alone, so from \code{\link{cube_identity}} they can never reach
#' a state that is one-coloured without being the identity outright. Slices do
#' move the centres, and then a cube turned bodily in space is solved by any
#' reasonable reading while its sticker vector is not \code{1:6n^2}.
#'
#' @param state Integer vector of length \eqn{6n^2}
#' @param n Integer, the side of the cube. Inferred from \code{state} when
#'   absent.
#' @return Logical
#' @export
#' @seealso \code{\link{cube_identity}}, \code{\link{cube_nnn}}
#' @examples
#' cube_is_colour_solved(cube_identity(3))
#'
#' # a face turn breaks it
#' g <- cube_group(3)
#' cube_is_colour_solved(group_apply(g, cube_identity(3), "R"))
#'
#' # turning every layer of one axis is turning the whole cube: the colours
#' # stay solved, but the stickers are no longer where they started
#' s <- group_apply(g, cube_identity(3), c("R", "M'", "L'"))
#' cube_is_colour_solved(s)
#' identical(s, cube_identity(3))
cube_is_colour_solved <- function(state, n = NULL) {
  state <- as.integer(state)
  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != as.integer(n)) {
      stop("cube_is_colour_solved: state of length ", length(state),
           " is not 6n^2 for any whole n")
    }
  }
  cube_is_colour_solved_cpp(state, as.integer(n))
}

#' The Group of an N x N x N Cube
#'
#' Builds the cube as a \code{\link{perm_group}}, so every search function in
#' the package runs over it: the same BFS and the same store that work on
#' TopSpin work here, and nothing about them knows the size of the cube.
#'
#' @param n Integer, the side of the cube (at least 2)
#' @param moves Character vector naming the alphabet, a subset of
#'   \code{\link{cube_move_names}(n)} (default all \eqn{6n} of them). A subset
#'   is a legitimate subgroup to search --- faces only, or one axis only.
#' @return An object of class \code{perm_group}
#' @export
#' @seealso \code{\link{cube_group}} for the 3x3x3 with half turns as single
#'   moves, \code{\link{cube_nnn}}, \code{\link{perm_group}}
#' @examples
#' g <- cube_group(3)
#' g
#' group_moves(g)
#'
#' # face turns only: the slices left out
#' cube_group(3, moves = c("U", "U'", "R", "R'", "F", "F'",
#'                         "D", "D'", "L", "L'", "B", "B'"))
#'
#' # a 2x2x2 is small enough to walk about in
#' g2 <- cube_group(2)
#' group_order(g2, "R")
#' group_order(g2, "R U")
cube_group <- function(n, moves = NULL) {
  n <- as.integer(n)
  tbl <- cube_moves(n)
  if (is.null(moves)) {
    moves <- names(tbl)
  } else {
    moves <- as.character(moves)
    unknown <- setdiff(moves, names(tbl))
    if (length(unknown) > 0L) {
      stop("cube_group: unknown move(s): ", paste(unknown, collapse = ", "),
           ". A ", n, "x", n, "x", n, " cube has: ",
           paste(names(tbl), collapse = " "))
    }
  }
  perm_group(tbl[moves], n = 6L * n * n,
             name = sprintf("cube%dx%dx%d", n, n, n))
}
