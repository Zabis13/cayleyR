#' Apply a Word of Moves to a Cube State
#'
#' Applies moves written in standard notation. Half turns and wide turns are
#' expanded into the package's quarter-turn alphabet, so \code{"R U2 R'"} is
#' four moves, not three.
#'
#' @param state Integer vector of 54 stickers
#' @param word Character, moves in standard notation ("R U R' U'")
#' @return Integer vector of 54 stickers, the state after the word
#' @export
#' @seealso \code{\link{cube_word_order}}, \code{\link{cube_read_state}}
#' @examples
#' s <- cube_identity(3)
#' # a quarter turn of U, four times, is the identity
#' identical(cube_apply_word(s, "U U U U"), s)
cube_apply_word <- function(state, word) {
  cube_apply_word_cpp(as.integer(state), as.character(word))
}

#' Order of a Word of Moves
#'
#' How many times a word must be repeated before the cube returns to where it
#' started. A quarter turn has order 4 and a half turn order 2; a word that is
#' not what its name says will have some other order, which is what makes this
#' a check on the alphabet rather than a curiosity.
#'
#' @param word Character, moves in standard notation
#' @param max_order Integer, give up after this many repetitions (default 1260,
#'   the largest order in the cube group)
#' @return Integer, the order, or -1 if none was found within \code{max_order}
#' @export
#' @seealso \code{\link{cube_apply_word}}
#' @examples
#' cube_word_order("R")        # 4
#' cube_word_order("R2")       # 2
#' cube_word_order("R U R' U'")  # 6
cube_word_order <- function(word, max_order = 1260L) {
  cube_word_order_cpp(as.character(word), as.integer(max_order))
}

#' Read a Cube State as Pieces
#'
#' A cube state is 54 stickers, but a cuber sees 8 corners and 12 edges, each
#' in some slot and turned some way. This is that reading, and it is what every
#' stage predicate is built on: \code{cp[i]} is which corner sits in slot
#' \code{i}, \code{co[i]} how far it is twisted, and likewise \code{ep}/
#' \code{eo} for edges. All indices are 0-based, as the C++ layer has them.
#'
#' On a solved cube every \code{cp}/\code{ep} is its own index and every
#' \code{co}/\code{eo} is zero.
#'
#' @param state Integer vector of 54 stickers
#' @return List with components \code{cp}, \code{co} (length 8) and \code{ep},
#'   \code{eo} (length 12)
#' @export
#' @seealso \code{\link{cube_predicates}}, \code{\link{cube_apply_word}}
#' @examples
#' cube_read_state(cube_identity(3))
cube_read_state <- function(state) {
  cube_read_state_cpp(as.integer(state))
}

#' Stage Predicates of a Cube State
#'
#' Every stage of a solving method is "search until some predicate holds", so
#' these predicates are the definitions of the stages. Reported together
#' because what matters is usually which of them changed.
#'
#' @param state Integer vector of 54 stickers
#' @return Named logical vector: \code{cross_solved}, \code{slot_1} to
#'   \code{slot_4}, \code{f2l_solved}, \code{oll_solved},
#'   \code{first_layer_solved}, \code{ll_cross_oriented},
#'   \code{ll_edges_placed}, \code{ll_corners_placed}, \code{cube_solved}
#' @export
#' @seealso \code{\link{cube_read_state}}, \code{\link{cube_solve_cfop}}
#' @examples
#' cube_predicates(cube_identity(3))   # all TRUE
cube_predicates <- function(state) {
  cube_predicates_cpp(as.integer(state))
}

#' The Algorithm Tables
#'
#' The learned sequences the solvers look up: 57 OLL cases, 21 PLL cases, and
#' the handful of small algorithms layer-by-layer repeats. Returned as written
#' in the literature, with the expanded quarter-turn word alongside.
#'
#' @param which Character, one of \code{"oll"}, \code{"pll"},
#'   \code{"lbl_cross"}, \code{"lbl_edge_perm"}, \code{"lbl_corner_perm"},
#'   \code{"lbl_corner_twist"}
#' @return data.frame with \code{name}, \code{notation}, \code{n_moves}. The
#'   expanded words are on the \code{"moves"} attribute, one character vector
#'   per row.
#' @export
#' @seealso \code{\link{cube_solve_cfop}}, \code{\link{cube_apply_word}}
#' @examples
#' oll <- cube_alg_table("oll")
#' nrow(oll)
#' head(oll)
cube_alg_table <- function(which = c("oll", "pll", "lbl_cross",
                                     "lbl_edge_perm", "lbl_corner_perm",
                                     "lbl_corner_twist")) {
  which <- match.arg(which)
  cube_alg_table_cpp(which)
}

#' Expand an Algorithm into the Package's Alphabet
#'
#' Published algorithms are written with half turns (\code{R2}), wide turns
#' (\code{r}), and whole-cube rotations (\code{x}, \code{y}, \code{z}). The
#' package's alphabet has none of those: it is quarter turns of single layers.
#' This is the translation.
#'
#' Rotations are the interesting case. A rotation in a printed algorithm is an
#' instruction to the reader --- turn the cube, and read what follows from
#' where it now is --- so it is expanded by renaming the letters after it, not
#' by turning the cube and leaving them alone. Expanding it the other way
#' silently produces a different algorithm, which is what happened to the
#' published V-perm before this was written.
#'
#' @param notation Character, an algorithm in standard notation
#' @return Character vector of move names in the package alphabet
#' @export
#' @seealso \code{\link{cube_alg_table}}, \code{\link{cube_apply_word}}
#' @examples
#' cube_expand_alg("R U2 R'")        # the half turn becomes two moves
#' cube_expand_alg("y R U R'")       # the rotation renames what follows
cube_expand_alg <- function(notation) {
  cube_expand_alg_cpp(as.character(notation))
}

#' Sticker Positions of the Six Centres
#'
#' The centres never move, which is what makes a face's name mean anything: the
#' U face is the face whose centre is the U centre, whatever has happened to
#' the rest of the cube.
#'
#' @return Integer vector of 6 sticker positions (1-based)
#' @export
#' @seealso \code{\link{cube_identity}}
#' @examples
#' cube_centre_positions()
#' s <- cube_apply_word(cube_identity(3), "R U R' U'")
#' # centres are where they always were
#' all(s[cube_centre_positions()] == cube_centre_positions())
cube_centre_positions <- function() {
  cube_centre_positions_cpp()
}
