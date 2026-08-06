#' Solve a 3x3x3 Cube by CFOP
#'
#' Cross, then the four first-two-layer pairs, then orient the last layer, then
#' permute it. The cross and the pairs are searched for optimally; the last
#' layer is looked up in the standard tables of 57 OLL and 21 PLL cases. That
#' division is the method's own: the first half is intuitive and the second is
#' memorised.
#'
#' Each stage is solved optimally on its own and the total is still around
#' three times optimal, because a stage that is shortest in isolation usually
#' leaves the next one worse off. That gap is the price of a method a person
#' can hold in their head. Move counts are quarter turns, so they run larger
#' than the counts quoted in speedcubing, which count a half turn as one.
#'
#' @param state Integer vector of 54 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(3)}.
#' @param cross_depth Integer, search depth for the cross (default 8, the
#'   standard bound: no cross is further than 8 quarter turns).
#' @param slot_depth Integer, search depth for one F2L pair (default 11).
#' @return List with components:
#'   \item{path}{Character vector of moves ("R", "U'", ...)}
#'   \item{found}{Logical, whether the cube ended solved}
#'   \item{stages}{data.frame of stages: \code{name}, \code{detail} (the case
#'     name, where there is one), \code{n_moves}. The moves themselves are on
#'     the \code{"moves"} attribute, one character vector per row.}
#'   \item{states}{List of integer vectors, the cube after each stage}
#' @export
#' @seealso \code{\link{cube_solve_lbl}}, \code{\link{cube_group}},
#'   \code{\link{generate_state}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(3), n_moves = 20)
#' \donttest{
#' res <- cube_solve_cfop(s)
#' res$found
#' length(res$path)
#' res$stages
#' }
cube_solve_cfop <- function(state, cross_depth = 8L, slot_depth = 11L) {
  cube_solve_cfop_cpp(as.integer(state), as.integer(cross_depth),
                      as.integer(slot_depth))
}

#' Solve a 3x3x3 Cube Layer by Layer
#'
#' The method a beginner is taught: cross on D, the four bottom corners, the
#' four middle edges, then the last layer in four steps --- make the yellow
#' cross, place its edges, place the corners, twist them. Each last-layer step
#' is a small algorithm applied over and over with a U turn between, which is
#' why the method is easy to learn and long to execute.
#'
#' It shares the cross and the last layer with \code{\link{cube_solve_cfop}}.
#' The middle is where it is slower and simpler: the bottom layer is finished
#' outright, and only then do the middle edges go in one at a time. Expect
#' roughly twice the moves of CFOP.
#'
#' @param state Integer vector of 54 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(3)}.
#' @param cross_depth Integer, search depth for the cross (default 8)
#' @param corner_depth Integer, search depth per first-layer corner (default 9)
#' @param edge_depth Integer, search depth per middle edge (default 11)
#' @return List with the same components as \code{\link{cube_solve_cfop}}:
#'   \code{path}, \code{found}, \code{stages}, \code{states}.
#' @export
#' @seealso \code{\link{cube_solve_cfop}}, \code{\link{cube_group}},
#'   \code{\link{generate_state}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(3), n_moves = 20)
#' \donttest{
#' res <- cube_solve_lbl(s)
#' res$found
#' length(res$path)
#' }
cube_solve_lbl <- function(state, cross_depth = 8L, corner_depth = 9L,
                           edge_depth = 11L) {
  cube_solve_lbl_cpp(as.integer(state), as.integer(cross_depth),
                     as.integer(corner_depth), as.integer(edge_depth))
}
