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

#' Solve a 3x3x3 Cube by Old Pochmann
#'
#' The blindfolded method, and it is built on a different principle from the
#' other two. CFOP and layer by layer both work by looking: a stage ends when
#' the cuber can see that it has. Old Pochmann never looks at the cube after
#' the start, so it repeats one step whose shape is always the same --- swap
#' whatever is in the buffer with one chosen piece, disturbing nothing else.
#' Follow it and the pieces come home in a chain.
#'
#' Nothing here is searched. Each piece is placed by a conjugate: setup moves
#' that bring the target where the algorithm can reach it, one memorised
#' algorithm, then the setup moves undone. The algorithms are ordinary PLLs,
#' because "swap two edges and two corners" is exactly what a T-perm does, and
#' the second swap is not incidental: a single swap is an odd permutation and
#' the cube group has none. Those extra swaps cancel in pairs, and when their
#' count is odd one is left over --- that is the parity step, between the edges
#' and the corners.
#'
#' Expect around twice the moves of layer by layer. The cost of never looking
#' is that no move ever does two things at once.
#'
#' @param state Integer vector of 54 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(3)}.
#' @return List with the same components as \code{\link{cube_solve_cfop}}:
#'   \code{path}, \code{found}, \code{stages}, \code{states}. Each stage's
#'   \code{detail} names the target sticker by its standard letter and the
#'   algorithm used.
#' @export
#' @seealso \code{\link{cube_solve_cfop}}, \code{\link{cube_solve_lbl}},
#'   \code{\link{cube_group}}, \code{\link{generate_state}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(3), n_moves = 20)
#' \donttest{
#' res <- cube_solve_old_pochmann(s)
#' res$found
#' length(res$path)
#' }
cube_solve_old_pochmann <- function(state) {
  cube_solve_old_pochmann_cpp(as.integer(state))
}

#' Solve a 3x3x3 Cube by M2
#'
#' Old Pochmann's method with a cheaper edge step, and the usual next thing a
#' blindfold solver learns. There an edge was placed by a whole PLL wrapped in
#' setup moves; here the buffer is DF and the swap is \code{M2}, which is two
#' moves. The corners are unchanged.
#'
#' \code{M2} is not a clean swap, and the method is arranged around that rather
#' than avoiding it. It turns the middle slice a half turn, so it moves the
#' centres and exchanges the UF/DB edge as well. Those two get their own
#' algorithms; edges left facing the wrong way are set aside while the chain
#' runs and turned afterwards in one orientation phase; and whether the parity
#' fix is needed is decided by reading the edge permutation rather than by
#' counting swaps, because a chain that breaks into a new cycle spends turns the
#' tally does not see.
#'
#' The cube comes back solved with the centres sometimes rotated among
#' themselves --- "solved relative to the centres", which is what the method
#' means by solved and what a real cube looks like.
#'
#' Expect roughly two thirds the moves of \code{\link{cube_solve_old_pochmann}},
#' with the saving all on the edges.
#'
#' @param state Integer vector of 54 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(3)}.
#' @return List with the same components as \code{\link{cube_solve_cfop}}:
#'   \code{path}, \code{found}, \code{stages}, \code{states}.
#' @export
#' @seealso \code{\link{cube_solve_old_pochmann}}, \code{\link{cube_solve_cfop}},
#'   \code{\link{cube_solve_lbl}}, \code{\link{cube_group}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(3), n_moves = 20)
#' \donttest{
#' res <- cube_solve_m2(s)
#' res$found
#' length(res$path)
#' }
cube_solve_m2 <- function(state) {
  cube_solve_m2_cpp(as.integer(state))
}
