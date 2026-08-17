#' Is a 4x4x4 Reduced?
#'
#' Reduction is the state an even cube has to reach before a 3x3x3 method can
#' touch it: every face's four centres one colour, so a face acts as one, and
#' the two wings of every edge a matching pair, so an edge acts as one. A cube
#' in that state is a 3x3x3 with fat pieces.
#'
#' What this does \emph{not} ask is where anything is. A solved cube turned by
#' one \code{U} is still reduced --- the centres are whole and the pairs are
#' together, they are merely in the wrong place, and putting them right is the
#' 3x3x3's job rather than the reduction's. Asking for position here is the easy
#' mistake and it makes the test reject cubes that are plainly reduced.
#'
#' @param state Integer vector of 96 stickers, a 4x4x4 state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(4)}.
#' @return \code{TRUE} if the cube is reduced.
#' @export
#' @seealso \code{\link{cube_solve4}}, \code{\link{cube_pieces}}
#' @examples
#' s <- cube_identity(4)
#' cube_is_reduced(s)
#'
#' # a turned face is still reduced: the pieces are whole, just not home
#' m <- cube_moves(4)
#' names(m) <- cube_move_names(4)
#' cube_is_reduced(s[m[["U"]]])
#'
#' # an inner-layer turn is not: it splits the pairs and the centres
#' cube_is_reduced(s[m[["1x"]]])
cube_is_reduced <- function(state) {
  state <- as.integer(state)
  if (length(state) != 96L) {
    stop("cube_is_reduced: a 4x4x4 state has 96 stickers, got ",
         length(state), call. = FALSE)
  }

  # A sticker's colour is the face it started on: 96 stickers in face blocks
  # of sixteen, so integer division recovers it.
  colour <- (state - 1L) %/% 16L

  p <- cube_pieces(4L)
  st <- strsplit(p$stickers, ",", fixed = TRUE)

  # Centres carry one sticker; the four of a face must agree.
  centres <- which(p$n_stickers == 1L)
  centre_sticker <- vapply(st[centres], function(v) as.integer(v[1L]), integer(1))
  centre_face <- (centre_sticker - 1L) %/% 16L
  for (f in unique(centre_face)) {
    here <- colour[centre_sticker[centre_face == f]]
    if (length(unique(here)) != 1L) return(FALSE)
  }

  # Wings carry two. The two wings of a dedge are the two slots whose colour
  # pairs match, and reduction asks that whatever sits in them is such a pair.
  wings <- which(p$n_stickers == 2L)
  ws <- do.call(rbind, lapply(st[wings], function(v) as.integer(v[1:2])))

  # Which dedge each slot belongs to, from the geometry: the unordered pair of
  # faces its two stickers lie on.
  slot_faces <- t(apply((ws - 1L) %/% 16L, 1L, sort))
  slot_dedge <- as.integer(factor(paste(slot_faces[, 1L], slot_faces[, 2L])))

  # And which dedge the piece now in each slot belongs to, read from its
  # colours the same way.
  here_faces <- t(apply(matrix(colour[ws], ncol = 2L), 1L, sort))
  here_dedge <- paste(here_faces[, 1L], here_faces[, 2L])

  # Slots pair up two by two; both members must hold the same dedge.
  for (d in unique(slot_dedge)) {
    if (length(unique(here_dedge[slot_dedge == d])) != 1L) return(FALSE)
  }
  TRUE
}
