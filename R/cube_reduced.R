#' Is a Cube Reduced?
#'
#' Reduction is the state a large cube has to reach before a 3x3x3 method can
#' touch it: every face's centres one colour, so a face acts as one, and the
#' wings of every edge a matching group, so an edge acts as one. A cube in that
#' state is a 3x3x3 with fat pieces.
#'
#' What this does \emph{not} ask is where anything is. A solved cube turned by
#' one \code{U} is still reduced --- the centres are whole and the pairs are
#' together, they are merely in the wrong place, and putting them right is the
#' 3x3x3's job rather than the reduction's. Asking for position here is the easy
#' mistake and it makes the test reject cubes that are plainly reduced.
#'
#' @section Any size:
#' Nothing in the test is about a 4x4x4 beyond how many pieces there are. A
#' face has \eqn{(n-2)^2} centres rather than four and an edge has \eqn{n-2}
#' wings rather than two, and both numbers come from
#' \code{\link{cube_pieces}}. An odd cube reduces too: its middle edges and
#' fixed centres are groups of one, which pass the test by having nothing to
#' disagree with.
#'
#' A 3x3x3 is reduced in every state, and that is the right answer rather than
#' a degenerate one --- there is nothing to reduce.
#'
#' @param state Integer vector of \eqn{6n^2} stickers --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(n)}.
#' @param n Side of the cube. Inferred from the length of \code{state} when
#'   absent.
#' @return \code{TRUE} if the cube is reduced.
#' @export
#' @seealso \code{\link{cube_solve4}}, \code{\link{cube_pieces}},
#'   \code{\link{cube_centre_counts}}
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
#'
#' # a 5x5x5 works the same way
#' cube_is_reduced(cube_identity(5))
cube_is_reduced <- function(state, n = NULL) {
  state <- as.integer(state)

  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != round(n) || n < 2)
      stop("cube_is_reduced: a state of ", length(state),
           " stickers is no cube; give n if it cannot be inferred",
           call. = FALSE)
    n <- as.integer(round(n))
  }
  n <- as.integer(n)
  face_size <- n * n
  if (length(state) != 6L * face_size)
    stop("cube_is_reduced: a ", n, "x", n, "x", n, " state has ",
         6L * face_size, " stickers, got ", length(state), call. = FALSE)

  # A sticker's colour is the face it started on: stickers run in face blocks,
  # so integer division by the block size recovers it.
  colour <- (state - 1L) %/% face_size

  p <- cube_pieces(n)
  st <- strsplit(p$stickers, ",", fixed = TRUE)

  # Centres carry one sticker; the ones on a face must agree.
  centres <- which(p$n_stickers == 1L)
  if (length(centres)) {
    centre_sticker <- vapply(st[centres], function(v) as.integer(v[1L]),
                             integer(1))
    centre_face <- (centre_sticker - 1L) %/% face_size
    for (f in unique(centre_face)) {
      here <- colour[centre_sticker[centre_face == f]]
      if (length(unique(here)) != 1L) return(FALSE)
    }
  }

  # Wings carry two. The wings of a dedge are the slots whose colour pairs
  # match, and reduction asks that whatever sits in them is such a group.
  wings <- which(p$n_stickers == 2L)
  if (length(wings)) {
    ws <- do.call(rbind, lapply(st[wings], function(v) as.integer(v[1:2])))

    # Which dedge each slot belongs to, from the geometry: the unordered pair
    # of faces its two stickers lie on.
    slot_faces <- t(apply((ws - 1L) %/% face_size, 1L, sort))
    slot_dedge <- paste(slot_faces[, 1L], slot_faces[, 2L])

    # And which dedge the piece now in each slot belongs to, read from its
    # colours the same way.
    here_faces <- t(apply(matrix(colour[ws], ncol = 2L), 1L, sort))
    here_dedge <- paste(here_faces[, 1L], here_faces[, 2L])

    # The slots of one dedge must all hold the same dedge. On a 4x4x4 that is
    # two slots; on a 6x6x6 four; on an odd cube the middle edge is one slot
    # and passes by itself.
    for (d in unique(slot_dedge)) {
      if (length(unique(here_dedge[slot_dedge == d])) != 1L) return(FALSE)
    }
  }

  TRUE
}
