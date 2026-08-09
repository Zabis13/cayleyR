#' Colours and Positions: the Two Ways to Write a Cube Down
#'
#' A cube state can record, for each sticker, either the colour it shows or the
#' place it belongs in. The package works in positions --- a state is a
#' permutation of \code{1:(6n^2)}, and the solvers and searches all read it that
#' way --- while most outside sources, Kaggle's Santa 2023 among them, record
#' colours.
#'
#' \code{cube_colours} goes from positions to colours, which always works and
#' loses information. \code{cube_colour_state} goes back, which works only when
#' the colours happen to determine the state.
#'
#' @section When colours are enough:
#' On a 2x2x2 or a 3x3x3 every piece carries a different set of colours, so a
#' colouring names exactly one state and the conversion is exact.
#'
#' From 4x4x4 up it is not. The four wings of an edge share their two colours,
#' and the centres of a face share one, so a colouring cannot say which of them
#' is which --- and the number of states it stands for grows quickly with
#' \eqn{n}. \code{cube_colour_state} still returns a state, choosing among the
#' identical pieces in the order they are numbered, but it warns; the state it
#' returns shows the right colours and is not otherwise the state you started
#' from.
#'
#' @param state Integer vector of \eqn{6n^2} entries: a permutation of
#'   \code{1:(6n^2)} for \code{cube_colours}, or colours for
#'   \code{cube_colour_state}. Colours may be \code{0..5} or \code{1..6}; the
#'   result uses whichever the input did.
#' @param n Integer, the side of the cube; inferred from the length when
#'   omitted
#' @return \code{cube_colours} returns \eqn{6n^2} colours numbered from
#'   \code{0}, one per sticker, in face order U R F D L B.
#'   \code{cube_colour_state} returns a permutation of \code{1:(6n^2)}.
#' @export
#' @seealso \code{\link{cube_santa_state}} for the same journey across
#'   notations, \code{\link{cube_is_colour_solved}}, \code{\link{cube_pieces}}
#' @examples
#' set.seed(1)
#' s <- generate_state(group = cube_group(3), n_moves = 10)
#'
#' # positions to colours and back is exact on a 3x3x3
#' identical(cube_colour_state(cube_colours(s)), s)
#'
#' # a solved cube is six blocks of one colour
#' table(cube_colours(cube_identity(3)))
cube_colours <- function(state, n = NULL) {
  state <- as.integer(state)
  n <- .cube_check_n(length(state), n)
  # A sticker's colour is the face it belongs to, which is what its position
  # number already says.
  (state - 1L) %/% (n * n)
}

#' @rdname cube_colours
#' @export
cube_colour_state <- function(state, n = NULL) {
  state <- as.integer(state)
  n <- .cube_check_n(length(state), n)
  f2 <- n * n

  lo <- min(state)
  if (!lo %in% c(0L, 1L) || max(state) - lo > 5L)
    stop("cube_colour_state: colours must be 0..5 or 1..6", call. = FALSE)
  colours <- state - lo

  pieces <- cube_pieces(n)
  st <- lapply(strsplit(pieces$stickers, ",", fixed = TRUE), as.integer)

  # A sticker is named by what its whole piece shows, read starting from
  # itself: the colour it carries, then the colours of its neighbours on the
  # piece in a fixed cyclic order. That signature is what identifies a sticker,
  # and matching signatures is a lookup rather than a search -- the greedy
  # piece-by-piece matching this replaced could paint itself into a corner.
  #
  # The cyclic order has to be the piece's own, not the order the stickers
  # happen to be listed in, so that a turned piece reads as a rotation of the
  # same signature. .cube_cycle_order puts them in that order.
  sig_of <- function(ix, cols) {
    ord <- .cube_cycle_order(ix, n)
    v <- cols[ord]
    k <- length(v)
    vapply(seq_len(k), function(i) paste(v[((seq_len(k) - 1L + i - 1L) %% k) + 1L],
                                         collapse = ","), character(1))
  }

  home_cols <- (seq_len(6L * f2) - 1L) %/% f2
  home_sig <- character(6L * f2)
  for (ix in st) {
    ord <- .cube_cycle_order(ix, n)
    home_sig[ord] <- sig_of(ix, home_cols)
  }

  seen_sig <- character(6L * f2)
  for (ix in st) {
    ord <- .cube_cycle_order(ix, n)
    seen_sig[ord] <- sig_of(ix, colours)
  }

  # Where a signature occurs more than once among the home stickers, the
  # colouring cannot say which is which. Report it, then break the tie by
  # taking them in order.
  dup <- sum(duplicated(home_sig))
  if (dup > 0L)
    warning("cube_colour_state: on a ", n, "x", n, "x", n, " cube ", dup,
            " stickers share their colours with another, so the colouring does ",
            "not name one state. Returning a state that shows these colours.",
            call. = FALSE)

  by_sig <- split(seq_along(home_sig), home_sig)
  used <- stats::setNames(integer(length(by_sig)), names(by_sig))

  out <- integer(6L * f2)
  for (i in seq_along(seen_sig)) {
    pool <- by_sig[[seen_sig[i]]]
    if (is.null(pool))
      stop("cube_colour_state: the colours around sticker ", i,
           " are not ones a cube can display", call. = FALSE)
    k <- used[[seen_sig[i]]] + 1L
    if (k > length(pool))
      stop("cube_colour_state: colour ", colours[i], " appears more often than ",
           "a cube has stickers of it", call. = FALSE)
    used[[seen_sig[i]]] <- k
    out[i] <- pool[k]
  }
  out
}

## The stickers of a piece in cyclic order about it, seen from outside, so
## that turning the piece rotates the list rather than reshuffling it. Sorting
## by face number would not do: it is a fixed order, and a turned corner would
## read as a different piece.
##
## The order comes from the geometry. Each sticker faces along an axis, and for
## a corner the three outward normals form a right-handed or left-handed set
## depending on which corner it is; ordering them so the set is right-handed
## makes the list rotate with the piece and nothing else.
.cube_face_normal <- function(face) {
  # faces U R F D L B, as outward unit vectors on the axes x (L->R), y (D->U),
  # z (B->F)
  switch(as.character(face),
    "0" = c(0, 1, 0), "3" = c(0, -1, 0),
    "1" = c(1, 0, 0),  "4" = c(-1, 0, 0),
    "2" = c(0, 0, 1),  "5" = c(0, 0, -1))
}

.cube_cycle_order <- function(ix, n) {
  if (length(ix) < 3L) return(ix)     # an edge or a centre needs no ordering
  faces <- (ix - 1L) %/% (n * n)
  v <- lapply(faces, .cube_face_normal)
  det3 <- v[[1L]][1L] * (v[[2L]][2L] * v[[3L]][3L] - v[[2L]][3L] * v[[3L]][2L]) -
          v[[1L]][2L] * (v[[2L]][1L] * v[[3L]][3L] - v[[2L]][3L] * v[[3L]][1L]) +
          v[[1L]][3L] * (v[[2L]][1L] * v[[3L]][2L] - v[[2L]][2L] * v[[3L]][1L])
  if (det3 < 0) ix[c(1L, 3L, 2L)] else ix
}

.cube_check_n <- function(len, n) {
  if (is.null(n)) {
    n <- as.integer(round(sqrt(len / 6)))
    if (6L * n * n != len)
      stop("cube state of length ", len, " is not 6n^2", call. = FALSE)
  } else {
    n <- as.integer(n)
    if (6L * n * n != len)
      stop("cube state of length ", len, " does not match n = ", n,
           call. = FALSE)
  }
  if (n < 2L) stop("cube: n must be at least 2", call. = FALSE)
  n
}
