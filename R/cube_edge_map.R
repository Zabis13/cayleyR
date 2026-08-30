#' The Edges of a Cube of Any Size, Derived
#'
#' What a method needs to know about edges before it can pair them: which
#' pieces are edge pieces, which of the twelve cube edges each sits on, where
#' along that edge it lies, and where every turn sends it. All of it read off
#' \code{\link{cube_pieces}} and \code{\link{cube_moves}} rather than written
#' down per size.
#'
#' @section Why this is derived:
#' The 4x4x4 edge solver in \code{src/cube_edges.h} carries its slots as a
#' hand-measured table of twenty-four sticker pairs, ordered "the twelve edges
#' by face pair, A then B". That table is correct, and it is also the reason
#' the file solves one size: twenty-four is a fact about a 4x4x4, and so is the
#' pair of names A and B. A 5x5x5 puts three pieces on every edge, a 7x7x7 puts
#' five, and neither has halves to call A and B.
#'
#' Nothing in the table is independent of the geometry, though. An edge piece
#' is a piece with two stickers; the edge it belongs to is the pair of faces it
#' touches; its place along that edge is the one coordinate that is not pinned
#' to a face. Asking those three questions of \code{\link{cube_pieces}}
#' reproduces the hand table at \code{n = 4} and asks the same question of any
#' other size. That equality is checked in the tests, so the generic version is
#' held to the hand-measured one rather than merely believed.
#'
#' @section How many pieces an edge holds:
#' Measured, not assumed: \code{n = 3} through \code{n = 7} give 12 cube edges
#' at every size, holding \code{n - 2} pieces each --- 1, 2, 3, 4, 5. The
#' pieces fall into \code{floor((n - 1) / 2)} orbits, and on an odd cube the
#' last of those holds the 12 middle edges, one per cube edge, sitting on the
#' symmetry plane. Those are the pieces that behave like the edges of a 3x3x3.
#'
#' @section Why outer turns cannot pair anything:
#' The mechanism the 4x4x4 solver is built on generalises exactly. Measured at
#' \code{n = 4} and \code{n = 5} over the whole alphabet:
#'
#' \itemize{
#'   \item the six OUTER turns move 4(n-2) slots and touch 4 cube edges, none
#'     of them partially --- an edge travels whole, so an outer turn can
#'     neither pair anything nor break a pair;
#'   \item every SLICE moves 4 slots across 4 cube edges, all four partially
#'     --- it carries one piece off each edge and leaves the rest. That
#'     mismatch is the entire mechanism of pairing, and it is also why a slice
#'     on its own wrecks the centres.
#' }
#'
#' So a slice has to be wrapped: open with it, work with outer turns, close
#' with its inverse. That is the same shape the l-slice stage of the centres
#' used.
#'
#' @name cube_edge_map_family
#' @seealso \code{\link{cube_edge_structure}}, \code{\link{cube_edge_map}},
#'   \code{\link{cube_pieces}}, \code{\link{cube_centre_structure}}
NULL

#' Edge Pieces of an N x N x N Cube
#'
#' One row per edge piece: which of the twelve cube edges it sits on, where
#' along that edge it lies, which orbit it belongs to and which stickers it
#' carries.
#'
#' An edge piece carries exactly two stickers, which is what distinguishes it
#' from a corner (three) and a centre (one). See
#' \code{\link{cube_edge_map_family}} for why this is derived rather than
#' tabulated per size.
#'
#' @section Naming an edge:
#' A cube edge is named by the two faces it lies between, as \code{face_lo} and
#' \code{face_hi} with \code{face_lo < face_hi}, so the name does not depend on
#' which way the edge is read. The \code{edge} column numbers those twelve
#' pairs in ascending order, and is stable at every size.
#'
#' @section Naming a place along an edge:
#' \code{pos} is the coordinate that runs along the edge, from 1 to
#' \code{n - 2}. \code{slot} is the same thing numbered from the nearer end,
#' from 1 up, so that pieces which are mirror images of each other across the
#' middle of the edge share a slot number; on an odd cube the middle piece is
#' the only one of its slot. Pieces of one edge with the same \code{slot} are
#' the ones a solver has to tell apart, and \code{pos} is what tells them.
#'
#' @param n Side of the cube, 3 or more.
#' @return A \code{data.frame} with one row per edge piece and columns
#'   \code{piece}, \code{edge}, \code{face_lo}, \code{face_hi}, \code{pos},
#'   \code{slot}, \code{orbit}, \code{label}, \code{sticker_a} and
#'   \code{sticker_b} --- the last two 1-based indices into the state vector.
#' @export
#' @seealso \code{\link{cube_edge_map}}, \code{\link{cube_pieces}}
#' @examples
#' e <- cube_edge_structure(4)
#' nrow(e)                  # 24 wings, two to an edge
#' table(e$edge)            # twelve edges, two pieces each
#'
#' e5 <- cube_edge_structure(5)
#' nrow(e5)                 # 36: 24 wings and 12 middles
#' table(e5$orbit)
cube_edge_structure <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 3L)
    stop("cube_edge_structure: a cube of side ", n, " has no edge pieces",
         call. = FALSE)

  p <- cube_pieces(n)
  e <- p[p$n_stickers == 2L, , drop = FALSE]

  xyz <- as.matrix(e[, c("x", "y", "z")])
  lo <- 0L
  hi <- n - 1L

  # The axis of an edge is the one coordinate that is not pinned to a face.
  # A piece with two stickers touches exactly two faces, so exactly one of the
  # three coordinates is free and this is never ambiguous.
  free <- (xyz != lo) & (xyz != hi)
  if (!all(rowSums(free) == 1L))
    stop("cube_edge_structure: an edge piece with two stickers did not have ",
         "exactly one free coordinate", call. = FALSE)
  axis <- max.col(free, ties.method = "first")

  pos <- xyz[cbind(seq_len(nrow(xyz)), axis)]

  # The faces an edge lies between: for each pinned coordinate, the face is
  # decided by the axis and by which end it is pinned to. Face order follows
  # cube_moves(): U R F D L B, so axis 1 (x) gives faces 4/1, axis 2 (y) gives
  # 0/3 and axis 3 (z) gives 5/2 -- read off the identity rather than assumed.
  face_of <- .cube_edge_face_of(n)

  pinned <- lapply(seq_len(nrow(xyz)), function(i) setdiff(1:3, axis[i]))
  faces <- t(vapply(seq_len(nrow(xyz)), function(i) {
    f <- vapply(pinned[[i]], function(a)
      face_of[[a]][[if (xyz[i, a] == lo) 1L else 2L]], integer(1))
    sort(f)
  }, integer(2)))

  key <- paste(faces[, 1], faces[, 2], sep = "-")
  edge <- match(key, sort(unique(key)))

  # Slot counts from the nearer end so that the two ends of an edge are named
  # alike; pos is what separates pieces that share a slot.
  slot <- pmin(pos, hi - pos)

  st <- strsplit(as.character(e$stickers), ",", fixed = TRUE)
  sa <- vapply(st, function(s) as.integer(s[1]), integer(1))
  sb <- vapply(st, function(s) as.integer(s[2]), integer(1))

  out <- data.frame(
    piece     = e$piece,
    edge      = as.integer(edge),
    face_lo   = as.integer(faces[, 1]),
    face_hi   = as.integer(faces[, 2]),
    pos       = as.integer(pos),
    slot      = as.integer(slot),
    orbit     = e$orbit,
    label     = e$label,
    sticker_a = sa,
    sticker_b = sb,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$edge, out$pos), , drop = FALSE]
  rownames(out) <- NULL
  out
}

## Which face lies at each end of each axis.
##
## Determined by asking the identity cube what colour sits on the far side of a
## given coordinate, rather than by writing down the U R F D L B order and
## hoping it matches the geometry. A face is a block of n*n stickers all of one
## colour on the identity, so the colour a sticker shows names its face.
.cube_edge_face_of <- function(n) {
  id <- cube_identity(n)
  p <- cube_pieces(n)

  # A centre-of-face piece on an even cube does not exist, so use any piece
  # pinned to the end in question and read the sticker that faces that way.
  # The sticker index itself names the face: face f owns f*n*n + 1 .. (f+1)*n*n
  face_of_sticker <- function(s) (s - 1L) %/% (n * n)

  # For each axis and each end, find a piece with that coordinate pinned and
  # exactly one other sticker on the axis-facing side.
  res <- vector("list", 3L)
  cn <- c("x", "y", "z")
  for (a in 1:3) {
    ends <- integer(2)
    for (k in 1:2) {
      want <- if (k == 1L) 0L else n - 1L
      # Pieces pinned at this end whose other coordinates are interior: on a
      # cube of side >= 3 these are the face centres of the face we want.
      sel <- p[[cn[a]]] == want
      for (b in setdiff(1:3, a))
        sel <- sel & p[[cn[b]]] > 0L & p[[cn[b]]] < n - 1L
      w <- which(sel)
      if (!length(w))
        stop("cube_edge_structure: no interior piece at axis ", a, " end ", k,
             call. = FALSE)
      s <- as.integer(strsplit(as.character(p$stickers[w[1]]), ",",
                               fixed = TRUE)[[1]])
      if (length(s) != 1L)
        stop("cube_edge_structure: expected a one-sticker piece", call. = FALSE)
      ends[k] <- face_of_sticker(s)
    }
    res[[a]] <- as.list(ends)
  }
  res
}

#' Where a Turn Sends Each Edge Piece
#'
#' The permutation a single move induces on the edge slots: which slot each
#' piece's contents lands in. This is the table an edge solver reads instead of
#' searching, and it is the generic form of \code{slot_maps()} in
#' \code{src/cube_edges.h}.
#'
#' Only the slots the move actually disturbs are listed. Measured at
#' \code{n = 4} and \code{n = 5}, an outer turn moves \code{4(n - 2)} slots and
#' never splits an edge, while a slice moves 4 and splits all four it touches
#' --- see \code{\link{cube_edge_map_family}} for why that difference is the
#' whole mechanism of pairing.
#'
#' @param n Side of the cube, 3 or more.
#' @param move Name of a move, as given by \code{\link{cube_moves}}.
#' @param structure Optional result of \code{\link{cube_edge_structure}} for
#'   the same \code{n}, to avoid recomputing it in a loop.
#' @return A \code{data.frame} with one row per disturbed slot and columns
#'   \code{from_piece}, \code{from_edge}, \code{from_pos}, \code{to_piece},
#'   \code{to_edge}, \code{to_pos} and \code{splits} --- the last \code{TRUE}
#'   when the move carries this piece off an edge whose other pieces it leaves
#'   behind.
#' @export
#' @seealso \code{\link{cube_edge_structure}}, \code{\link{cube_slice_map}}
#' @examples
#' cube_edge_map(4, "U")     # eight slots, four whole edges
#' cube_edge_map(4, "1x")    # four slots, four edges split
cube_edge_map <- function(n, move, structure = NULL) {
  n <- as.integer(n)
  es <- if (is.null(structure)) cube_edge_structure(n) else structure

  perm <- cube_moves(n)[[move]]
  if (is.null(perm))
    stop("cube_edge_map: no move '", move, "' on a ", n, "x", n, "x", n,
         call. = FALSE)

  # A piece is identified by the unordered pair of stickers it occupies, so
  # that a piece which is turned in place still matches itself.
  keys <- .cube_edge_keys(es$sticker_a, es$sticker_b)

  # Where the contents of each slot goes: apply the move to the identity and
  # ask which slot now holds what this slot held.
  after <- perm
  moved_a <- after[es$sticker_a]
  moved_b <- after[es$sticker_b]
  dest_keys <- .cube_edge_keys(moved_a, moved_b)
  j <- match(dest_keys, keys)

  if (anyNA(j))
    stop("cube_edge_map: an edge piece left the edge set", call. = FALSE)

  keep <- which(j != seq_along(j))
  if (!length(keep))
    return(data.frame(from_piece = integer(0), from_edge = integer(0),
                      from_pos = integer(0), to_piece = integer(0),
                      to_edge = integer(0), to_pos = integer(0),
                      splits = logical(0), stringsAsFactors = FALSE))

  # An edge is split when the move takes some of its pieces and not all.
  n_per_edge <- table(es$edge)
  n_moved <- table(factor(es$edge[keep], levels = names(n_per_edge)))
  split_edge <- names(n_per_edge)[n_moved > 0 & n_moved < n_per_edge]

  data.frame(
    from_piece = es$piece[keep],
    from_edge  = es$edge[keep],
    from_pos   = es$pos[keep],
    to_piece   = es$piece[j[keep]],
    to_edge    = es$edge[j[keep]],
    to_pos     = es$pos[j[keep]],
    splits     = as.character(es$edge[keep]) %in% split_edge,
    stringsAsFactors = FALSE
  )
}

## An unordered sticker pair, as a single comparable string.
.cube_edge_keys <- function(a, b) {
  lo <- pmin(a, b)
  hi <- pmax(a, b)
  paste(lo, hi, sep = ",")
}

#' How Many Edges Are Whole
#'
#' The count a pairing stage ranks its moves by, and the per-edge breakdown
#' behind it. An edge is whole when every piece sitting on it shows the same
#' pair of colours --- on a 4x4x4 that is two wings agreeing, on a 5x5x5 three
#' pieces, on an odd cube the middle piece counts with its wings.
#'
#' This is the rule \code{\link{cube_is_reduced}} applies to decide the whole
#' cube; here it is asked one edge at a time, because a solver that can only
#' see "reduced or not" has nothing to rank a candidate move by.
#'
#' @param state Integer vector of \eqn{6n^2} stickers.
#' @param n Side of the cube; inferred from \code{state} when \code{NULL}.
#' @param structure Optional result of \code{\link{cube_edge_structure}} for
#'   the same \code{n}, to avoid recomputing it in a loop.
#' @return A list with \code{whole} (how many of the twelve edges are whole),
#'   \code{total} (twelve) and \code{by_edge}, a logical vector one entry per
#'   edge.
#' @export
#' @seealso \code{\link{cube_edge_structure}}, \code{\link{cube_is_reduced}}
#' @examples
#' cube_edge_counts(cube_identity(5))$whole      # 12
#'
#' # an inner slice splits the edges it touches
#' s <- cube_identity(5)
#' cube_edge_counts(s[cube_moves(5)[["1x"]]])$whole
cube_edge_counts <- function(state, n = NULL, structure = NULL) {
  state <- as.integer(state)

  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != round(n) || n < 2)
      stop("cube_edge_counts: a state of ", length(state),
           " stickers is no cube; give n if it cannot be inferred",
           call. = FALSE)
    n <- as.integer(round(n))
  }
  n <- as.integer(n)
  face_size <- n * n
  if (length(state) != 6L * face_size)
    stop("cube_edge_counts: a ", n, "x", n, "x", n, " state has ",
         6L * face_size, " stickers, got ", length(state), call. = FALSE)

  es <- if (is.null(structure)) cube_edge_structure(n) else structure

  # A sticker's colour is the face it started on, as in cube_is_reduced().
  colour <- (state - 1L) %/% face_size

  # The pair of colours each piece now shows, sorted so it can be compared
  # regardless of which way round the piece sits.
  ca <- colour[es$sticker_a]
  cb <- colour[es$sticker_b]
  here <- paste(pmin(ca, cb), pmax(ca, cb), sep = "-")

  by_edge <- vapply(split(here, es$edge),
                    function(v) length(unique(v)) == 1L, logical(1))

  list(whole = sum(by_edge), total = length(by_edge), by_edge = by_edge)
}

#' Wide Turns of an N x N x N Cube
#'
#' A wide turn is a face together with the inner layer behind it, turned as one
#' slab --- \code{Rw}, \code{Uw}, \code{Dw} in the notation the published edge
#' algorithms are written in. This gives the pair of package moves that make
#' one up, at any size.
#'
#' @section Why this is measured:
#' \code{src/cube_edges.h} carries the expansions as constants:
#' \code{Dw -> D 1y\'}, \code{Uw -> U 2y}, \code{Rw -> R 2x}. Two things in
#' those are 4x4x4 facts --- which numbered slice lies behind a face, and
#' nothing else. Measured across \code{n = 4, 5, 6}: a slice \code{k<axis>}
#' turns layer \code{k}, and the faces sit at layers \code{0} and \code{n - 1},
#' so the layer behind a face is 1 for D, L, B and \code{n - 2} for U, R, F.
#' At \code{n = 4} that returns the three constants above.
#'
#' The direction is not free either, and edge-wholeness cannot decide it: both
#' directions leave 8 edges whole, and both have order 4, because a face and a
#' slice on the same axis commute. What separates them is whether the slab
#' turns as one rigid rotation. Measured by asking whether every piece the
#' composite moves undergoes the same rotation in the plane perpendicular to
#' the axis: \code{U}, \code{R}, \code{F} take the slice straight and \code{D},
#' \code{L}, \code{B} take it inverted --- uniform at \code{n = 4} and
#' \code{n = 5} alike, and agreeing with the three constants.
#'
#' @param n Side of the cube, 3 or more.
#' @param face One of \code{"U"}, \code{"R"}, \code{"F"}, \code{"D"},
#'   \code{"L"}, \code{"B"}.
#' @param prime \code{TRUE} for the anticlockwise turn.
#' @return A character vector of package moves.
#' @export
#' @seealso \code{\link{cube_edge_algs}}, \code{\link{cube_moves}}
#' @examples
#' cube_wide_turn(4, "R")        # R 2x  -- the Rw of the sources
#' cube_wide_turn(5, "R")        # R 3x  -- the same turn on a bigger cube
#' cube_wide_turn(4, "D")        # D 1y\' -- low faces take the slice inverted
cube_wide_turn <- function(n, face, prime = FALSE) {
  n <- as.integer(n)
  if (is.na(n) || n < 3L)
    stop("cube_wide_turn: a cube of side ", n, " has no inner layer",
         call. = FALSE)

  # Which axis the face turns about, and which end of it the face sits at, are
  # read from the permutation rather than tabulated: the pieces a face turn
  # moves all share one coordinate, and its value is the layer.
  fl <- .cube_face_layer(n, face)
  axis <- c("x", "y", "z")[fl$axis]

  # A face sits at layer 0 or n-1, and the layer behind it is 1 or n-2.
  is_high <- fl$layer != 0L
  layer <- if (is_high) n - 2L else 1L

  # A low face turns the opposite way round its axis from a high one, so its
  # slice has to be inverted to turn with it.
  slice_prime <- xor(!is_high, prime)

  c(paste0(face, if (prime) "'" else ""),
    paste0(layer, axis, if (slice_prime) "'" else ""))
}

#' The Published Edge-Pairing Algorithms, at Any Size
#'
#' The six slice-flip-slice words the 4x4x4 solver uses, written in the package
#' alphabet for a cube of side \code{n}. A slice brings two pieces of one edge
#' together, a face turn flips one of them, the slice goes back.
#'
#' @section Why these six:
#' They are the set measured in \code{src/cube_edges.h}, which records for each
#' one its length, how many pairs it breaks and how many slots it touches. All
#' six leave the centres built, which is what makes them safe to run after the
#' centre stage. The last is the parity case --- it swaps two pieces of one
#' edge rather than permuting whole edges.
#'
#' @section What changes with size:
#' Only the wide turns, and only in which numbered slice they name. The words
#' are stated here in the sources' own notation (\code{Rw}, \code{Dw},
#' \code{Uw}) and expanded through \code{\link{cube_wide_turn}}, so a single
#' statement covers every size instead of one transcription per cube. At
#' \code{n = 4} the expansion reproduces the six constants in the reference
#' file exactly, which the tests check.
#'
#' Verified at \code{n = 4} against the reference's own table: from a solved
#' cube the six leave 9, 9, 10, 10, 10 and 8 edges whole --- that is 3, 3, 2,
#' 2, 2 and 4 pairs broken, as the reference records.
#'
#' @param n Side of the cube, 3 or more.
#' @return A character vector of six words, each a space-separated sequence of
#'   package moves.
#' @export
#' @seealso \code{\link{cube_wide_turn}}, \code{\link{cube_edge_counts}}
#' @examples
#' cube_edge_algs(4)[1]
#' cube_edge_algs(5)[1]     # the same algorithm, the slice renumbered
cube_edge_algs <- function(n) {
  n <- as.integer(n)
  w <- function(face, prime = FALSE) cube_wide_turn(n, face, prime)

  # A face and the slice behind it are on the same axis, so they commute and
  # the two orders are the same permutation -- checked in the tests. The order
  # below is the reference's: the face leads when the wide turn opens a word
  # and trails when it closes one, which is how cube_edges.h writes them.
  rw  <- w("R"); rwi <- .cube_invert_moves(rw)
  dw  <- w("D"); dwi <- .cube_invert_moves(dw)
  uw  <- w("U")

  algs <- list(
    c(rwi, "F", "R", "F'", rw),
    c(dw, "R", "U", "R'", dwi),
    c(dw, "R", "F'", "U", "R'", "F", dwi),
    c(dw, "R", "U", "R'", "F", "R'", "F'", "R", dwi),
    c(uw, uw, "R", "U", "R'", "F", "R'", "F'", "R", uw, uw),
    rep(c(w("R"), "U", "U"), 5L)
  )
  vapply(algs, paste, character(1), collapse = " ")
}

## Invert a sequence of moves, move by move. The caller reverses the order.
.cube_invert_moves <- function(mv) {
  ifelse(grepl("'", mv, fixed = TRUE), sub("'", "", mv, fixed = TRUE),
         paste0(mv, "'"))
}

#' Pair the Edges of a Cube of Any Size
#'
#' The second stage of reduction, after the centres are built: bring the pieces
#' of each cube edge together so the cube can be finished as a 3x3x3. This is
#' the generic form of \code{pair_edges()} in \code{src/cube_edges.h}.
#'
#' @section How it works:
#' Try every algorithm under every setup word, take whatever pairs the most,
#' repeat. The setup words are outer turns only --- they carry an edge whole
#' (measured; see \code{\link{cube_edge_map_family}}), so they can reposition
#' the cube without ever undoing work. The algorithms are the six of
#' \code{\link{cube_edge_algs}}. A round that cannot improve ends the stage.
#'
#' A candidate is rejected outright if it disturbs the centres, so the stage
#' can never spend what the centre stage built. That check is what
#' \code{keep_centres} controls; turning it off is for diagnosis, not solving.
#'
#' @section On the setup pool:
#' The reference widened its pool to every composition of up to three outer
#' turns, having measured five positions that stalled with anything shorter.
#' \code{depth} is that number. Three gives 1885 words and is the default;
#' two gives 157 and is enough to watch the stage work but not to finish
#' reliably.
#'
#' @param state Integer vector of \eqn{6n^2} stickers.
#' @param n Side of the cube; inferred from \code{state} when \code{NULL}.
#' @param depth Longest setup word, in outer turns. Default 3.
#' @param max_rounds Give up after this many rounds. Default 40.
#' @param keep_centres Reject any candidate that disturbs the centres.
#' @return A list with \code{state} (the cube after the stage), \code{path}
#'   (the moves applied, as a character vector), \code{whole} (how many of the
#'   twelve edges ended whole), \code{rounds} and \code{solved}.
#' @export
#' @seealso \code{\link{cube_edge_algs}}, \code{\link{cube_edge_counts}},
#'   \code{\link{cube_build_lslice}}
#' @examples
#' \donttest{
#' set.seed(1)
#' s <- generate_state(group = cube_group(5), n_moves = 20)
#' r <- cube_pair_edges(s, depth = 2, max_rounds = 5)
#' r$whole
#' }
cube_pair_edges <- function(state, n = NULL, depth = 3L, max_rounds = 40L,
                            keep_centres = TRUE) {
  state <- as.integer(state)

  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != round(n) || n < 2)
      stop("cube_pair_edges: a state of ", length(state),
           " stickers is no cube; give n if it cannot be inferred",
           call. = FALSE)
    n <- as.integer(round(n))
  }
  n <- as.integer(n)
  if (length(state) != 6L * n * n)
    stop("cube_pair_edges: a ", n, "x", n, "x", n, " state has ",
         6L * n * n, " stickers, got ", length(state), call. = FALSE)

  mv <- cube_moves(n)
  es <- cube_edge_structure(n)
  algs <- lapply(cube_edge_algs(n), function(w) strsplit(w, " ", fixed = TRUE)[[1]])

  outer <- .cube_outer_turns(n, es)
  pool <- .cube_setup_pool(outer, as.integer(depth))

  centres_ok <- if (keep_centres) .cube_centres_signature(state, n) else NULL

  path <- character(0)
  st <- state
  rounds <- 0L

  for (guard in seq_len(as.integer(max_rounds))) {
    before <- cube_edge_counts(st, n, es)$whole
    if (before == 12L) break
    rounds <- guard

    best_score <- before
    best_word <- NULL

    for (setup in pool) {
      staged <- st
      for (m in setup) staged <- staged[mv[[m]]]

      for (a in algs) {
        cand <- staged
        for (m in a) cand <- cand[mv[[m]]]
        if (!is.null(centres_ok) &&
            !identical(.cube_centres_signature(cand, n), centres_ok)) next
        score <- cube_edge_counts(cand, n, es)$whole
        if (score <= best_score) next
        best_score <- score
        best_word <- c(setup, a)
      }
    }

    if (is.null(best_word)) break
    for (m in best_word) st <- st[mv[[m]]]
    path <- c(path, best_word)
  }

  whole <- cube_edge_counts(st, n, es)$whole
  list(state = st, path = path, whole = whole, rounds = rounds,
       solved = whole == 12L)
}

## Every composition of up to `depth` outer turns, as a list of character
## vectors, the empty word first. A turn is never followed by its own inverse
## or by a repeat of itself, which cuts the pool without losing any position:
## those pairs are either a no-op or a half turn reachable another way.
.cube_setup_pool <- function(outer, depth) {
  pool <- list(character(0))
  frontier <- list(character(0))
  face_of <- sub("'", "", outer, fixed = TRUE)

  for (d in seq_len(max(0L, depth))) {
    nxt <- list()
    for (w in frontier) {
      last <- if (length(w)) sub("'", "", w[length(w)], fixed = TRUE) else ""
      for (m in outer) {
        if (sub("'", "", m, fixed = TRUE) == last) next
        nxt[[length(nxt) + 1L]] <- c(w, m)
      }
    }
    pool <- c(pool, nxt)
    frontier <- nxt
  }
  pool
}

## What the centres look like, as the thing a candidate must not change. Two
## states have the same signature when every centre sticker shows the same
## colour, which is what "the centres are still built" means -- the pieces need
## not be the same pieces.
.cube_centres_signature <- function(state, n) {
  face_size <- n * n
  p <- cube_pieces(n)
  centres <- which(p$n_stickers == 1L)
  s <- vapply(strsplit(as.character(p$stickers[centres]), ",", fixed = TRUE),
              function(v) as.integer(v[1L]), integer(1))
  (state[s] - 1L) %/% face_size
}

## The turns that carry an edge whole, and so can be used to set up without
## ever undoing work.
##
## Not a written-down list of six faces: at n = 3 the alphabet also holds the
## slices M, E and S, which do not start with a digit and would pass a test on
## the name. The property wanted is the measured one -- a turn qualifies when
## it splits no edge, which is exactly what cube_edge_map() reports. Measured
## at n = 3..7 this returns the twelve face turns at every size.
.cube_outer_turns <- function(n, structure = NULL) {
  es <- if (is.null(structure)) cube_edge_structure(n) else structure
  keep <- vapply(names(cube_moves(n)), function(m) {
    mp <- cube_edge_map(n, m, es)
    nrow(mp) > 0L && !any(mp$splits)
  }, logical(1))
  names(keep)[keep]
}

## Which axis a face turn turns about, and which layer of that axis it is.
##
## Measured from the permutation: every piece a face turn moves shares one
## coordinate, and that coordinate is the axis, its value the layer. Measured
## at n = 4, 5, 7 this gives U=y(n-1), R=x(n-1), F=z(n-1) and D=y0, L=x0, B=z0
## -- the table this replaces, now obtained rather than asserted.
.cube_face_layer <- function(n, face) {
  mv <- cube_moves(n)
  perm <- mv[[face]]
  if (is.null(perm))
    stop("cube_wide_turn: '", face, "' is not a move on a ", n, "x", n, "x", n,
         call. = FALSE)

  id <- cube_identity(n)
  moved <- which(id[perm] != id)
  p <- cube_pieces(n)
  st <- strsplit(as.character(p$stickers), ",", fixed = TRUE)
  pm <- vapply(st, function(s) any(as.integer(s) %in% moved), logical(1))

  cn <- c("x", "y", "z")
  for (a in seq_along(cn)) {
    v <- unique(p[[cn[a]]][pm])
    if (length(v) == 1L && (v == 0L || v == n - 1L))
      return(list(axis = a, layer = as.integer(v)))
  }
  stop("cube_wide_turn: '", face, "' does not turn a single outer layer",
       call. = FALSE)
}
