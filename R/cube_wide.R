#' Wide Turns and Whole-Cube Rotations
#'
#' The alphabet a cube is generated in turns one layer at a time, because that
#' is what keeps the metric honest: \eqn{6n} moves, half turns written out, so
#' a shortest path counts quarter turns. Speedcubing writes in a larger
#' vocabulary. \code{Rw} turns the two layers nearest the right face together,
#' \code{3Rw} turns three, and \code{x} turns the whole cube. Reduction methods
#' for the 4x4x4 and up are published in that vocabulary and in no other.
#'
#' These functions translate it. \code{cube_expand_move} takes one such name
#' and returns the layer moves it stands for; \code{cube_expand_word} does a
#' whole algorithm; \code{cube_wide_move} returns the single permutation the
#' name performs. Nothing here enters the generating set --- \code{Rw} is a
#' word of two moves, not a move --- so the metric and every diameter measured
#' in it are untouched.
#'
#' @section The notation:
#' \tabular{ll}{
#'   \code{R} \tab the face layer alone \cr
#'   \code{Rw}, \code{r} \tab the face layer and the one behind it \cr
#'   \code{3Rw} \tab three layers, counting in from the face \cr
#'   \code{3R} \tab the third layer alone, counting in from the face \cr
#'   \code{x} \tab every layer: the whole cube turned \cr
#'   \code{M}, \code{E}, \code{S} \tab the 3x3x3 slices, where \eqn{n = 3}
#' }
#'
#' A trailing prime inverts and a trailing \code{2} doubles, as usual, and a
#' half turn expands into two quarter turns because the alphabet has no other
#' way to say it.
#'
#' The senses are the ones the literature uses: \code{x} follows \code{R},
#' \code{y} follows \code{U}, \code{z} follows \code{F}. Lower-case \code{r} is
#' accepted as a synonym for \code{Rw}, which is how many sources write it.
#'
#' @section Why not generators:
#' A wide turn could be added to the alphabet instead, and then \code{Rw} would
#' cost one move rather than two. That would change what a shortest path is,
#' and the numbers this package reports --- diameters, level sizes, the
#' distance any search returns --- would no longer be comparable with the ones
#' it reported before, nor with the published quarter-turn figures. Keeping the
#' vocabulary outside the alphabet costs nothing and avoids all of that.
#'
#' @param name Single move name, such as \code{"Rw"}, \code{"3Rw'"},
#'   \code{"x2"} or \code{"R"}
#' @param n Integer, the side of the cube (at least 2)
#' @return \code{cube_expand_move} and \code{cube_expand_word} return a
#'   character vector of layer moves from the cube's own alphabet;
#'   \code{cube_wide_move} returns a permutation of \code{1:(6n^2)}.
#' @export
#' @seealso \code{\link{cube_moves}}, \code{\link{cube_move_names}},
#'   \code{\link{cube_group}}
#' @examples
#' # on a 4x4x4 a wide turn is two layers
#' cube_expand_move("Rw", 4)
#'
#' # three of them, and the whole cube
#' cube_expand_move("3Rw", 4)
#' cube_expand_move("x", 4)
#'
#' # a single inner layer, counted in from the face
#' cube_expand_move("3R", 5)
#'
#' # a whole algorithm at once
#' cube_expand_word("Rw U Rw'", 4)
#'
#' # and the permutation it performs
#' p <- cube_wide_move("Rw", 4)
#' length(p)
cube_expand_move <- function(name, n) {
  n <- as.integer(n)
  if (is.na(n) || n < 2L) stop("cube: n must be at least 2", call. = FALSE)
  name <- as.character(name)
  if (length(name) != 1L) stop("cube_expand_move: one name at a time",
                               call. = FALSE)

  spec <- .cube_parse_move(name, n)
  layers <- spec$layers

  # The name says how far the letter's own sense turns; what the axis has to
  # do is the same for every layer of the move, so the turn is worked out once
  # and every layer takes it. Doing it per layer would invite the two halves
  # to disagree.
  out <- character(0)
  for (layer in layers) {
    nm <- .cube_layer_name(n, spec$axis, layer, spec$turns)
    out <- c(out, nm)
  }
  if (spec$half) out <- rep(out, 2L)
  out
}

#' @rdname cube_expand_move
#' @param word A word: a character vector of names, or one space-separated
#'   string
#' @export
cube_expand_word <- function(word, n) {
  if (length(word) == 1L && grepl("[ ]", word))
    word <- strsplit(trimws(word), "[[:space:]]+")[[1L]]
  word <- word[nzchar(word)]
  unlist(lapply(word, cube_expand_move, n = n), use.names = FALSE)
}

#' @rdname cube_expand_move
#' @export
cube_wide_move <- function(name, n) {
  # Deliberately not a second geometric derivation. The permutation is what
  # the expansion does, composed -- so the two can never drift apart, and
  # every test of this function is also a test of the expansion.
  moves <- cube_expand_move(name, n)
  group_compose(cube_group(n), moves)
}

#' @rdname cube_expand_move
#' @export
cube_wide_word <- function(word, n) {
  moves <- cube_expand_word(word, n)
  group_compose(cube_group(n), moves)
}

## ---- parsing -------------------------------------------------------------

## Which axis a face letter turns about, and whether the letter's own sense
## runs with the axis or against it. R, U and F are at the positive end of
## their axis and L, D and B at the negative one, which is the whole of the
## asymmetry -- the same one cube_nnn.h records in name_is_reversed().
.cube_face_axis <- list(
  R = list(axis = 1L, from_far = TRUE),
  L = list(axis = 1L, from_far = FALSE),
  U = list(axis = 2L, from_far = TRUE),
  D = list(axis = 2L, from_far = FALSE),
  F = list(axis = 3L, from_far = TRUE),
  B = list(axis = 3L, from_far = FALSE)
)

## Rotations follow a face: x follows R, y follows U, z follows F.
.cube_rot_face <- c(x = "R", y = "U", z = "F")

.cube_parse_move <- function(name, n) {
  tok <- name

  # suffix: ' inverts, 2 doubles
  turns_mult <- 1L
  half <- FALSE
  if (grepl("'$", tok)) {
    turns_mult <- 3L
    tok <- sub("'$", "", tok)
  } else if (grepl("2$", tok)) {
    half <- TRUE
    tok <- sub("2$", "", tok)
  }

  # optional leading count: "3Rw" or "3R"
  count <- NA_integer_
  m <- regmatches(tok, regexec("^([0-9]+)(.*)$", tok))[[1L]]
  if (length(m) == 3L) {
    count <- as.integer(m[2L])
    tok <- m[3L]
  }

  # a whole-cube rotation
  if (tok %in% names(.cube_rot_face)) {
    face <- .cube_rot_face[[tok]]
    spec <- .cube_face_axis[[face]]
    return(list(axis = spec$axis, layers = seq_len(n),
                turns = .cube_turns(spec$from_far, turns_mult), half = half))
  }

  # the 3x3x3 slices, which exist only where there is one inner layer
  if (tok %in% c("M", "E", "S")) {
    if (n != 3L)
      stop("cube_expand_move: '", name, "' names a 3x3x3 slice; on a ", n,
           "x", n, "x", n, " cube say which layer, as \"2R\" or \"Rw\"",
           call. = FALSE)
    # M follows L, E follows D, S follows F -- the senses cube_nnn.h fixes
    face <- c(M = "L", E = "D", S = "F")[[tok]]
    spec <- .cube_face_axis[[face]]
    return(list(axis = spec$axis, layers = 2L,
                turns = .cube_turns(spec$from_far, turns_mult), half = half))
  }

  wide <- FALSE
  if (grepl("w$", tok)) { wide <- TRUE; tok <- sub("w$", "", tok) }

  # lower case is the older way of writing a wide turn: r means Rw
  if (tok %in% c("r", "l", "u", "d", "f", "b")) {
    wide <- TRUE
    tok <- toupper(tok)
  }

  if (!tok %in% names(.cube_face_axis))
    stop("cube_expand_move: '", name, "' is not a move name", call. = FALSE)

  spec <- .cube_face_axis[[tok]]

  # How many layers, and which. A bare letter is one layer at the face; "Rw"
  # is two; "3Rw" is three; "3R" is the third alone.
  depth <- if (is.na(count)) (if (wide) 2L else 1L) else count
  if (depth < 1L || depth > n)
    stop("cube_expand_move: '", name, "' asks for layer ", depth, " of a ", n,
         "x", n, "x", n, " cube", call. = FALSE)
  depths <- if (wide || is.na(count)) seq_len(depth) else depth

  # Layers are numbered from the negative end of the axis, and a letter counts
  # inward from its own face, so the far faces count backwards.
  layers <- if (spec$from_far) n + 1L - depths else depths

  list(axis = spec$axis, layers = layers,
       turns = .cube_turns(spec$from_far, turns_mult), half = half)
}

## Quarter turns about the axis, given which end the letter is named for. A
## letter at the far end turns against the axis: U is three quarter turns
## about y and D is one.
.cube_turns <- function(from_far, mult) {
  if (from_far) (4L - mult) %% 4L else mult
}

## The alphabet's own name for one layer turned so many quarters about its
## axis. Rather than reproduce the naming rules, ask the cube: build the
## permutation and look it up. One source of truth, and it cannot drift.
.cube_layer_name <- function(n, axis, layer, turns) {
  key <- paste(cube_layer_move(n, axis, layer, turns), collapse = ",")
  cache <- .cube_name_cache(n)
  nm <- cache[[key]]
  if (is.null(nm))
    stop("cube_expand_move: no move of the ", n, "x", n, "x", n,
         " alphabet turns layer ", layer, call. = FALSE)
  nm
}

## The lookup from permutation to name, built once per size.
.cube_wide_env <- new.env(parent = emptyenv())

.cube_name_cache <- function(n) {
  key <- as.character(n)
  hit <- .cube_wide_env[[key]]
  if (!is.null(hit)) return(hit)
  tbl <- cube_moves(n)
  cache <- stats::setNames(
    as.list(names(tbl)),
    vapply(tbl, function(p) paste(p, collapse = ","), character(1)))
  .cube_wide_env[[key]] <- cache
  cache
}
