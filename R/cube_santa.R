#' The Santa 2023 Cube Notation
#'
#' The Kaggle Santa 2023 puzzles name a cube's moves \code{f0}, \code{r1},
#' \code{-d2} and so on: a letter for the axis, an index for the layer, and a
#' leading minus for the inverse. That is the same alphabet
#' \code{\link{cube_moves}} builds --- one quarter turn of one layer, \eqn{6n}
#' of them, half turns excluded --- written differently, so the two are
#' interchangeable once the correspondence is pinned down.
#'
#' @section What differs:
#' Two things, and neither can be guessed from the names.
#'
#' The faces are numbered in a different order. Santa's \code{0..5} are our
#' \code{U}, \code{F}, \code{R}, \code{B}, \code{L}, \code{D}; ours are
#' \code{U R F D L B}. Inside a face the two layouts agree exactly --- row by
#' row, left to right, seen from outside --- so the relabelling moves whole
#' faces and never disturbs a face's interior.
#'
#' The senses do not line up letter for letter. \code{f3} is \verb{B'} rather
#' than \code{B}, \code{d3} is \verb{U'}, \code{d1} is \verb{1y'}. Written in
#' axes and layers instead the irregularity disappears, which is why the
#' correspondence is defined that way here:
#'
#' \tabular{lll}{
#'   \strong{Santa} \tab \strong{axis} \tab \strong{layer, quarter turns} \cr
#'   \code{f}\eqn{i} \tab z \tab layer \eqn{n - i}, 3 turns \cr
#'   \code{r}\eqn{i} \tab x \tab layer \eqn{n - i}, 3 turns \cr
#'   \code{d}\eqn{i} \tab y \tab layer \eqn{i + 1}, 1 turn
#' }
#'
#' A leading \code{-} inverts, which is 1 turn against 3.
#'
#' @section Which correspondence:
#' Sixteen relabellings satisfy all \eqn{6n} equations, because a cube may be
#' held sixteen ways that leave this alphabet looking like itself --- four
#' turns about the vertical, a choice of which pole is up, and a mirror. They
#' are the same correspondence seen from different angles, not a genuine
#' ambiguity, and the one used here is the one that keeps \code{f0} as
#' \code{F} and \code{r0} as \code{R}.
#'
#' @section States:
#' A Santa state is \eqn{6n^2} colours \code{0..5} in their face order; ours is
#' \eqn{6n^2} sticker numbers. \code{\link{cube_santa_state}} and
#' \code{\link{cube_santa_state_out}} carry a state across, and
#' \code{\link{cube_santa_perm}} carries the relabelling itself.
#'
#' @name cube_santa
#' @seealso \code{\link{cube_santa_moves}}, \code{\link{cube_santa_state}},
#'   \code{\link{cube_moves}}, \code{\link{cube_nnn}}
NULL

## Santa's faces in our numbering: their 0..5 are our U F R B L D. Our own
## faces are U R F D L B = 0..5, so this is the one table the file needs; the
## moves come from geometry below rather than from a table.
.santa_faces <- c(0L, 2L, 1L, 5L, 4L, 3L)

## A Santa move name as an axis, a layer and a number of quarter turns in the
## package's own vocabulary. Everything irregular about the names lives here.
.santa_move_geometry <- function(name, n) {
  inverse <- substr(name, 1L, 1L) == "-"
  body <- if (inverse) substring(name, 2L) else name
  letter <- substr(body, 1L, 1L)
  if (!letter %in% c("f", "r", "d"))
    stop("cube_santa: unknown move '", name, "'", call. = FALSE)
  idx <- suppressWarnings(as.integer(substring(body, 2L)))
  if (is.na(idx) || idx < 0L || idx >= n)
    stop("cube_santa: layer index out of range in '", name,
         "' for n = ", n, call. = FALSE)

  # f and r count layers from the positive end of their axis and turn three
  # quarters; d counts from the negative end and turns one. That asymmetry is
  # Santa's, not ours.
  if (letter == "d") {
    c(axis = 2L, layer = idx + 1L, turns = if (inverse) 3L else 1L)
  } else {
    c(axis = if (letter == "r") 1L else 3L,
      layer = n - idx,
      turns = if (inverse) 1L else 3L)
  }
}

#' Santa 2023 Move Names for a Cube
#'
#' The \eqn{6n} move names Santa 2023 uses for a cube of side \eqn{n}, in the
#' order its puzzle files list them: \code{f0}, \code{-f0}, \code{f1} and so on
#' through \code{f}, \code{r} and \code{d}.
#'
#' @param n Integer, the side of the cube (at least 2)
#' @return Character vector of length \eqn{6n}
#' @export
#' @seealso \code{\link{cube_santa}}, \code{\link{cube_santa_moves}}
#' @examples
#' cube_santa_move_names(2)
#' cube_santa_move_names(4)
cube_santa_move_names <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 2L) stop("cube_santa: n must be at least 2", call. = FALSE)
  out <- character(0)
  for (letter in c("f", "r", "d"))
    for (i in seq_len(n) - 1L)
      out <- c(out, paste0(letter, i), paste0("-", letter, i))
  out
}

#' Translate Between Santa 2023 and Package Move Names
#'
#' \code{cube_santa_moves} names, for each Santa move, the package move that
#' does the same thing; \code{cube_moves_santa} is its inverse.
#'
#' The map is not letter for letter --- \code{f3} is \verb{B'} on a 4x4x4, and
#' \code{d3} is \verb{U'} --- so it is derived from the geometry each time
#' rather than tabulated. See \code{\link{cube_santa}} for why.
#'
#' @param n Integer, the side of the cube (at least 2)
#' @return Named character vector: names are the source notation, values the
#'   target one
#' @export
#' @seealso \code{\link{cube_santa}}, \code{\link{cube_santa_perm}}
#' @examples
#' m <- cube_santa_moves(4)
#' m[c("f0", "f3", "d3", "d1")]
#'
#' # and back again
#' cube_moves_santa(4)[c("F", "B'", "U'")]
cube_santa_moves <- function(n) {
  n <- as.integer(n)
  nms <- cube_santa_move_names(n)
  ours <- cube_move_names(n)
  key <- vapply(cube_moves(n), function(p) paste(p, collapse = ","), character(1))
  lookup <- stats::setNames(ours, key)

  out <- vapply(nms, function(nm) {
    g <- .santa_move_geometry(nm, n)
    k <- paste(cube_layer_move(n, g[["axis"]], g[["layer"]], g[["turns"]]),
               collapse = ",")
    v <- lookup[[k]]
    if (is.null(v)) stop("cube_santa: no package move matches '", nm, "'",
                         call. = FALSE)
    v
  }, character(1))
  stats::setNames(out, nms)
}

#' @rdname cube_santa_moves
#' @export
cube_moves_santa <- function(n) {
  m <- cube_santa_moves(n)
  stats::setNames(names(m), unname(m))
}

#' Sticker Relabelling Between Santa 2023 and This Package
#'
#' The permutation carrying a Santa 2023 sticker position to the package's own.
#' Santa numbers the faces \code{U F R B L D} and we number them
#' \code{U R F D L B}; within a face the two agree, so this permutation moves
#' whole faces and nothing else.
#'
#' @param n Integer, the side of the cube (at least 2)
#' @param inverse Logical, return the permutation the other way round
#' @return Integer vector of length \eqn{6n^2}
#' @export
#' @seealso \code{\link{cube_santa}}, \code{\link{cube_santa_state}}
#' @examples
#' p <- cube_santa_perm(3)
#' length(p)
#'
#' # applying it twice with the inverse gets back where it started
#' identical(cube_santa_perm(3, inverse = TRUE)[p], seq_len(54L))
cube_santa_perm <- function(n, inverse = FALSE) {
  n <- as.integer(n)
  if (is.na(n) || n < 2L) stop("cube_santa: n must be at least 2", call. = FALSE)
  f2 <- n * n
  out <- integer(6L * f2)
  for (f in 0:5) {
    to <- .santa_faces[f + 1L]
    out[(f * f2 + 1L):((f + 1L) * f2)] <- (to * f2 + 1L):((to + 1L) * f2)
  }
  if (inverse) {
    inv <- integer(6L * f2)
    inv[out] <- seq_along(out)
    inv
  } else {
    out
  }
}

#' Read and Write Santa 2023 Cube States
#'
#' \code{cube_santa_state} turns a Santa state --- the colours \code{0..5}, or
#' the letters a puzzle file writes them as --- into a package state.
#' \code{cube_santa_state_out} goes back the other way.
#'
#' Santa writes a state in one of two ways, and both are read here. Most of
#' its cube puzzles give a colour per sticker --- \code{0..5}, or the letters
#' \code{A} to \code{F} --- which is what \code{\link{cube_is_colour_solved}}
#' and the solvers compare. Some give a distinct number per sticker instead,
#' \code{0} to \eqn{6n^2 - 1}, which is a permutation and carries strictly more
#' information: it says which of four identically coloured centres is which.
#'
#' Both forms are relabelled the same way, because the relabelling acts on
#' positions. Colours are additionally renumbered, since a colour is a face
#' number and the two conventions order their faces differently; distinct
#' sticker numbers are renumbered too, into the package's \code{1..6n^2}.
#'
#' @param state For \code{cube_santa_state}, the Santa state: an integer vector
#'   of \eqn{6n^2} entries --- colours \code{0..5} or distinct sticker numbers
#'   \code{0..6n^2-1} --- or a character vector of face letters, or a single
#'   comma-separated string as the puzzle files store it. For
#'   \code{cube_santa_state_out}, a package state in either matching form.
#' @param n Integer, the side of the cube; inferred from the length when
#'   omitted
#' @return Integer vector of length \eqn{6n^2}: colours \code{0..5} numbered by
#'   the receiving convention's face order, or sticker numbers, matching
#'   whichever form was given. Package sticker numbers are 1-based and Santa's
#'   are 0-based.
#' @export
#' @seealso \code{\link{cube_santa}}, \code{\link{cube_santa_perm}}
#' @examples
#' # a solved Santa 3x3x3 is nine of each colour in face order
#' s <- rep(0:5, each = 9L)
#' cube_santa_state(s)
#'
#' # and back
#' identical(cube_santa_state_out(cube_santa_state(s)), s)
#'
#' # the permutation form: Santa's solved state is 0..53, ours is 1..54
#' identical(cube_santa_state(0:53), seq_len(54L))
cube_santa_state <- function(state, n = NULL) {
  parsed <- .santa_parse_state(state)
  n <- .santa_check_n(length(parsed$state), n)
  s <- parsed$state
  if (parsed$colours) {
    # A colour is a face number, so relabelling the positions is only half the
    # job -- the colours themselves are named in Santa's face order too.
    .santa_faces[s + 1L][cube_santa_perm(n, inverse = TRUE)]
  } else {
    # A sticker number names a position, so it is relabelled the same way the
    # positions are, and shifted from Santa's 0-based to our 1-based.
    sig <- cube_santa_perm(n)
    sig[s + 1L][cube_santa_perm(n, inverse = TRUE)]
  }
}

#' @rdname cube_santa_state
#' @export
cube_santa_state_out <- function(state, n = NULL) {
  parsed <- .santa_parse_state(state, ours = TRUE)
  n <- .santa_check_n(length(parsed$state), n)
  s <- parsed$state
  if (parsed$colours) {
    back <- integer(6L)
    back[.santa_faces + 1L] <- 0:5
    back[s + 1L][cube_santa_perm(n)]
  } else {
    inv <- cube_santa_perm(n, inverse = TRUE)
    (inv[s] - 1L)[cube_santa_perm(n)]
  }
}

## Accept the shapes a state arrives in: a comma-separated string as the CSV
## holds it, face letters, or numbers already. Report whether the numbers are
## colours or distinct sticker labels, since the two are relabelled
## differently -- a colour names a face, a sticker number names a position.
.santa_parse_state <- function(state, ours = FALSE) {
  colours <- NULL
  if (is.character(state)) {
    if (length(state) == 1L && grepl(",", state, fixed = TRUE))
      state <- strsplit(state, ",", fixed = TRUE)[[1L]]
    state <- trimws(state)
    if (!all(grepl("^[0-9]+$", state))) {
      # face letters. Santa's own face order is U F R B L D; the letters A..F
      # its later puzzle files use run in that same order.
      up <- toupper(state)
      key <- if (all(up %in% c("A", "B", "C", "D", "E", "F"))) {
        stats::setNames(0:5, c("A", "B", "C", "D", "E", "F"))
      } else {
        c(U = 0L, F = 1L, R = 2L, B = 3L, L = 4L, D = 5L)
      }
      unknown <- setdiff(up, names(key))
      if (length(unknown))
        stop("cube_santa: unknown face letter(s): ",
             paste(unknown, collapse = ", "), call. = FALSE)
      state <- unname(key[up])
      colours <- TRUE
    }
  }
  state <- as.integer(state)
  if (anyNA(state)) stop("cube_santa: state must be whole numbers", call. = FALSE)

  if (is.null(colours)) {
    len <- length(state)
    lo <- if (ours) 1L else 0L
    is_perm <- len == length(unique(state)) &&
      min(state) == lo && max(state) == lo + len - 1L
    colours <- !is_perm
  }
  if (colours) {
    lo <- 0L
    if (any(state < lo) || any(state > lo + 5L))
      stop("cube_santa: colours must be 0..5", call. = FALSE)
  }
  list(state = state, colours = colours)
}

.santa_check_n <- function(len, n) {
  if (is.null(n)) {
    n <- as.integer(round(sqrt(len / 6)))
    if (6L * n * n != len)
      stop("cube_santa: state of length ", len, " is not 6n^2", call. = FALSE)
  } else {
    n <- as.integer(n)
    if (6L * n * n != len)
      stop("cube_santa: state of length ", len, " does not match n = ", n,
           call. = FALSE)
  }
  if (n < 2L) stop("cube_santa: n must be at least 2", call. = FALSE)
  n
}

#' Translate a Santa 2023 Solution Path
#'
#' A Santa path is its moves joined by dots, as the submission files write
#' them. \code{cube_santa_path} turns one into a character vector of package
#' moves, and \code{cube_santa_path_out} turns package moves back into a Santa
#' path.
#'
#' Half turns have no name in either alphabet --- both count in quarter turns
#' --- so a package move written \code{U2} becomes two Santa moves.
#'
#' @param path For \code{cube_santa_path}, a Santa path: one dot-separated
#'   string, or a character vector of move names. For
#'   \code{cube_santa_path_out}, a character vector of package moves.
#' @param n Integer, the side of the cube (at least 2)
#' @return \code{cube_santa_path} returns a character vector of package moves;
#'   \code{cube_santa_path_out} returns one dot-separated string.
#' @export
#' @seealso \code{\link{cube_santa}}, \code{\link{cube_santa_moves}}
#' @examples
#' cube_santa_path("f0.-d3.r1", n = 4)
#' cube_santa_path_out(c("F", "U", "2x"), n = 4)
cube_santa_path <- function(path, n) {
  if (length(path) == 1L && grepl(".", path, fixed = TRUE))
    path <- strsplit(path, ".", fixed = TRUE)[[1L]]
  path <- trimws(path)
  path <- path[nzchar(path)]
  m <- cube_santa_moves(n)
  unknown <- setdiff(path, names(m))
  if (length(unknown))
    stop("cube_santa: unknown move(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  unname(m[path])
}

#' @rdname cube_santa_path
#' @export
cube_santa_path_out <- function(path, n) {
  path <- trimws(as.character(path))
  path <- path[nzchar(path)]
  m <- cube_moves_santa(n)

  # A half turn is a word in both alphabets, so expand it rather than fail.
  expanded <- unlist(lapply(path, function(mv) {
    if (grepl("2$", mv)) rep(sub("2$", "", mv), 2L) else mv
  }), use.names = FALSE)

  unknown <- setdiff(expanded, names(m))
  if (length(unknown))
    stop("cube_santa: unknown move(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  paste(unname(m[expanded]), collapse = ".")
}

#' A Cube Group in Santa 2023 Notation
#'
#' The same group \code{\link{cube_group}} builds, with its moves named the way
#' Santa 2023 names them. Solvers and searches work on it unchanged; only the
#' names in the paths they return differ.
#'
#' @param n Integer, the side of the cube (at least 2)
#' @return An external pointer to the group, as \code{\link{cube_group}} returns
#' @export
#' @seealso \code{\link{cube_santa}}, \code{\link{cube_group}}
#' @examples
#' g <- cube_santa_group(3)
#' group_apply(g, cube_identity(3), c("f0", "-d2"))
cube_santa_group <- function(n) {
  n <- as.integer(n)
  m <- cube_santa_moves(n)
  g <- cube_moves(n)
  perm_group(stats::setNames(g[unname(m)], names(m)), n = 6L * n * n,
             name = sprintf("cube%dx%dx%d-santa", n, n, n))
}
