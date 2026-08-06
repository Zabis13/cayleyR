#' Permutation Groups
#'
#' A \code{perm_group} is what the rest of the package means by "the puzzle
#' being solved": a state length, an alphabet of named moves, and a way to
#' apply one. Search functions take a group and work through it, so the same
#' BFS, the same store and the same iterative solver run over TopSpin, over the
#' 3x3x3 cube, or over any group you declare yourself.
#'
#' Two constructors cover the built-in puzzles --- \code{\link{topspin_group}}
#' and \code{\link{cube_group}} --- and \code{perm_group} itself takes an
#' arbitrary table of permutations, which is how a new puzzle enters without a
#' line of C++.
#'
#' @section What a group must supply:
#' Moves are given as permutations of \code{1:n}, each read as
#' \code{new[i] <- state[perm[i]]}: entry \code{i} says where position \code{i}
#' draws its new value from. This is the same convention
#' \code{\link{cube_moves}} is written in, so that table can be handed over
#' unchanged.
#'
#' Inverses are worked out by composition, not by naming, so you need not say
#' which move undoes which. A move whose inverse is absent from the alphabet is
#' recorded as having none, and functions that need to walk a path backwards
#' will say so rather than guess.
#'
#' @section Moves versus alphabet:
#' The group fixes the alphabet; a search may use any subset of it. Every
#' search function still takes \code{moves}, now naming a subset of the group's
#' moves rather than defining them. Leaving it out means the whole alphabet.
#'
#' @name perm_group_intro
#' @seealso \code{\link{perm_group}}, \code{\link{topspin_group}},
#'   \code{\link{cube_group}}
NULL

#' Create a Permutation Group from a Table of Moves
#'
#' Declares a group by listing its moves as explicit permutations. This is the
#' general constructor: any puzzle whose moves permute a fixed set of positions
#' can be expressed here, with no C++ involved.
#'
#' @param perms Named list of integer vectors, each a permutation of \code{1:n}
#'   read as \code{new[i] <- state[perm[i]]}. Names become the move names.
#' @param n Integer, the state length. Defaults to the length of the first
#'   permutation.
#' @param name Character, a label for printing (default \code{"perm_group"})
#' @return An object of class \code{perm_group}
#' @export
#' @seealso \code{\link{topspin_group}}, \code{\link{cube_group}},
#'   \code{\link{perm_group_intro}}
#' @examples
#' # the 3x3x3 cube, from the table the package already ships
#' g <- perm_group(cube_moves(3), n = 54, name = "cube3")
#' g
#'
#' # a group of your own: two generators on five points
#' g2 <- perm_group(list(
#'   a = c(2L, 3L, 4L, 5L, 1L),
#'   b = c(2L, 1L, 3L, 4L, 5L)
#' ))
#' group_moves(g2)
perm_group <- function(perms, n = NULL, name = "perm_group") {
  if (!is.list(perms) || length(perms) == 0L) {
    stop("perm_group: perms must be a non-empty list of permutations")
  }
  if (is.null(names(perms)) || any(!nzchar(names(perms)))) {
    stop("perm_group: every move must be named")
  }
  if (anyDuplicated(names(perms))) {
    stop("perm_group: move names must be unique")
  }

  perms <- lapply(perms, as.integer)
  if (is.null(n)) n <- length(perms[[1L]])
  n <- as.integer(n)

  xp <- perm_group_create_table_cpp(n, names(perms), perms)
  new_perm_group(xp, name)
}

#' Create the TopSpin Group
#'
#' The group the package was built on: \code{L} rotates the ring one step left,
#' \code{R} one step right, and \code{X} reverses the first \code{k} elements.
#'
#' \code{k} is fixed here, at construction, and does not travel through the
#' search functions afterwards --- the group knows what its own \code{X} does.
#'
#' @param n Integer, the number of positions on the ring
#' @param k Integer, how many leading elements \code{X} reverses (default
#'   \code{n}, i.e. reverse the whole ring)
#' @param moves Character vector naming the alphabet, in either spelling:
#'   \code{c("L","R","X")} or \code{c("1","2","3")} (default all three, as
#'   \code{"1","2","3"}, which is the spelling the package has always used)
#' @return An object of class \code{perm_group}
#' @export
#' @seealso \code{\link{perm_group}}, \code{\link{cube_group}}
#' @examples
#' g <- topspin_group(12, k = 4)
#' g
#' group_moves(g)
#'
#' # a two-move alphabet: shifts only, no reversal
#' topspin_group(12, k = 4, moves = c("L", "R"))
topspin_group <- function(n, k = n, moves = c("1", "2", "3")) {
  n <- as.integer(n)
  k <- as.integer(k)
  moves <- as.character(moves)
  if (length(moves) == 0L) stop("topspin_group: need at least one move")
  if (anyDuplicated(moves)) stop("topspin_group: move names must be unique")

  xp <- perm_group_create_topspin_cpp(n, k, moves)
  g <- new_perm_group(xp, sprintf("topspin(n=%d, k=%d)", n, k))
  g$k <- k
  g
}

# Shared constructor: wraps the external pointer with the metadata R needs to
# talk about the group without crossing into C++ for every question.
new_perm_group <- function(xp, name) {
  info <- perm_group_info_cpp(xp)
  structure(
    list(
      ptr = xp,
      name = name,
      n = info$n,
      moves = as.character(info$move_names),
      inverse_of = info$inverse_of
    ),
    class = "perm_group"
  )
}

#' Print a Permutation Group
#'
#' Shows the group's name, the length of the states it works on, and its move
#' alphabet.
#'
#' @param x A \code{\link{perm_group}}
#' @param ... Ignored, present for consistency with \code{\link[base]{print}}
#' @return \code{x}, invisibly
#' @export
#' @seealso \code{\link{perm_group}}, \code{\link{group_moves}}
#' @examples
#' print(topspin_group(12, k = 4))
#' cube_group(3)
print.perm_group <- function(x, ...) {
  cat("<perm_group> ", x$name, "\n", sep = "")
  cat("  state length: ", x$n, "\n", sep = "")
  cat("  moves (", length(x$moves), "): ",
      paste(x$moves, collapse = " "), "\n", sep = "")
  invisible(x)
}

#' Test Whether an Object is a Permutation Group
#'
#' @param x Any object
#' @return Logical
#' @export
#' @examples
#' is_perm_group(topspin_group(10, 4))
#' is_perm_group(1:10)
is_perm_group <- function(x) inherits(x, "perm_group")

#' Move Alphabet of a Group
#'
#' @param group A \code{\link{perm_group}}
#' @return Character vector of move names
#' @export
#' @examples
#' group_moves(topspin_group(10, 4))
#' group_moves(cube_group(3))
group_moves <- function(group) {
  stopifnot(is_perm_group(group))
  group$moves
}

#' Solved State of a Group
#'
#' The identity, \code{1:n}, for whatever \code{n} the group works on.
#'
#' @param group A \code{\link{perm_group}}
#' @return Integer vector of length \code{n}
#' @export
#' @examples
#' group_identity(topspin_group(6, 3))
#' length(group_identity(cube_group(3)))
group_identity <- function(group) {
  stopifnot(is_perm_group(group))
  perm_group_identity_cpp(group$ptr)
}

#' Apply a Move Sequence Through a Group
#'
#' Applies moves left to right. Moves may be named or given as indices into the
#' group's alphabet.
#'
#' @param group A \code{\link{perm_group}}
#' @param state Integer vector, the state to act on
#' @param moves Move sequence: names, indices, or a single space-separated
#'   string
#' @return Integer vector, the state after the moves
#' @export
#' @seealso \code{\link{group_compose}}, \code{\link{group_inverse_seq}}
#' @examples
#' g <- cube_group(3)
#' s <- group_apply(g, group_identity(g), "R U R' U'")
#' sum(s != group_identity(g))
#'
#' tg <- topspin_group(6, k = 3)
#' group_apply(tg, 1:6, c("1", "1", "3"))
group_apply <- function(group, state, moves) {
  stopifnot(is_perm_group(group))
  idx <- group_move_index(group, moves)
  perm_group_apply_cpp(group$ptr, as.integer(state), idx)
}

#' Compose a Move Sequence into One Permutation
#'
#' Multiplies a word out into a single permutation of \code{1:n}. Applying that
#' permutation once does what the whole word does, which is what makes
#' repeating a word cheap.
#'
#' @param group A \code{\link{perm_group}}
#' @param moves Move sequence: names, indices, or a space-separated string
#' @return Integer vector of length \code{n}, the composed permutation
#' @export
#' @seealso \code{\link{group_apply}}, \code{\link{group_order}}
#' @examples
#' g <- cube_group(3)
#' p <- group_compose(g, "R U R' U'")
#' identical(group_identity(g)[p], group_apply(g, group_identity(g), "R U R' U'"))
group_compose <- function(group, moves) {
  stopifnot(is_perm_group(group))
  perm_group_compose_cpp(group$ptr, group_move_index(group, moves))
}

#' Invert a Move Sequence
#'
#' The word that undoes the given one: reversed, each move replaced by its
#' inverse. Errors if some move's inverse is not in the group's alphabet.
#'
#' @param group A \code{\link{perm_group}}
#' @param moves Move sequence: names, indices, or a space-separated string
#' @param as_names Logical, return names rather than indices (default
#'   \code{TRUE})
#' @return The inverse sequence, as names or indices
#' @export
#' @seealso \code{\link{group_apply}}
#' @examples
#' g <- cube_group(3)
#' group_inverse_seq(g, "R U R' U'")
#'
#' # a word and its inverse cancel
#' s <- group_apply(g, group_identity(g), "R U U F'")
#' identical(group_apply(g, s, group_inverse_seq(g, "R U U F'")), group_identity(g))
group_inverse_seq <- function(group, moves, as_names = TRUE) {
  stopifnot(is_perm_group(group))
  idx <- perm_group_inverse_seq_cpp(group$ptr, group_move_index(group, moves))
  if (isTRUE(as_names)) group$moves[idx] else idx
}

#' Order of a Move Sequence in a Group
#'
#' How many times a word must be repeated before the state returns to where it
#' started. This is the order of the word as a group element, and does not
#' depend on the starting state.
#'
#' @param group A \code{\link{perm_group}}
#' @param moves Move sequence: names, indices, or a space-separated string
#' @param max_reps Integer, cap on repetitions before giving up (default 100000)
#' @return Integer, the order, or \code{NA} if \code{max_reps} was reached
#' @export
#' @seealso \code{\link{group_compose}}
#' @examples
#' g <- cube_group(3)
#' group_order(g, "R")
#' group_order(g, "R U")
#' group_order(g, "R U R' U'")
group_order <- function(group, moves, max_reps = 100000L) {
  stopifnot(is_perm_group(group))
  idx <- group_move_index(group, moves)
  if (length(idx) == 0L) return(1L)

  perm <- group_compose(group, idx)
  id <- seq_len(group$n)
  cur <- perm
  for (i in seq_len(as.integer(max_reps))) {
    if (identical(cur, id)) return(i)
    cur <- cur[perm]
  }
  NA_integer_
}

#' Resolve Move Names to Indices Within a Group
#'
#' Accepts moves however they are spelled --- names, indices, or one
#' space-separated string --- and returns indices into the group's alphabet.
#' Every function taking a \code{moves} argument runs it through here, which is
#' why names and indices are interchangeable everywhere.
#'
#' Names are matched against the alphabet the group was built with, so a
#' TopSpin group made with the default \code{c("1","2","3")} spelling does not
#' answer to \code{"L"}: build it with \code{moves = c("L","R","X")} if that is
#' the spelling you want to use.
#'
#' @param group A \code{\link{perm_group}}
#' @param moves Move sequence: names, indices, or a space-separated string.
#'   \code{NULL} means the group's whole alphabet.
#' @return Integer vector of 1-based move indices
#' @export
#' @seealso \code{\link{group_moves}}
#' @examples
#' g <- cube_group(3)
#' group_move_index(g, "R U R' U'")
#' group_move_index(g, c(4, 1, 5, 2))
#'
#' # names resolve against the alphabet the group was built with
#' tg <- topspin_group(10, 4, moves = c("L", "R", "X"))
#' group_move_index(tg, c("L", "X"))
#'
#' # the default spelling is "1","2","3", so those are its names
#' group_move_index(topspin_group(10, 4), c("1", "3"))
group_move_index <- function(group, moves = NULL) {
  stopifnot(is_perm_group(group))
  if (is.null(moves)) return(seq_along(group$moves))
  if (length(moves) == 0L) return(integer(0))

  # a lone string may carry a whole word
  if (is.character(moves) && length(moves) == 1L &&
      grepl("[[:space:]]", moves)) {
    moves <- strsplit(trimws(moves), "[[:space:]]+")[[1]]
  }
  if (is.character(moves)) moves <- moves[nzchar(moves)]
  if (length(moves) == 0L) return(integer(0))

  if (is.numeric(moves)) {
    idx <- as.integer(moves)
    bad <- moves[is.na(idx) | idx < 1L | idx > length(group$moves)]
    if (length(bad) > 0L) {
      stop("move index out of range: ", paste(unique(bad), collapse = ", "),
           " (group has ", length(group$moves), " moves)")
    }
    return(idx)
  }

  tok <- as.character(moves)
  idx <- match(tok, group$moves)

  # a token may also be a bare index, which is how TopSpin's "1"/"2"/"3" keep
  # working whichever spelling the group was built with
  if (anyNA(idx)) {
    num <- suppressWarnings(as.integer(tok))
    ok <- is.na(idx) & !is.na(num) & num >= 1L & num <= length(group$moves) &
      tok == as.character(num)
    idx[ok] <- num[ok]
  }

  if (anyNA(idx)) {
    stop("unknown move(s): ", paste(unique(tok[is.na(idx)]), collapse = ", "),
         ". Group '", group$name, "' has: ",
         paste(group$moves, collapse = " "))
  }
  idx
}

#' Parse a Word Written as One String
#'
#' Combination words have always been written unseparated --- \code{"1231"} is
#' four TopSpin operations --- which reads only because each move is one
#' character. A cube's moves are not: \code{"R'M'"} has to be split on its move
#' names, not its characters. This does both, longest name first, so a group's
#' own spelling always wins over a shorter prefix of it.
#'
#' @param group A \code{\link{perm_group}}
#' @param word Character string, or a vector of them
#' @return For one string, an integer vector of move indices; for a vector, a
#'   list of them
#' @export
#' @seealso \code{\link{group_move_index}}
#' @examples
#' group_parse_word(topspin_group(10, 4), "1231")
#' group_parse_word(cube_group(3), "RM'R'")
#' group_parse_word(cube_group(3), "R M' R'")
group_parse_word <- function(group, word) {
  stopifnot(is_perm_group(group))
  if (length(word) != 1L) return(lapply(word, group_parse_word, group = group))

  word <- as.character(word)
  if (is.na(word) || !nzchar(word)) return(integer(0))

  # spaces, when present, settle it outright
  if (grepl("[[:space:]]", word)) return(group_move_index(group, word))

  # otherwise chew from the left, always taking the longest move that matches,
  # so "R'" is never read as "R" followed by a stray quote
  by_len <- group$moves[order(nchar(group$moves), decreasing = TRUE)]
  out <- integer(0)
  pos <- 1L
  n <- nchar(word)
  while (pos <= n) {
    hit <- NA_character_
    for (m in by_len) {
      if (substr(word, pos, pos + nchar(m) - 1L) == m) { hit <- m; break }
    }
    if (is.na(hit)) {
      stop("cannot parse '", word, "' at position ", pos,
           ": no move of group '", group$name, "' matches. Moves: ",
           paste(group$moves, collapse = " "))
    }
    out <- c(out, match(hit, group$moves))
    pos <- pos + nchar(hit)
  }
  out
}

# Internal: accept either a group or the legacy (n, k, moves) triple, so every
# search function can take `group =` without breaking a single existing call.
# When `group` is absent the arguments describe TopSpin, which is what they have
# always described.
#
# Returns both the group and the move subset to search with, because the two
# cases resolve `moves` differently: given a group, `moves` selects from its
# alphabet; given the legacy arguments, `moves` *is* the alphabet and the
# TopSpin group is built to match, so the subset is all of it.
resolve_group <- function(group, n, k = NULL, moves = NULL) {
  if (!is.null(group)) {
    if (!is_perm_group(group)) stop("group must be a perm_group object")
    if (!is.null(n) && group$n != n) {
      stop("state has length ", n, " but group '", group$name,
           "' works on length ", group$n)
    }
    return(list(group = group, moves = group_move_index(group, moves)))
  }
  if (is.null(k)) k <- n
  g <- topspin_group(n, k, if (is.null(moves)) c("1", "2", "3") else moves)
  list(group = g, moves = seq_along(g$moves))
}
