#' The Centres of a Cube of Any Size, Derived
#'
#' What a method needs to know about centres before it can build them: which
#' stickers are centre stickers, which orbit each belongs to, where it sits on
#' its face, and where every slice turn sends it. All of it read off
#' \code{\link{cube_moves}} and \code{\link{cube_orbits}} rather than written
#' down per size.
#'
#' @section Why this is derived:
#' The 4x4x4 centre solver in \code{src/cube_centres.h} carries its slice table
#' as six hand-measured blocks of eight steps. That table is correct, and it is
#' also the reason the file solves one size: the numbers say
#' \code{f*16 + local}, the slots run 1 to 4, and both facts are assumptions
#' about a 4x4x4 rather than about cubes.
#'
#' Nothing in that table is independent of the permutations, though. A slice
#' turn is a permutation; asking it where each centre sticker goes reproduces
#' the hand table exactly --- all six blocks, at \code{n = 4} --- and asks the
#' same question of any other size. That equality is checked in the tests, so
#' the generic version is held to the hand-measured one rather than merely
#' believed.
#'
#' @section Slots:
#' A face carries several centre stickers of the same orbit, and a method has
#' to name them. They are numbered along the cycle of that face's own turn:
#' slot 1 is the lowest sticker index in the group, slot 2 is where a quarter
#' turn sends it, and so on. Numbering by the turn rather than by reading order
#' means slot arithmetic is the same on every face and at every size --- a
#' quarter turn always adds one, modulo the cycle length.
#'
#' @name cube_centre_map_family
#' @seealso \code{\link{cube_centre_structure}}, \code{\link{cube_slice_map}},
#'   \code{\link{cube_orbits}}
NULL

## Which outer layer turn is a given face's own turn.
##
## Both outer layers of an axis map a face's block onto itself: the near one
## turns it, the far one is the opposite face's turn and fixes every sticker of
## the block where it stands. So mapping the block onto itself is not enough to
## identify the turn -- it has to stir the block as well, and requiring that is
## what tells the two apart.
.cube_face_turn <- function(n, face) {
  blk <- face * n * n + seq_len(n * n)
  for (axis in 1:3) {
    for (layer in c(1L, as.integer(n))) {
      p <- cube_layer_move(n, axis, layer, 1L)
      if (all(p[blk] %in% blk) && any(p[blk] != blk))
        return(list(axis = axis, layer = layer))
    }
  }
  stop("cube_centre_structure: no face turn for face ", face, call. = FALSE)
}

#' Centre Stickers of an N x N x N Cube
#'
#' One row per centre sticker: which face it is on, where on that face, which
#' orbit it belongs to and which slot of its (face, orbit) group it occupies.
#'
#' A centre piece carries exactly one sticker, so pieces and stickers are the
#' same thing here and the table can be indexed either way.
#'
#' @param n Side of the cube, 2 or more.
#' @return A \code{data.frame} with columns \code{sticker} (1-based index into
#'   the state vector), \code{face} (0 to 5), \code{local} (0-based position
#'   within the face), \code{orbit} (as numbered by \code{\link{cube_orbits}})
#'   and \code{slot} (1-based, along the face-turn cycle).
#' @export
#' @seealso \code{\link{cube_slice_map}}, \code{\link{cube_centre_map_family}}
#' @examples
#' # a 4x4x4 has one centre orbit, four per face
#' cs <- cube_centre_structure(4)
#' table(cs$orbit)
#'
#' # a 5x5x5 has three: two of 24 and the six fixed centres
#' table(cube_centre_structure(5)$orbit)
#'
#' # the fixed centres of an odd cube are alone in their group
#' cs5 <- cube_centre_structure(5)
#' table(cs5$slot[cs5$orbit == max(cs5$orbit)])
cube_centre_structure <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 2L) stop("cube: n must be at least 2", call. = FALSE)
  face_size <- n * n

  orb <- cube_orbits(n)
  pieces <- cube_pieces(n)
  centre_orbits <- orb$orbit[orb$kind == "centre"]
  rows <- pieces[pieces$orbit %in% centre_orbits, , drop = FALSE]

  # One sticker per centre piece, so the split is one index each and the two
  # can be lined up without a join.
  st <- as.integer(unlist(strsplit(rows$stickers, ",", fixed = TRUE)))
  if (length(st) != nrow(rows))
    stop("cube_centre_structure: a centre piece with more than one sticker",
         call. = FALSE)

  # A 2x2x2 has no centre at all: every sticker belongs to a corner. The empty
  # table is the right answer, and it has to be built by hand because
  # data.frame() will not take a zero-length column beside a scalar one.
  if (!length(st))
    return(data.frame(sticker = integer(0), face = integer(0),
                      local = integer(0), orbit = integer(0),
                      slot = integer(0), stringsAsFactors = FALSE))

  out <- data.frame(
    sticker = st,
    face    = (st - 1L) %/% face_size,
    local   = (st - 1L) %% face_size,
    orbit   = rows$orbit,
    slot    = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (f in sort(unique(out$face))) {
    ft <- .cube_face_turn(n, f)
    p <- cube_layer_move(n, ft$axis, ft$layer, 1L)
    for (ob in sort(unique(out$orbit[out$face == f]))) {
      grp <- which(out$face == f & out$orbit == ob)
      ids <- out$sticker[grp]
      # p[dest] == src means the sticker at src is now at dest, so where a
      # sticker goes is which(p == it) -- not p[it], which is the other
      # direction and silently gives the cycle backwards.
      start <- min(ids)
      cur <- start
      for (k in seq_along(ids)) {
        out$slot[out$sticker == cur] <- k
        nxt <- which(p == cur)
        if (length(nxt) != 1L || nxt == start) break
        cur <- nxt
      }
    }
  }

  rownames(out) <- NULL
  out
}

#' Three-Cycles of Centres, One Per Orbit
#'
#' The commutator that moves three centres and nothing else. Steps that fill a
#' face leave the centres in arrangements a swap cannot reach --- a three-cycle
#' is an even permutation and a swap is odd --- so a method needs this shape as
#' well as the shots.
#'
#' @section Derived, and wider than the original:
#' \code{src/cube_centres.h} carries one such commutator, written out for a
#' 4x4x4: \code{1x U 2x' U' 1x' U 2x U'}, cycling a centre between faces 0, 2
#' and 5. The same shape is searched here instead --- over every ordered pair
#' of inner slices, about all three axes, conjugated by each of the six face
#' turns. Each candidate is checked to move exactly three centres of one orbit
#' before it is kept, so what comes back is a three-cycle by measurement rather
#' than by construction.
#'
#' The result is wider than the file's in two ways. Every moving orbit gets its
#' own commutators, not just the one a 4x4x4 has; and they reach all twenty
#' triples of faces rather than the single triple 0, 2, 5 that one fixed word
#' acts on. Both matter to a method, which has to cycle whichever three faces
#' the cube presents.
#'
#' @section The central slice belongs in the search:
#' An earlier version excluded \code{\link{cube_central_moves}} on the grounds
#' that turning the middle layer of an odd cube turns the whole cube. That is
#' true of the move by itself and false of a commutator built from it: only the
#' whole word is judged, and the frame it disturbs in the middle it puts back
#' at the end. Measured on a 5x5x5, 1644 of 2592 candidate words leave the
#' fixed centres exactly where they were.
#'
#' Excluding it cost the 5x5x5 an entire orbit. Its plus centres are cycled by
#' \code{1x U 2x' U' 1x' U 2x U'} and nothing else of this shape --- one
#' character from the corner centres' own word, and that character is the
#' central slice.
#'
#' @param n Side of the cube, 4 or more. Smaller cubes have no movable centres
#'   and get no rows.
#' @param all Whether to return every commutator found. The default keeps one
#'   per (orbit, face triple), which is what a method needs; \code{TRUE} keeps
#'   all of them.
#' @return A \code{data.frame} with columns \code{word} (the eight moves, space
#'   separated), \code{orbit} (which centre orbit it cycles), \code{faces}
#'   (the three faces involved, comma separated, ascending) and \code{slots}
#'   (the slot on each, in that same face order).
#' @export
#' @seealso \code{\link{cube_centre_shots}},
#'   \code{\link{cube_centre_structure}}, \code{\link{cube_central_moves}}
#' @examples
#' # a 4x4x4 has one moving orbit, cycled between many triples of faces
#' cyc4 <- cube_centre_cycles(4)
#' unique(cyc4$orbit)
#' length(unique(cyc4$faces))
#'
#' # a 5x5x5 has two, and the plus centres need the central slice
#' cyc5 <- cube_centre_cycles(5)
#' unique(cyc5$orbit)
cube_centre_cycles <- function(n, all = FALSE) {
  n <- as.integer(n)
  cs <- cube_centre_structure(n)
  empty <- data.frame(word = character(0), orbit = integer(0),
                      faces = character(0), slots = character(0),
                      stringsAsFactors = FALSE)
  if (!nrow(cs)) return(empty)

  # Every inner slice, the central one included -- see the note above on why
  # excluding it was wrong.
  slices <- grep("^[0-9]+[xyz]$", cube_move_names(n), value = TRUE)
  if (length(slices) < 2L) return(empty)
  faces <- c("U", "R", "F", "D", "L", "B")

  fixed_orbit <- if (n %% 2L == 1L) {
    per <- table(cs$orbit)
    as.integer(names(per)[per == 6L])[1]
  } else NA_integer_

  g <- cube_group(n)
  id <- group_identity(g)
  rows <- list()
  seen <- character(0)

  for (a in slices) for (b in slices) {
    if (a == b) next
    for (f in faces) {
      fi <- paste0(f, "'")
      word <- c(a, f, paste0(b, "'"), fi, paste0(a, "'"), f, b, fi)
      s <- group_apply(g, id, word)

      moved <- cs[s[cs$sticker] != cs$sticker, , drop = FALSE]
      # A three-cycle moves three pieces. Anything else is a different shape
      # and no use for placing one piece among many.
      if (nrow(moved) != 3L) next
      orbit <- unique(moved$orbit)
      if (length(orbit) != 1L) next
      # The fixed centres of an odd cube cannot be cycled -- a word that seems
      # to has turned the cube instead.
      if (!is.na(fixed_orbit) && orbit == fixed_orbit) next

      ord <- order(moved$face)
      key <- paste(orbit, paste(moved$face[ord], collapse = ","))
      if (!all && key %in% seen) next
      seen <- c(seen, key)

      rows[[length(rows) + 1L]] <- data.frame(
        word   = paste(word, collapse = " "),
        orbit  = orbit,
        faces  = paste(moved$face[ord], collapse = ","),
        slots  = paste(moved$slot[ord], collapse = ","),
        stringsAsFactors = FALSE)
    }
  }

  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out <- out[order(out$orbit, out$faces), ]
  rownames(out) <- NULL
  out
}

#' The Layer Beside a Face, and What Leaves It Alone
#'
#' Building centres face by face does not work: the commutators that fill one
#' face disturb the four beside it, so a face finished early is taken apart
#' later. Pochmann's method builds a \emph{layer} instead --- one face together
#' with the column of each side face lying against it --- because a layer can
#' be turned to the bottom and kept there.
#'
#' \code{cube_lslice_cells} is that layer, and \code{cube_free_moves} is the
#' vocabulary that may be used once it is built: the single moves that leave
#' every one of its centres where it stands.
#'
#' @section Derived, not tabulated:
#' \code{src/cube_centres.h} writes the layer out as twelve hand-measured cells
#' and names the free moves in a comment. Both are read off the permutations
#' here instead: the layer is what a wide L turn moves, and a move is free when
#' it moves none of it. At \code{n = 4} the two agree exactly --- twelve cells,
#' and the free moves \code{R}, \code{R'}, \code{2x}, \code{2x'} --- which the
#' tests check. At other sizes the same questions have bigger answers: twenty
#' cells on a 5x5x5, thirty-two on a 6x6x6.
#'
#' @param n Side of the cube, 2 or more.
#' @return For \code{cube_lslice_cells}, a \code{data.frame} of the layer's
#'   centres with columns \code{sticker}, \code{face}, \code{orbit} and
#'   \code{slot}. For \code{cube_free_moves}, a character vector of move names.
#' @export
#' @seealso \code{\link{cube_centre_shots}},
#'   \code{\link{cube_centre_structure}}
#' @examples
#' # the twelve centres of a 4x4x4's l-slice: all of L, two of each side face
#' table(cube_lslice_cells(4)$face)
#'
#' # what may be turned once it is built
#' cube_free_moves(4)
#'
#' # the same questions at another size
#' nrow(cube_lslice_cells(5))
#' cube_free_moves(5)
cube_lslice_cells <- function(n) {
  n <- as.integer(n)
  cs <- cube_centre_structure(n)
  if (!nrow(cs))
    return(cs[0, c("sticker", "face", "orbit", "slot"), drop = FALSE])

  g <- cube_group(n)
  moved <- group_apply(g, group_identity(g), cube_expand_move("Lw", n))
  out <- cs[moved[cs$sticker] != cs$sticker,
            c("sticker", "face", "orbit", "slot"), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname cube_lslice_cells
#' @export
cube_free_moves <- function(n) {
  n <- as.integer(n)
  cells <- cube_lslice_cells(n)$sticker
  if (!length(cells)) return(cube_move_names(n))

  g <- cube_group(n)
  id <- group_identity(g)
  nm <- cube_move_names(n)
  nm[vapply(nm, function(mv) all(group_apply(g, id, mv)[cells] == cells),
            logical(1))]
}

#' Slice Turns That Reorient the Whole Cube
#'
#' On an odd cube the middle layer of an axis carries that axis's fixed
#' centres, so turning it is a rotation of the cube rather than a move within
#' it. A method that means to keep a face where it is must leave these alone.
#'
#' Which moves these are is found by asking, not by counting layers: a move is
#' central exactly when it disturbs the six fixed centres. An even cube has no
#' fixed centres and so no central moves, and the answer is the empty vector.
#'
#' @param n Side of the cube, 2 or more.
#' @return Character vector of move names, empty on an even cube.
#' @export
#' @seealso \code{\link{cube_centre_shots}}, \code{\link{cube_centre_structure}}
#' @examples
#' cube_central_moves(5)   # 2x, 2y, 2z and their inverses
#' cube_central_moves(4)   # none: an even cube has no middle layer
cube_central_moves <- function(n) {
  n <- as.integer(n)
  cs <- cube_centre_structure(n)
  if (!nrow(cs)) return(character(0))

  # The fixed centres are the orbit with one piece per face. On an even cube
  # there is no such orbit and nothing to protect.
  per_orbit <- table(cs$orbit)
  fixed_orbit <- names(per_orbit)[per_orbit == 6L]
  if (!length(fixed_orbit)) return(character(0))
  fixed <- cs$sticker[cs$orbit == as.integer(fixed_orbit[1])]

  moves <- cube_moves(n)
  nm <- cube_move_names(n)
  nm[vapply(nm, function(mv) any(moves[[mv]][fixed] != fixed), logical(1))]
}

#' Commutators That Carry Centres Off One Face and Spare Another
#'
#' The generic form of the eight shots that \code{src/cube_centres5.h} lists by
#' hand for a 5x5x5. A shot is a conjugation --- a slice turn, a turn of the
#' working face, then the slice back --- and what makes it usable is
#' what it does not do: the kept face's centres come back exactly where they
#' were, so whatever has already been built there survives however many shots
#' are fired afterwards.
#'
#' @section What is required of a shot:
#' Three conditions, each checked against the permutations rather than assumed.
#' The kept face must be untouched. Some centre must actually cross between
#' faces, which is what rules out the conjugations by a turn of the working
#' face's own axis --- they stir that face without emptying it. And the slice
#' must not be a central one, since on an odd cube that turns the whole cube
#' and moves the kept face out from under the method
#' (\code{\link{cube_central_moves}}).
#'
#' @param n Side of the cube, 2 or more.
#' @param keep_face Face whose centres must not move, 0 to 5. Defaults to the
#'   face at the negative end of the y axis, which is D.
#' @param turn_face Name of the face turn conjugated, default \code{"U"}.
#' @return A \code{data.frame} with columns \code{word} (the three moves,
#'   space separated), \code{to_face} (the face centres are carried to),
#'   \code{n_moved} (how many centre stickers move) and \code{orbits} (which
#'   centre orbits they belong to, comma separated). No rows if the size has no
#'   moving centres.
#' @export
#' @seealso \code{\link{cube_central_moves}},
#'   \code{\link{cube_centre_structure}}
#' @examples
#' # a 5x5x5 has eight, two onto each side face
#' s <- cube_centre_shots(5)
#' table(s$to_face)
#'
#' # each carries both moving orbits at once, which is why the two are solved
#' # together rather than in turn
#' unique(s$orbits)
cube_centre_shots <- function(n, keep_face = NULL, turn_face = "U") {
  n <- as.integer(n)
  cs <- cube_centre_structure(n)
  empty <- data.frame(word = character(0), to_face = integer(0),
                      n_moved = integer(0), orbits = character(0),
                      stringsAsFactors = FALSE)
  if (!nrow(cs)) return(empty)

  if (is.null(keep_face)) keep_face <- .cube_face_at(n, axis = 2L, layer = 1L)
  keep <- cs$sticker[cs$face == keep_face]
  if (!length(keep)) return(empty)

  central <- cube_central_moves(n)
  nm <- cube_move_names(n)
  slices <- setdiff(grep("^[0-9]+[xyz]'?$", nm, value = TRUE), central)

  g <- cube_group(n)
  id <- group_identity(g)
  rows <- list()

  for (a in slices) {
    back <- if (grepl("'", a, fixed = TRUE)) sub("'", "", a) else paste0(a, "'")
    if (!(back %in% nm)) next

    word <- c(a, turn_face, back)
    s <- group_apply(g, id, word)
    if (any(s[keep] != keep)) next               # the kept face moved

    moved <- cs$sticker[s[cs$sticker] != cs$sticker]
    faces <- unique(cs$face[cs$sticker %in% moved])
    # A shot has to empty the working face onto another one. A conjugation by
    # the working face's own axis only stirs it, which is no use for building.
    away <- setdiff(faces, .cube_face_of_move(n, turn_face))
    if (!length(away)) next

    rows[[length(rows) + 1L]] <- data.frame(
      word    = paste(word, collapse = " "),
      to_face = away[1],
      n_moved = length(moved),
      orbits  = paste(sort(unique(cs$orbit[cs$sticker %in% moved])),
                      collapse = ","),
      stringsAsFactors = FALSE)
  }

  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

## Which face sits at one end of an axis: the face whose block that outer
## layer turn stirs.
.cube_face_at <- function(n, axis, layer) {
  p <- cube_layer_move(n, axis, layer, 1L)
  for (f in 0:5) {
    blk <- f * n * n + seq_len(n * n)
    if (all(p[blk] %in% blk) && any(p[blk] != blk)) return(f)
  }
  stop("cube_centre_shots: no face at axis ", axis, " layer ", layer,
       call. = FALSE)
}

## Which face a named face turn belongs to, found the same way.
.cube_face_of_move <- function(n, move) {
  p <- cube_moves(n)[[move]]
  if (is.null(p))
    stop("cube_centre_shots: no move '", move, "'", call. = FALSE)
  for (f in 0:5) {
    blk <- f * n * n + seq_len(n * n)
    if (all(p[blk] %in% blk) && any(p[blk] != blk)) return(f)
  }
  stop("cube_centre_shots: '", move, "' is not a face turn", call. = FALSE)
}

#' Where a Slice Turn Sends Each Centre
#'
#' The generic form of the slice table that \code{src/cube_centres.h} writes
#' out by hand for a 4x4x4: one row per centre sticker the turn moves, saying
#' which (face, orbit, slot) it leaves and which it arrives at.
#'
#' @param n Side of the cube, 2 or more.
#' @param move A move name from \code{\link{cube_move_names}}, such as
#'   \code{"1x"} or \code{"U"}.
#' @param structure Optionally the result of
#'   \code{\link{cube_centre_structure}} for the same \code{n}, to avoid
#'   recomputing it when mapping many moves.
#' @return A \code{data.frame} with columns \code{from_face}, \code{from_orbit},
#'   \code{from_slot}, \code{to_face}, \code{to_orbit} and \code{to_slot}, with
#'   no rows if the move leaves every centre alone.
#' @export
#' @seealso \code{\link{cube_centre_structure}},
#'   \code{\link{cube_centre_map_family}}
#' @examples
#' # a 4x4x4 slice moves eight centres, in one orbit
#' m <- cube_slice_map(4, "1x")
#' nrow(m)
#'
#' # no slice ever mixes two orbits, at any size
#' m5 <- cube_slice_map(5, "1x")
#' all(m5$from_orbit == m5$to_orbit)
cube_slice_map <- function(n, move, structure = NULL) {
  n <- as.integer(n)
  cs <- if (is.null(structure)) cube_centre_structure(n) else structure

  perm <- cube_moves(n)[[move]]
  if (is.null(perm))
    stop("cube_slice_map: no move '", move, "' on a ", n, "x", n, "x", n,
         call. = FALSE)

  from <- cs$sticker
  dest <- match(from, perm)          # where each centre sticker goes
  keep <- which(dest != from)
  j <- match(dest[keep], cs$sticker)

  # A centre can only land on a centre -- the orbits guarantee it -- so an
  # unmatched destination means the structure and the move disagree.
  if (anyNA(j))
    stop("cube_slice_map: a centre left the centre set", call. = FALSE)

  data.frame(
    from_face  = cs$face[keep],
    from_orbit = cs$orbit[keep],
    from_slot  = cs$slot[keep],
    to_face    = cs$face[j],
    to_orbit   = cs$orbit[j],
    to_slot    = cs$slot[j],
    stringsAsFactors = FALSE
  )
}

#' The Layer Beside a Face, as the 4x4x4 Solver Sees It
#'
#' The twelve centre cells of the layer lying against one face: that face's own
#' four, together with the column each of the four faces around it presents to
#' it. This is what step 2 of the reduction builds and what step 3 must not
#' disturb.
#'
#' @section Derived from the slice table:
#' \code{src/cube_centres.h} used to write two of these out by hand --- one for
#' L, which step 2 builds, and one for D, where the rotation puts it. Both are
#' read off the measured slice map instead: the layer of a face is that face
#' plus whatever the slice lying beside it carries, and which of the two slices
#' on the axis is the near one is settled by turning a solved cube and seeing
#' which leaves the layer whole.
#'
#' The slots are numbered as \code{centre_slots_of} in the C++ numbers them,
#' which is not the numbering \code{\link{cube_centre_structure}} uses --- that
#' one runs along the face-turn cycle. The two tables are not comparable cell by
#' cell.
#'
#' @param face Face index, 0 to 5, in the order U R F D L B.
#' @return A \code{data.frame} of twelve rows with columns \code{face} and
#'   \code{slot}.
#' @seealso \code{\link{cube_reduce_cpp}}, \code{\link{cube_slice_map}}
#' @examples
#' # the layer beside L: all of L, and a column of each face around it
#' cube_slice_cells_cpp(4)
#'
#' # every face has one, and each is twelve cells
#' vapply(0:5, function(f) nrow(cube_slice_cells_cpp(f)), integer(1))
#' @name cube_slice_cells_cpp
#' @export
NULL
