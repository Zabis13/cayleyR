## ---- the frame -------------------------------------------------------------
##
## Turning the cube bodily moves every face, so "the colour that belongs at
## position p" stops being "p". On an odd cube each face's fixed centre says
## outright what belongs there; on an even one there is no such sticker and the
## face keeps its own number, which holds as long as the cube has not been
## turned -- the state a scramble hands over.
##
## Reading the count in the wrong frame is the mistake this exists to prevent.
## cube_centres.h records what it looks like: 21 of 24 arrangements appear to
## collapse when nothing has been lost, because the rotation was not accounted
## for.
.lslice_ctx <- function(n) {
  g <- cube_group(n)
  cs <- cube_centre_structure(n)
  face_size <- n * n
  cells <- cube_lslice_cells(n)

  per <- table(cs$orbit)
  fixed_orbit <- if (n %% 2L == 1L && any(per == 6L))
    as.integer(names(per)[per == 6L])[1] else NA_integer_

  colour_at <- function(st, sk) as.integer((st[sk] - 1L) %/% face_size)

  frame <- function(st) vapply(0:5, function(f) {
    if (!is.na(fixed_orbit)) {
      a <- cs$sticker[cs$face == f & cs$orbit == fixed_orbit]
      if (length(a) == 1L) return(colour_at(st, a))
    }
    as.integer(f)
  }, integer(1))

  score <- function(st) {
    fr <- frame(st)
    sum(colour_at(st, cells$sticker) == fr[cells$face + 1L])
  }

  list(g = g, cs = cs, cells = cells, face_size = face_size,
       fixed_orbit = fixed_orbit, colour_at = colour_at, frame = frame,
       score = score, n = n, target = nrow(cells))
}

.invert_move <- function(m)
  if (grepl("'", m, fixed = TRUE)) sub("'", "", m, fixed = TRUE) else paste0(m, "'")
.invert_word <- function(w)
  rev(vapply(w, .invert_move, character(1), USE.NAMES = FALSE))

## ---- the ladder ------------------------------------------------------------
##
## l_slice_insert of cube_centres.h: single moves of the whole alphabet, then
## pairs, then triples, taking the first word that raises the count. `before` is
## read once and every candidate is judged whole against it -- the etalon's own
## worked example is "2x U'" running 8 -> 8 -> 9, where the first move improves
## nothing, which is why moves cannot be judged one at a time.
.lslice_ladder_step <- function(st, C, alphabet) {
  before <- C$score(st)
  g <- C$g

  for (a in alphabet) {
    x <- group_apply(g, st, a)
    if (C$score(x) > before) return(list(st = x, word = a, len = 1L))
  }
  for (a in alphabet) {
    xa <- group_apply(g, st, a)
    for (b in alphabet) {
      x <- group_apply(g, xa, b)
      if (C$score(x) > before) return(list(st = x, word = c(a, b), len = 2L))
    }
  }
  for (a in alphabet) {
    xa <- group_apply(g, st, a)
    for (b in alphabet) {
      xb <- group_apply(g, xa, b)
      for (cc in alphabet) {
        x <- group_apply(g, xb, cc)
        if (C$score(x) > before)
          return(list(st = x, word = c(a, b, cc), len = 3L))
      }
    }
  }
  NULL
}

## ---- which rotation brings a triple of faces to the commutator -------------
##
## The commutators act on faces 0, 2 and 5, and 2 and 5 are opposite. A rotation
## of a rigid body preserves adjacency, so only triples containing an opposite
## pair can ever be brought there -- twelve of the twenty. cube_centres.h
## measures the same twelve and notes that in practice the stalls only ever want
## those, which the runs here bear out.
##
## The table is walked rather than reasoned about: the 24 orientations are
## generated from the six rotations and each is asked which triple it delivers.
.lslice_rotations <- function(C) {
  rots <- c("x", "x'", "y", "y'", "z", "z'")
  g <- C$g
  id <- group_identity(g)

  face_perm <- function(w) {
    s <- group_apply(g, id, cube_expand_word(w, C$n))
    vapply(0:5, function(f) {
      sk <- C$cs$sticker[C$cs$face == f]
      as.integer(names(sort(table(C$colour_at(s, sk)), decreasing = TRUE))[1])
    }, integer(1))
  }
  rp <- lapply(rots, face_perm)
  names(rp) <- rots

  seen <- list(list(perm = 0:5, word = character(0)))
  i <- 1L
  while (i <= length(seen) && length(seen) < 24L) {
    for (r in rots) {
      cand <- rp[[r]][seen[[i]]$perm + 1L]
      if (any(vapply(seen, function(s) identical(s$perm, cand), logical(1))))
        next
      seen[[length(seen) + 1L]] <- list(perm = cand,
                                        word = c(seen[[i]]$word, r))
    }
    i <- i + 1L
  }

  tbl <- list()
  for (s in seen) {
    key <- paste(sort(s$perm[c(1L, 3L, 6L)]), collapse = ",")
    if (is.null(tbl[[key]])) tbl[[key]] <- s$word
  }
  tbl
}

.lslice_face_turn <- function(f, q) {
  nm <- c("U", "R", "F", "D", "L", "B")[f + 1L]
  if (q == 0L) character(0) else rep(nm, q)
}

## ---- one addressed three-cycle ---------------------------------------------
##
## Find three faces where a holds a piece of b and b holds one of d, rotate that
## triple onto the commutator's positions, turn each of the three faces so the
## pieces sit in the slots it reads, cycle, and undo. The third face need not
## point back at the first: any wrong piece of it will do, which is what catches
## a 4-cycle -- (a b c d) = (a b c)(c d), so three of the four are taken and a
## swap is left behind.
.lslice_cycle_step <- function(st, C, cycles, rotations) {
  before <- C$score(st)
  fr <- C$frame(st)
  g <- C$g
  movable <- is.na(C$fixed_orbit) | C$cs$orbit != C$fixed_orbit

  for (a in 0:5) for (b in 0:5) {
    if (b == a) next
    for (d in 0:5) {
      if (d == a || d == b) next

      ska <- C$cs$sticker[C$cs$face == a & movable]
      if (!any(C$colour_at(st, ska) == fr[b + 1L])) next
      skb <- C$cs$sticker[C$cs$face == b & movable]
      if (!any(C$colour_at(st, skb) == fr[d + 1L])) next
      skd <- C$cs$sticker[C$cs$face == d & movable]
      if (!any(C$colour_at(st, skd) != fr[d + 1L])) next

      key <- paste(sort(c(a, b, d)), collapse = ",")
      rot <- rotations[[key]]
      if (is.null(rot)) next

      to_pos <- if (length(rot)) cube_expand_word(rot, C$n) else character(0)
      from_pos <- .invert_word(to_pos)
      staged <- if (length(to_pos)) group_apply(g, st, to_pos) else st

      for (i in seq_len(nrow(cycles))) {
        body <- strsplit(cycles$word[i], " ", fixed = TRUE)[[1]]
        for (t0 in 0:3) for (t1 in 0:3) for (t2 in 0:3) {
          pre <- c(.lslice_face_turn(0, t0), .lslice_face_turn(2, t1),
                   .lslice_face_turn(5, t2))
          post <- .invert_word(pre)
          for (dir in 1:2) {
            w <- c(pre, if (dir == 1L) body else .invert_word(body), post)
            x <- group_apply(g, staged, w)
            y <- if (length(from_pos)) group_apply(g, x, from_pos) else x
            if (C$score(y) > before)
              return(list(st = y, word = c(to_pos, w, from_pos),
                          len = length(to_pos) + length(w) + length(from_pos),
                          triple = sort(c(a, b, d))))
          }
        }
      }
    }
  }
  NULL
}

#' Build the Layer Beside a Face, at Any Size
#'
#' The second stage of Pochmann's centres, generalised: fill the layer that
#' \code{\link{cube_lslice_cells}} names --- the L face together with the
#' column of each side face lying against it --- so that every centre in it
#' shows the colour of the face it sits on.
#'
#' @section Why a layer and not a face:
#' Building face by face does not work, and the reason is measurable: every
#' commutator that fills one face disturbs the four beside it, so a face
#' finished early is taken apart later. A layer can be turned to the bottom
#' and kept there, which is what makes the stage after this one safe.
#'
#' @section Two stages, and both are needed:
#' Bringing a piece into the layer and rearranging pieces already inside it are
#' different problems, and one vocabulary does not answer both.
#'
#' \strong{The ladder} does the first: single moves of the whole alphabet, then
#' pairs, then triples, taking the first word that raises the count. This is
#' \code{l_slice_insert} of \code{src/cube_centres.h}, whose own note is that
#' pairs carry the method --- its worked example, an inner slice followed by a
#' reversed U, runs 8, 8, 9, improving nothing on the first move, which is why
#' words are judged whole rather than one move at a time.
#'
#' \strong{Addressed three-cycles} do the second. The ladder stops when the
#' remaining pieces are inside the layer but in each other's places: no short
#' word rearranges the layer's own contents, so the count simply stops
#' improving, whatever the depth. A three-cycle moves exactly three centres and
#' leaves the rest standing, and pointing one at named positions closes what
#' the ladder cannot.
#'
#' Measured over ten scrambles at each size, the ladder alone finished 2 of 30
#' and the two together 30 of 30 --- reaching 17.9 of 20 on a 5x5x5 by itself
#' and 20 of 20 with the cycles.
#'
#' @section What the cycles need:
#' A commutator for every moving orbit, and the 5x5x5 is the case that shows
#' why: its plus centres are cycled only through the central slice. While that
#' was excluded from \code{\link{cube_centre_cycles}} on the grounds that a
#' central turn rotates the cube, half the pieces had no tool and builds
#' stalled on them.
#'
#' The triple of faces also has to be one a rotation can deliver. The
#' commutators act on faces 0, 2 and 5, two of which are opposite, and rotation
#' preserves adjacency --- so twelve of the twenty triples are reachable and
#' eight are not. In practice only reachable ones are ever wanted, which
#' \code{cube_centres.h} reports measuring too.
#'
#' @param state Integer vector of \eqn{6n^2} stickers.
#' @param n Side of the cube. Inferred from the length of \code{state} when
#'   absent.
#' @param max_rounds Most three-cycles to apply before giving up.
#' @return List with components:
#'   \item{state}{The cube after the moves}
#'   \item{path}{Character vector of moves}
#'   \item{count}{How many of the layer's centres are home}
#'   \item{target}{How many there are}
#'   \item{built}{Whether the layer was finished}
#'   \item{rounds}{How many three-cycles were used}
#' @export
#' @seealso \code{\link{cube_lslice_cells}}, \code{\link{cube_centre_cycles}},
#'   \code{\link{cube_centre_shots}}
#' @examples
#' set.seed(1)
#' g <- cube_group(4)
#' s <- group_apply(g, group_identity(g),
#'                  sample(cube_move_names(4), 40, replace = TRUE))
#' \donttest{
#' r <- cube_build_lslice(s)
#' r$built
#' length(r$path)
#' }
cube_build_lslice <- function(state, n = NULL, max_rounds = 40L) {
  state <- as.integer(state)

  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != round(n) || n < 2)
      stop("cube_build_lslice: a state of ", length(state),
           " stickers is no cube; give n if it cannot be inferred",
           call. = FALSE)
    n <- as.integer(round(n))
  }
  n <- as.integer(n)
  if (length(state) != 6L * n * n)
    stop("cube_build_lslice: a ", n, "x", n, "x", n, " state has ",
         6L * n * n, " stickers, got ", length(state), call. = FALSE)

  C <- .lslice_ctx(n)
  if (!C$target)
    return(list(state = state, path = character(0), count = 0L, target = 0L,
                built = TRUE, rounds = 0L))

  alphabet <- cube_move_names(n)
  cycles <- cube_centre_cycles(n)
  rotations <- if (nrow(cycles)) .lslice_rotations(C) else list()

  # Ladder to exhaustion, then a cycle, then the ladder again -- the cycle
  # opens positions the ladder can go on from, so alternating gets further than
  # either run to the end on its own.
  path <- character(0)
  rounds <- 0L
  cur <- state

  repeat {
    s <- .lslice_ladder_step(cur, C, alphabet)
    if (is.null(s)) break
    cur <- s$st
    path <- c(path, s$word)
  }

  while (C$score(cur) < C$target && rounds < max_rounds && nrow(cycles)) {
    cy <- .lslice_cycle_step(cur, C, cycles, rotations)
    if (is.null(cy)) break
    cur <- cy$st
    path <- c(path, cy$word)
    rounds <- rounds + 1L

    repeat {
      s <- .lslice_ladder_step(cur, C, alphabet)
      if (is.null(s)) break
      cur <- s$st
      path <- c(path, s$word)
    }
  }

  list(state = cur, path = path, count = C$score(cur), target = C$target,
       built = C$score(cur) >= C$target, rounds = rounds)
}
