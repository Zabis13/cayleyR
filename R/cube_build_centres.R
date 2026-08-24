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

## ---- step 3: empty the U slice by shooting ---------------------------------
##
## With a layer built and turned to the bottom, the shots fire pieces off U
## onto the side faces. Every shot leaves D exactly as it found it -- measured,
## and the reason the stage is safe: whatever has been built and turned down
## cannot be disturbed however many shots follow.
##
## Four things decide which shot, and each was arrived at by measurement after
## a simpler rule failed:
##
##   aim, not improvement. The piece is named first and the shot chosen for it:
##     a centre on U belonging to a side face, a shot firing at that face, and
##     the U turn that brings the piece to that shot's entry slot. Asking
##     instead for a shot that raises the count refuses the ones that work --
##     over the stalls this replaces, every foreign piece had two shot-and-setup
##     pairs that sent it home and none of the thirty-two raised the total. A
##     piece going home displaces one already there, and the count recovers a
##     shot or two later.
##
##   reloading. A shot is a 4-cycle: it takes a piece off U and pulls another
##     up in exchange. Once U holds only pieces belonging to U there is nothing
##     to place, and the way on is to spend one to bring up another. The test
##     is the ammunition -- how many pieces on U belong to neither the top nor
##     the bottom -- and not the count, which a reload lowers by design.
##
##   a target face. Ranking by the total treats every centre alike, and
##     measured over thirty steps that took the total from 25 to 38 while the
##     faces went 1 3 4 8 6 3 to 6 6 6 8 6 6 -- five faces at six of eight and
##     none closed. Topping up five faces scores as much as closing one and is
##     easier to find. So one face leads the ranking and the rest of the cube
##     is the tie-break.
##
##   memory. Two pieces trading places leave every count where it was, so no
##     arithmetic rule sees the loop; a state already visited is simply not
##     entered again.
##
## What it does not do is finish. Measured over five scrambles, it takes a
## 5x5x5 from 25 of 48 centres to about 36, and closes one face of six. The
## last two or three pieces on U can be placed only at the cost of a side face
## that is not finished either, and neither a slack in that guard (tried: 30.4
## against 35.4) nor a target face (tried: 36.2 against 35.4) answers it.
## Pochmann's remedy is the tilt, which needs three finished faces to be legal
## and so cannot be reached from one. That is where this stops.

.stage3_ctx <- function(n) {
  C <- .lslice_ctx(n)
  C$shots <- cube_centre_shots(n)
  C$y <- cube_expand_move("y", n)
  C$d_face <- 3L
  C$movable <- is.na(C$fixed_orbit) | C$cs$orbit != C$fixed_orbit
  C$per_face <- sum(C$movable) / 6L
  C$u_sk <- C$cs$sticker[C$cs$face == 0L & C$movable]

  C$face_full <- function(st, f) {
    fr <- C$frame(st)
    sk <- C$cs$sticker[C$cs$face == f & C$movable]
    sum(C$colour_at(st, sk) == fr[f + 1L])
  }
  C$centres_home <- function(st) {
    fr <- C$frame(st)
    sum(C$colour_at(st, C$cs$sticker[C$movable]) ==
        fr[C$cs$face[C$movable] + 1L])
  }
  C$ammunition <- function(st) {
    fr <- C$frame(st)
    cols <- C$colour_at(st, C$u_sk)
    sum(cols != fr[1L] & cols != fr[C$d_face + 1L])
  }
  C$key <- function(st) paste(st[C$cs$sticker[C$movable]], collapse = ",")
  C
}

## The fullest unfinished side face, lowest index breaking a tie so that a run
## is repeatable. U is the working face and D holds what is built, so neither
## is a candidate.
.stage3_target <- function(st, C, skip = integer(0)) {
  pick <- function(exclude) {
    cand <- setdiff(0:5, c(0L, C$d_face, exclude))
    if (!length(cand)) return(NA_integer_)
    fill <- vapply(cand, function(f) C$face_full(st, f), integer(1))
    open <- fill < C$per_face
    if (!any(open)) return(NA_integer_)
    cand <- cand[open]
    cand[order(-fill[open], cand)][1]
  }
  t <- pick(skip)
  if (is.na(t)) t <- pick(integer(0))   # everything set aside: grind on
  t
}

.stage3_fire <- function(st, C, seen, target) {
  fr <- C$frame(st)
  col_up <- fr[1L]                 # a COLOUR: what belongs on top
  col_down <- fr[C$d_face + 1L]    # a COLOUR: what belongs on the bottom

  d_sk <- C$cs$sticker[C$cs$face == C$d_face & C$movable]
  d_before <- st[d_sk]

  side <- setdiff(0:5, c(0L, C$d_face))
  side_sk <- C$cs$sticker[C$cs$face %in% side & C$movable]
  side_fc <- C$cs$face[C$cs$face %in% side & C$movable]
  side_home <- function(x) {
    f2 <- C$frame(x)
    sum(C$colour_at(x, side_sk) == f2[side_fc + 1L])
  }
  before_side <- side_home(st)
  ammo_before <- C$ammunition(st)
  now <- C$centres_home(st)
  target_before <- if (is.na(target)) 0L else C$face_full(st, target)

  best <- NULL
  best_rank <- -1L

  for (u in 0:3) {
    base <- if (u) group_apply(C$g, st, rep("U", u)) else st
    cols <- C$colour_at(base, C$u_sk)

    for (k in seq_len(nrow(C$shots))) {
      w <- strsplit(C$shots$word[k], " ", fixed = TRUE)[[1]]
      cand <- group_apply(C$g, base, w)

      if (!identical(cand[d_sk], d_before)) next
      if (side_home(cand) < before_side) next
      if (exists(C$key(cand), envir = seen, inherits = FALSE)) next

      # Did some ammunition piece land on the face it belongs to? `to_face` is
      # a POSITION; the piece's home is its COLOUR sent through the frame. The
      # two are different questions, and reading one as the other counts the
      # rotation twice.
      placed <- FALSE
      for (i in seq_along(C$u_sk)) {
        col <- cols[i]
        if (col == col_up || col == col_down) next
        home <- which(fr == col) - 1L
        if (!length(home) || home != C$shots$to_face[k]) next
        j <- match(which(cand == base[C$u_sk[i]]), C$cs$sticker)
        if (!is.na(j) && C$cs$face[j] == home) { placed <- TRUE; break }
      }

      ammo_after <- C$ammunition(cand)
      gain <- C$centres_home(cand) - now
      target_gain <- if (is.na(target)) 0L
                     else C$face_full(cand, target) - target_before

      if (placed) {
        rank <- 10000L + 1000L * target_gain + 10L * gain +
                (if (ammo_after >= ammo_before) 1L else 0L)
      } else {
        if (ammo_after <= ammo_before) next
        rank <- 10L * (ammo_after - ammo_before)
      }

      if (rank > best_rank) {
        best_rank <- rank
        best <- list(st = cand, word = c(rep("U", u), w), placed = placed)
      }
    }
  }
  best
}

#' Empty the Top Face onto the Sides
#'
#' The third stage of Pochmann's centres, generalised. With a layer built by
#' \code{\link{cube_build_lslice}} and turned to the bottom, the shots of
#' \code{\link{cube_centre_shots}} fire pieces off U onto the side faces they
#' belong to. Every shot leaves D untouched, so what has been built and turned
#' down survives however many shots follow.
#'
#' @section How a shot is chosen:
#' The piece is named first and the shot chosen for it, not the other way
#' round: a centre on U belonging to a side face, a shot firing at that face,
#' and the number of U turns that bring the piece to that shot's entry slot.
#'
#' Asking instead for a shot that raises the count refuses the ones that work.
#' Measured on the positions where an earlier version stalled: every foreign
#' piece had two shot-and-setup pairs that sent it home, and not one of the
#' thirty-two combinations raised the total. A piece going home displaces one
#' that was there already, and the count recovers a shot or two later.
#'
#' A shot has a second use as well. It is a 4-cycle --- it takes a piece off U
#' and pulls another up in exchange --- so once U holds only pieces belonging
#' to U, the way on is to spend one to bring up another. Such a reload lowers
#' the count by design and is judged by the ammunition instead: how many pieces
#' on U belong to neither the top face nor the bottom.
#'
#' @section What it reaches:
#' Measured over five scrambles of a 5x5x5, it takes the cube from 25 centres
#' home to about 36 of 48, closing one face. It does not finish: the last two
#' or three pieces on U can be placed only at the cost of a side face that is
#' unfinished too. Pochmann answers that with a tilt --- turning an unfinished
#' face up to U --- but a tilt is only legal once three faces are done, and
#' from one it cannot be reached. Widening the guard was tried and lost ground
#' (30.4 centres against 35.4), and so was ranking by a target face (36.2).
#'
#' @param state Integer vector of \eqn{6n^2} stickers, with a layer already
#'   built and turned to the bottom.
#' @param n Side of the cube. Inferred from the length of \code{state} when
#'   absent.
#' @param max_shots Most shots to fire before giving up.
#' @return List with components:
#'   \item{state}{The cube after the moves}
#'   \item{path}{Character vector of moves}
#'   \item{count}{How many movable centres are home}
#'   \item{target}{How many there are}
#'   \item{faces}{How many faces are finished}
#'   \item{shots}{How many shots were fired}
#'   \item{reloads}{How many of those were reloads}
#' @export
#' @seealso \code{\link{cube_build_lslice}}, \code{\link{cube_centre_shots}},
#'   \code{\link{cube_centre_counts}}
#' @examples
#' set.seed(1)
#' g <- cube_group(5)
#' s <- group_apply(g, group_identity(g),
#'                  sample(cube_move_names(5), 40, replace = TRUE))
#' \donttest{
#' lay <- cube_build_lslice(s)
#' down <- group_apply(g, lay$state, cube_expand_word("z'", 5))
#' r <- cube_empty_u_slice(down)
#' r$count
#' r$faces
#' }
cube_empty_u_slice <- function(state, n = NULL, max_shots = 200L) {
  state <- as.integer(state)

  if (is.null(n)) {
    n <- sqrt(length(state) / 6)
    if (n != round(n) || n < 2)
      stop("cube_empty_u_slice: a state of ", length(state),
           " stickers is no cube; give n if it cannot be inferred",
           call. = FALSE)
    n <- as.integer(round(n))
  }
  n <- as.integer(n)
  if (length(state) != 6L * n * n)
    stop("cube_empty_u_slice: a ", n, "x", n, "x", n, " state has ",
         6L * n * n, " stickers, got ", length(state), call. = FALSE)

  C <- .stage3_ctx(n)
  n_movable <- sum(C$movable)
  if (!n_movable || !nrow(C$shots))
    return(list(state = state, path = character(0), count = 0L, target = 0L,
                faces = 0L, shots = 0L, reloads = 0L))

  seen <- new.env(hash = TRUE, parent = emptyenv())
  assign(C$key(state), TRUE, envir = seen)

  path <- character(0)
  fired <- 0L
  reloads <- 0L
  no_shot <- 0L
  skip <- integer(0)
  stuck <- 0L
  best_count <- C$centres_home(state)
  since_gain <- 0L
  st <- state

  # A face that will not grow for this many shots is set aside and the next
  # fullest worked on instead; the set is cleared as soon as any face gains.
  patience <- 6L
  # Steps in a row without the total improving. Memory catches a loop that
  # returns to a state; this catches one that wanders through fresh ones.
  stall_limit <- 12L

  for (guard in seq_len(max_shots)) {
    if (C$centres_home(st) >= n_movable) break

    target <- .stage3_target(st, C, skip)
    had <- if (is.na(target)) 0L else C$face_full(st, target)

    f <- .stage3_fire(st, C, seen, target)
    if (!is.null(f)) {
      st <- f$st
      assign(C$key(st), TRUE, envir = seen)
      path <- c(path, f$word)
      fired <- fired + 1L
      if (!isTRUE(f$placed)) reloads <- reloads + 1L
      no_shot <- 0L

      if (!is.na(target)) {
        if (C$face_full(st, target) > had) { stuck <- 0L; skip <- integer(0) }
        else {
          stuck <- stuck + 1L
          if (stuck >= patience) { skip <- c(skip, target); stuck <- 0L }
        }
      }

      now <- C$centres_home(st)
      if (now > best_count) { best_count <- now; since_gain <- 0L }
      else {
        since_gain <- since_gain + 1L
        if (since_gain >= stall_limit) break
      }
      next
    }

    # Every side face has been offered every U slot and none wanted anything.
    # Turn the cube about the vertical axis to bring a different pair into the
    # shots' fixed roles. D is on the axis, so nothing built is at risk.
    if (no_shot < 4L) {
      st <- group_apply(C$g, st, C$y)
      path <- c(path, C$y)
      no_shot <- no_shot + 1L
      next
    }
    break
  }

  list(state = st, path = path, count = C$centres_home(st),
       target = n_movable,
       faces = sum(vapply(0:5, function(f) C$face_full(st, f) == C$per_face,
                          logical(1))),
       shots = fired, reloads = reloads)
}
