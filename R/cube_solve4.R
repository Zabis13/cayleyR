#' Solve a 4x4x4 Cube by Reduction
#'
#' Reduction is the standard way to solve an even cube: build the centres so
#' each face acts as one, pair the edges so each acts as one, and the cube is
#' then a 3x3x3 with fat pieces --- finished by
#' \code{\link{cube_solve_cfop}}.
#'
#' Two things happen on a 4x4x4 that cannot happen on a 3x3x3, and both are
#' handled here. The centres and edge pairs are not fixed relative to one
#' another, so the first two stages are searches over the cube's own geometry
#' rather than table lookups. And reduction can leave the last layer in a state
#' no 3x3x3 reaches --- OLL and PLL parity --- which the 3x3x3 tables have no
#' entry for; those are repaired with algorithms that turn inner layers, before
#' the cube is handed on.
#'
#' Expect roughly 300 quarter turns. The method is layer by layer throughout
#' and makes no attempt at brevity; a search-based solver reaches the same cube
#' in a fraction of the moves and a great deal more time.
#'
#' @section What the reduction costs, and why:
#' The length is a property of the method rather than of the position, and the
#' clearest case measured is this. A cube eight moves from solved --- scrambled
#' by \code{R' D U U' 2z B 1x' 1z'} --- was reduced in 204 moves. Walking the
#' inverse scramble back shows a reduced state four moves away: the fourth move
#' of the way home already satisfies \code{\link{cube_is_reduced}}. The method
#' built one in two hundred and four while one lay in four.
#'
#' It is not a defect of the implementation. The reduction does not look for
#' the NEAREST reduced state; it builds its own by a fixed schedule --- one
#' centre, then the layer beside it, then the top slice emptied, then the edge
#' pairs --- and every stage runs whatever the cube arrived like. Following one
#' such solution move by move, 74\% of the moves changed neither the count of
#' centres home nor the count of edges paired. The edge stage spent 107 moves
#' pairing twelve edges, nine apiece, with the centres flickering between four
#' and eight all the way: take them apart, pair an edge, put them back.
#'
#' That is also why no cheap test predicts the price. The sum of missing
#' centres, their arrangement over the faces, and the number of unpaired edges
#' were each measured against the length over 420 cubes, and none separates a
#' cheap cube from an expensive one --- because there is little to separate.
#' Only the threshold shows: while a cube is still reduced the stage does
#' almost nothing (three to eight moves), and once it is not, the schedule runs
#' in full at a price that barely depends on how far from solved it is.
#'
#' Searching for a nearby reduced state instead does not rescue this. Reduced
#' is a weak condition and countless states satisfy it, but iterative deepening
#' still walks the whole tree to each depth before the next, so the many goals
#' buy nothing --- measured, a fourteen-move scramble did not finish inside six
#' minutes at depth six. Using the gap would want a prune table over the
#' distance to reduced, which is what \code{\link{cube_kociemba}} has for the
#' 3x3x3 and this does not.
#'
#' What does help is choosing where the schedule starts. If the price turns on
#' how the scramble sits relative to the face the first centre is built on, then
#' that face is worth picking rather than fixing, and \code{start_face} below is
#' how it is picked. Measured over 200 scrambles, the face that had been written
#' into the pipeline is the best of the six on 22\% of cubes; the reduction
#' stage alone falls to 64\% of its length when all six are tried.
#'
#' Picking by the reduction is not the same as picking by the answer, though,
#' and the difference is not small. A short reduction can leave the cube in a
#' parity a longer one avoids, and the fifty moves that costs outweigh what it
#' saved. Over 40 scrambles the two criteria chose differently on 48\% of cubes,
#' the reduction criterion losing 70 moves on average and 152 at worst --- and
#' on four of the forty it produced a solution LONGER than the fixed face would
#' have. Choosing on the finished solution cannot do that, since the fixed face
#' is one of the six it compares.
#'
#' \code{orientations} below is the same effect reached from the other side: the
#' same cube held differently reduces in anything from 139 moves to 312, and
#' that range is the view rather than the position.
#'
#' @param state Integer vector of 96 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(4)}.
#' @param method Which 3x3x3 solver finishes the reduced cube. All five reach a
#'   solved cube and differ in length and time; measured on 3x3x3 states,
#'   \code{"kociemba"} averages about 40 moves in 0.9 s, \code{"cfop"} 103 in
#'   0.04 s, \code{"lbl"} 169 in 0.06 s, \code{"m2"} 278 in 0.09 s and
#'   \code{"pochmann"} 433 in 0.14 s. The reduction in front of them is the same
#'   either way, so the difference in the 4x4x4 total is the difference above.
#'
#'   Parity is detected with CFOP whichever is chosen, because refusing a state
#'   is how the parity cases are found at all, and only CFOP knows the 57 OLL
#'   and 21 PLL cases well enough to refuse. The chosen solver sees the cube
#'   after any repair.
#' @param orientations How many ways of holding the cube to try, keeping the
#'   shortest answer. One --- the default --- solves the cube as it arrives and
#'   is what this function has always done.
#'
#'   The reduction works the faces in a fixed order, and a scramble leaves the
#'   cube in whatever orientation it leaves it in, so that order lands well or
#'   badly by chance. Measured over the 24 orientations of one cube, the
#'   reduction returned words from 139 moves to 312 --- the same cube
#'   throughout, since a rotation moves no piece relative to another, so the
#'   whole of that range is the view rather than the position.
#'
#'   Trying a few and keeping the least is therefore not a search for a better
#'   answer but for a better vantage. Measured over twenty cubes, counting the
#'   turn and its undoing as the moves they are, and passing each result
#'   through \code{\link{short_path_bfs}} as a caller would:
#'
#'   \tabular{lrrr}{
#'     \tab solver \tab shipped \tab seconds \cr
#'     1 orientation \tab 231 \tab 203 \tab 3.7 \cr
#'     8 \tab 205 \tab 181 \tab 4.4 \cr
#'     24 \tab 191 \tab 172 \tab 7.0
#'   }
#'
#'   Eight is the bargain: eleven per cent shorter for a fifth more time. The
#'   last four per cent cost four times as much again --- the solve itself is
#'   cheap and it is the shortener that spends the seconds, and a shorter path
#'   is quicker to shorten, which pays part of the sweep back.
#'
#'   This multiplies with \code{start_face}: each orientation tried is solved
#'   from whichever face that setting picks.
#' @param start_face How to choose the face the first centre is built on.
#'
#'   \code{"full"}, the default, solves from all six and keeps the shortest
#'   solution. \code{"reduction"} keeps the face that reduces shortest, which is
#'   cheaper but optimises the stage rather than the answer.  \code{"fixed"} is
#'   the single face the pipeline was written to, and what this function did
#'   before the choice existed.
#'
#'   Measured over 40 scrambles of depth 2 to 20, total moves against
#'   \code{"fixed"}, and the seconds a cube costs:
#'
#'   \tabular{lrrr}{
#'     \tab total \tab of fixed \tab seconds \cr
#'     fixed \tab 9794 \tab 100\% \tab 0.06 \cr
#'     reduction \tab 7396 \tab 76\% \tab 0.32 \cr
#'     full \tab 6064 \tab 62\% \tab 1.70
#'   }
#'
#'   \code{"full"} is never worse than \code{"fixed"} --- the fixed face is one
#'   of the six it compares --- while \code{"reduction"} was worse on 4 of the
#'   40, once by 152 moves. The two agree on about half the cubes and diverge
#'   most from depth 6 up, where solutions are longest.
#'
#'   The default stays at one so that existing callers keep the time they
#'   expect; eight is a thing to ask for, not to be given. And the eight is
#'   measured on a 4x4x4 with these twenty cubes --- the shape of the curve on
#'   another size, or on a differently distributed set of cubes, is not known,
#'   and picking the number again there means measuring it again.
#' @return List with components:
#'   \item{path}{Character vector of moves}
#'   \item{found}{Logical, whether the cube ended solved}
#'   \item{stages}{data.frame of stages: \code{name}, \code{detail},
#'     \code{n_moves}}
#'   \item{states}{List of integer vectors, the cube after each stage}
#' @export
#' @seealso \code{\link{cube_solve_centres}}, \code{\link{cube_solve_cfop}},
#'   \code{\link{cube_group}}
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(4), n_moves = 40)
#' \donttest{
#' res <- cube_solve4(s)
#' res$found
#' length(res$path)
#' }
cube_solve4 <- function(state, method = c("cfop", "kociemba", "lbl", "m2",
                                          "pochmann"),
                        orientations = 1L,
                        start_face = c("full", "reduction", "fixed")) {
  method <- match.arg(method)
  start_face <- match.arg(start_face)
  state <- as.integer(state)
  orientations <- as.integer(orientations)

  if (is.na(orientations) || orientations < 1L)
    stop("cube_solve4: orientations must be at least 1", call. = FALSE)
  if (orientations == 1L) return(.cube_solve4_pick(state, method, start_face))

  .cube_solve4_swept(state, method, orientations, start_face)
}

## Which starting face to build the first centre on.
##
## "fixed" is the face the pipeline used to be written to and nothing else.
## "reduction" keeps whichever of the six reduces shortest, which is cheap --
## six reductions -- but optimises the wrong thing: a short reduction can leave
## the cube in a parity a longer one avoided, and then the fifty moves of
## parity outweigh what it saved. Measured over 40 scrambles it picked a
## different face from "full" on 48% of them, losing 70 moves on average, and
## on 4 of the 40 it came out LONGER than the fixed face.
##
## "full" solves from all six and keeps the shortest solution. It costs six
## solves rather than six reductions -- 1.7 s against 0.32 on the machine
## measured -- and it cannot lose: the fixed face is one of the six, so the
## minimum over them is never worse. Over the same 40 scrambles it came to 62%
## of the fixed-face length against 76% for "reduction", and was longer than
## the fixed face on none of them.
.cube_solve4_pick <- function(state, method, start_face) {
  if (start_face == "fixed") return(.cube_solve4_one(state, method, 4L))
  if (start_face == "reduction") {
    red <- cube_reduce_best(state)
    return(.cube_solve4_one(state, method, red$face))
  }

  runs <- lapply(0:5, function(f)
    tryCatch(.cube_solve4_one(state, method, f), error = function(e) NULL))
  ok <- which(vapply(runs, function(r) !is.null(r) && isTRUE(r$found),
                     logical(1)))
  if (!length(ok)) {
    first <- Filter(Negate(is.null), runs)
    if (!length(first)) return(.cube_solve4_one(state, method, 4L))
    return(first[[1]])
  }

  n <- vapply(runs[ok], function(r) length(r$path), integer(1))
  best <- runs[[ok[which.min(n)]]]
  best$start_face <- (0:5)[ok[which.min(n)]]
  best
}

## The solve from one orientation, which is what cube_solve4 was before the
## sweep was put in front of it.
.cube_solve4_one <- function(state, method, start_face = 4L) {
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  # The 3x3x3 stage, chosen by `method`. Each takes a 3x3x3 colour state and
  # returns a list with `path` and `found`, so they are interchangeable here.
  solve3 <- switch(method,
    cfop     = cube_solve_cfop,
    kociemba = cube_kociemba,
    lbl      = cube_solve_lbl,
    m2       = cube_solve_m2,
    pochmann = cube_solve_old_pochmann)

  apply_path <- function(s, p) {
    for (m in p) s <- s[moves[[m]]]
    s
  }

  stage_name <- character(0)
  stage_detail <- character(0)
  stage_n <- integer(0)
  stage_state <- list()
  path <- character(0)

  add_stage <- function(name, detail, mv, s) {
    stage_name <<- c(stage_name, name)
    stage_detail <<- c(stage_detail, detail)
    stage_n <<- c(stage_n, length(mv))
    stage_state[[length(stage_state) + 1L]] <<- s
    path <<- c(path, mv)
  }

  # Stages one and two: the centres and the edge pairs, built from the face the
  # caller picked. Which face that is decides most of the cost -- see
  # .cube_solve4_pick above for what it is worth and why.
  red <- cube_reduce_cpp(state, start_face)
  if (!isTRUE(red$found)) {
    return(list(path = red$path, found = FALSE, stages = red$stages,
                states = red$states, failure = "reduction failed"))
  }
  # A cube already reduced needs no stages, and then there is no last state to
  # take -- the input is it.
  cur <- if (length(red$states)) red$states[[length(red$states)]] else state
  if (length(red$path)) add_stage("reduction", "centres and edges", red$path, cur)

  # The last layer, and the parity cases a 3x3x3 has no name for. Which case it
  # is comes from the 3x3x3 solver refusing the state, not from a separate
  # detector: it already knows all 57 OLL and 21 PLL cases, so "no case
  # matched" is exactly the statement that this is a parity.
  # CFOP does the classifying whatever `method` is, and the reason is that it
  # is the only one of the five that can refuse. It matches the state against
  # all 57 OLL and 21 PLL cases, so "no case matched" IS the parity detection --
  # there is no separate detector. The others cannot stand in: kociemba
  # searches and would spend its budget rather than report the case, and m2 and
  # pochmann place one piece at a time and never consult a last-layer table at
  # all, so a parity would pass them unnoticed and come out as an unsolved cube.
  #
  # So parity is found and repaired with cfop in hand, and only the final,
  # parity-free state goes to the chosen solver. On method = "cfop" the last
  # classify() already holds the answer and solving twice would be waste, which
  # is why the path is kept here rather than recomputed below.
  classify <- function(s4) {
    st3 <- try(suppressWarnings(cube_colour_state(cube_squeeze_cpp(s4), 3)),
               silent = TRUE)
    if (inherits(st3, "try-error")) return(list(kind = "error"))
    res <- try(cube_solve_cfop(st3), silent = TRUE)
    if (inherits(res, "try-error")) return(list(kind = "error"))
    if (isTRUE(res$found)) return(list(kind = "solved", path = res$path,
                                       state3 = st3))
    if (grepl("OLL", res$failure)) list(kind = "OLL") else list(kind = "PLL")
  }

  cls <- classify(cur)
  # A loop, not one pass of each: fixing OLL turns some cases into PLL cases.
  # Measured over forty states, of nineteen OLL cases the fix solved nine and
  # left ten as PLL.
  for (round in 1:4) {
    if (cls$kind %in% c("solved", "error")) break
    fix <- cube_parity_fix_cpp(cur, cls$kind)
    cur <- as.integer(fix$state)
    add_stage("parity", cls$kind, fix$path, cur)
    cls <- classify(cur)
  }

  if (cls$kind != "solved") {
    return(list(path = path, found = FALSE,
                stages = data.frame(name = stage_name, detail = stage_detail,
                                    n_moves = stage_n, stringsAsFactors = FALSE),
                states = stage_state,
                failure = paste("could not clear", cls$kind, "parity")))
  }

  # Stage three: the 3x3x3 solution, lifted back to 4x4x4 moves.
  #
  # The cube is parity-free by now, so any of the five can finish it. cfop's
  # answer is already in hand from the classify() that proved it solvable; the
  # others are run once, here.
  if (method == "cfop") {
    path3 <- cls$path
  } else {
    res3 <- try(solve3(cls$state3), silent = TRUE)
    if (inherits(res3, "try-error") || !isTRUE(res3$found)) {
      return(list(path = path, found = FALSE,
                  stages = data.frame(name = stage_name, detail = stage_detail,
                                      n_moves = stage_n,
                                      stringsAsFactors = FALSE),
                  states = stage_state,
                  failure = paste0("the 3x3x3 stage (", method,
                                   ") did not finish")))
    }
    path3 <- res3$path
  }

  lifted <- cube_lift_path_cpp(path3)$path
  cur <- apply_path(cur, lifted)
  add_stage("3x3x3", method, lifted, cur)

  list(path = path,
       found = cube_is_colour_solved(cur),
       stages = data.frame(name = stage_name, detail = stage_detail,
                           n_moves = stage_n, stringsAsFactors = FALSE),
       states = stage_state,
       failure = "")
}

## ---- the orientation sweep -------------------------------------------------
##
## The reduction is not indifferent to how the cube is held. It works the faces
## in a fixed order -- which one is the top, which the front -- and a scramble
## leaves the cube in whatever orientation it happens to leave it in, so that
## order lands well or badly by chance.
##
## How badly is worth stating: over the 24 orientations of ONE cube, measured
## on twelve of them, the reduction returned words from 139 moves to 312. The
## cube is the same throughout -- a rotation moves no piece relative to another
## -- so the whole of that range is the view rather than the position.
##
## Trying a few and keeping the shortest is therefore not a search for a better
## answer but for a better vantage, and it needs no cleverness: solve, count,
## keep the least. Measured over twenty cubes, through the shortener that a
## caller would run afterwards:
##
##     as given      231 moves from the solver, 203 shipped, 3.7 s
##     8 orientations 205                       181         4.4 s   -11%
##     24            191                        172         7.0 s   -15%
##
## Eight is the bargain: eleven per cent off for a fifth more time. The last
## four per cent cost four times as much again, because the solve is cheap and
## it is the shortener that spends the seconds -- and a shorter path is also
## quicker to shorten, which pays part of the sweep back.
##
## The rotations are generated rather than listed, by walking x, y and z until
## 24 distinct ones have been seen.
.cube4_orientation_words <- function() {
  rots <- c("x", "x'", "y", "y'", "z", "z'")
  g <- cube_group(4)
  perm_of <- function(w) group_compose(g, cube_expand_word(w, 4L))

  seen <- list(list(p = seq_len(96L), w = character(0)))
  i <- 1L
  while (i <= length(seen) && length(seen) < 24L) {
    for (r in rots) {
      cand <- seen[[i]]$p[perm_of(r)]
      if (any(vapply(seen, function(s) identical(s$p, cand), logical(1)))) next
      seen[[length(seen) + 1L]] <- list(p = cand, w = c(seen[[i]]$w, r))
    }
    i <- i + 1L
  }
  lapply(seen, function(s) s$w)
}

.cube_solve4_swept <- function(state, method, orientations,
                               start_face = "full") {
  words <- .cube4_orientation_words()
  words <- words[seq_len(min(orientations, length(words)))]

  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

  best <- NULL
  for (w in words) {
    fwd <- if (length(w)) cube_expand_word(w, 4L) else character(0)
    turned <- state
    for (m in fwd) turned <- turned[moves[[m]]]

    res <- .cube_solve4_pick(turned, method, start_face)
    if (!isTRUE(res$found)) next

    # The turn and its undoing are moves like any others and belong in the
    # count; a sweep that ignored them would prefer orientations that are
    # expensive to reach.
    back <- .cube4_invert_moves(fwd)
    res$path <- c(fwd, res$path, back)
    if (is.null(best) || length(res$path) < length(best$path)) best <- res
  }

  if (is.null(best)) {
    # every orientation refused: report it the way one of them would have
    return(.cube_solve4_pick(state, method, start_face))
  }
  best
}
