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
                                          "pochmann")) {
  method <- match.arg(method)
  state <- as.integer(state)
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

  # Stages one and two: the centres and the edge pairs.
  red <- cube_reduce_cpp(state)
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
