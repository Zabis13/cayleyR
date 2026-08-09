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
cube_solve4 <- function(state) {
  state <- as.integer(state)
  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)

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
  classify <- function(s4) {
    st3 <- try(suppressWarnings(cube_colour_state(cube_squeeze_cpp(s4), 3)),
               silent = TRUE)
    if (inherits(st3, "try-error")) return(list(kind = "error"))
    res <- try(cube_solve_cfop(st3), silent = TRUE)
    if (inherits(res, "try-error")) return(list(kind = "error"))
    if (isTRUE(res$found)) return(list(kind = "solved", path = res$path))
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
  lifted <- cube_lift_path_cpp(cls$path)$path
  cur <- apply_path(cur, lifted)
  add_stage("3x3x3", "cfop", lifted, cur)

  list(path = path,
       found = cube_is_colour_solved(cur),
       stages = data.frame(name = stage_name, detail = stage_detail,
                           n_moves = stage_n, stringsAsFactors = FALSE),
       states = stage_state,
       failure = "")
}
