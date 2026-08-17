#' Solve a 3x3x3 by Two-Phase Search
#'
#' The fifth solver in the package and the only one that is not a human method.
#' The other four place pieces by rule or by table and stay inside what a
#' person could do by looking; this one searches, and pays for the short
#' solution in a way no cuber could.
#'
#' @section How this differs from the published algorithm:
#' Kociemba's own two-phase algorithm does not stop at the first solution. It
#' keeps searching phase 1 for other ways into the subgroup and keeps whichever
#' leaves phase 2 shortest, which is how it reaches around twenty moves; the
#' two phases are cheap enough to run many times over. This takes the first
#' entry phase 1 finds, so a cube one quarter turn from solved comes back as
#' \code{R R R} rather than \code{R}-prime. The prune tables are hash tables grown on
#' demand rather than full coordinate tables. Short rather than shortest, in
#' other words, and the demo names it \code{KociembaMod} for that reason.
#'
#' @section Where the search machinery comes from:
#' The engine underneath -- the canonical move automaton, the hash prune table
#' grown to half the depth about to be searched, the two-level cutoff that
#' abandons a whole move class when the bound is hopeless by more than one --
#' follows \code{twips} (\url{https://github.com/cubing/twips}) by Lucas Garron
#' and the cubing.js authors, MPL-2.0. The phase definitions and the piece
#' reading are this package's own, built on its existing sticker geometry.
#'
#' The algorithm is the observation that a cube is much easier to solve if you
#' first make it easy. The subgroup
#' \eqn{G_1 = \langle U, D, L^2, R^2, F^2, B^2\rangle}
#' is the set of states reachable without ever quarter-turning L,
#' R, F or B, and it is small: about 20 million states against 43 quintillion.
#' Getting into it is the first phase; finishing inside it is the second.
#'
#' What makes a state a member of \eqn{G_1} is three things, and each is
#' exactly what the excluded quarter turns would break: every edge oriented,
#' every corner oriented, and the four E-slice edges somewhere in the E slice
#' --- though which of the four sits where does not matter yet. Phase 1
#' searches for that and nothing else, on a coordinate that has thrown the rest
#' of the cube away.
#'
#' @section Why the moves are counted twice over:
#' The returned path is in the package's quarter-turn alphabet, so a half turn
#' comes back as two moves. Inside the solver it is one: phase 2 is generated
#' by \eqn{\langle U, D, L^2, R^2, F^2, B^2\rangle} and a half turn there is a
#' single step of the group, not a shorthand for two. Move cost is a property
#' of the phase rather than of the package, which is what lets one search
#' engine serve both phases.
#'
#' @section Centres:
#' Slice moves turn the centres, and a piece is identified by its colours read
#' against them --- so on a cube whose centres have moved, every cubie reads as
#' the wrong one. As in the four human methods, the cube is turned bodily back
#' first and the rotation is returned as part of the path, so the answer
#' applies to the cube as it was handed over.
#'
#' @param state Integer vector of 54 stickers, a reachable cube state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(3)}.
#' @param max_depth1,max_depth2 Deepest search each phase may go to. The
#'   defaults are past what either needs.
#' @param node_budget Nodes a phase may visit before giving up. A search that
#'   stops here returns nothing rather than a wrong answer; see
#'   \code{\link{cube_kociemba_report}} for which phase it was.
#' @return Character vector of moves, empty if no solution was found within the
#'   limits.
#' @export
#' @seealso \code{\link{cube_solve_cfop}} and \code{\link{cube_solve_lbl}} for
#'   the methods that look at the cube, \code{\link{cube_kociemba_report}} for
#'   what the last solve did
#' @examples
#' set.seed(42)
#' s <- generate_state(group = cube_group(3), n_moves = 20)
#' \donttest{
#' path <- cube_kociemba(s)
#' length(path)
#'
#' # it really solves it
#' moves <- cube_moves(3)
#' names(moves) <- cube_move_names(3)
#' for (mv in path) s <- s[moves[[mv]]]
#' identical(s, cube_identity(3))
#' }
cube_kociemba <- function(state, max_depth1 = 12L, max_depth2 = 18L,
                          node_budget = 5e7) {
  state <- as.integer(state)
  if (length(state) != 54L) {
    stop("cube_kociemba: a 3x3x3 state has 54 stickers, got ",
         length(state), call. = FALSE)
  }
  cube_kociemba_cpp(state, as.integer(max_depth1), as.integer(max_depth2),
                    as.numeric(node_budget))
}

#' What the Last Kociemba Solve Did
#'
#' "No solution" and "ran out of budget" are different facts, and a caller that
#' cannot tell them apart goes looking for a fault in the cube when the answer
#' is to raise a limit. This reports the outcome of each phase of the most
#' recent \code{\link{cube_kociemba}} call, and how many nodes it visited.
#'
#' @return List with \code{phase1}, \code{phase2} --- each one of
#'   \code{"found"}, \code{"no_solution"} or \code{"exhausted"} --- and
#'   \code{phase1_nodes}, \code{phase2_nodes}.
#' @export
#' @seealso \code{\link{cube_kociemba}}
#' @examples
#' \donttest{
#' set.seed(42)
#' s <- generate_state(group = cube_group(3), n_moves = 20)
#' cube_kociemba(s)
#' cube_kociemba_report()
#' }
cube_kociemba_report <- function() {
  cube_kociemba_last_cpp()
}

#' Build the Kociemba Prune Tables
#'
#' The tables grow themselves as the search asks for depth, so nothing needs
#' calling before \code{\link{cube_kociemba}}. This exists for the caller who
#' would rather pay that cost somewhere they expect it than inside the first
#' solve.
#'
#' @param table1,table2 Size in entries of each phase's table, rounded up to a
#'   power of two.
#' @param depth1,depth2 Depth to fill in now. Zero seeds the table with the
#'   goal alone and leaves the rest to the search.
#' @return Invisibly \code{NULL}, called for its effect.
#' @export
#' @seealso \code{\link{cube_kociemba}}
#' @examples
#' \donttest{
#' cube_kociemba_init()
#' }
cube_kociemba_init <- function(table1 = 4194304, depth1 = 0L,
                               table2 = 16777216, depth2 = 0L) {
  cube_kociemba_init_cpp(as.numeric(table1), as.integer(depth1),
                         as.numeric(table2), as.integer(depth2))
  invisible(NULL)
}

#' Reduce a 4x4x4 by Four-Phase Search
#'
#' Reduction is what an even cube has to reach before a 3x3x3 method can touch
#' it: the four centres of a face one colour, so the face acts as one, and the
#' two wings of every edge together, so the edge acts as one. This searches for
#' it, in the three phases described in \code{src/kociemba4.h}: the F/B centres,
#' then the other four faces' centres, then the wings.
#'
#' @section Parity:
#' Reduction can leave a cube in a state no 3x3x3 reaches --- the wings paired
#' and the corners solved with the permutation odd --- and the package's other
#' 4x4x4 solver, \code{\link{cube_solve4}}, meets that at the end and repairs it
#' with an inner-layer algorithm. Here it cannot arise: the parity is carried in
#' phase 3's coordinate, so a solution that would leave the cube parity-odd is
#' not a solution to the phase and the search never returns it. No repair step
#' follows, because there is nothing to repair.
#'
#' @section What this reaches today:
#' Phase 3's coordinate grows by about a factor of fifteen per level and its
#' search is the deep one. Measured over five scrambles at each length with the
#' default budget: five moves reduces every time in about a tenth of a second,
#' six moves three times in five, seven moves once in five. Past that the phase
#' spends its budget and the function returns nothing --- which
#' \code{\link{cube_kociemba4_report}} distinguishes from "no solution exists".
#' Raising \code{node_budget} buys some of this back at a cost in time. For a
#' cube from a full scramble, \code{\link{cube_solve4}} is the one that finishes;
#' it takes many more moves and always gets there.
#'
#' @param state Integer vector of 96 stickers, a 4x4x4 state --- from
#'   \code{\link{generate_state}} with \code{group = cube_group(4)}.
#' @param max_depth1,max_depth2,max_depth3 Deepest each reduction phase may
#'   search to.
#' @param node_budget Nodes a phase may visit before giving up. A phase that
#'   stops here makes the whole call return an empty path rather than a wrong
#'   one; \code{\link{cube_kociemba4_report}} says which phase it was.
#' @param progress_every Print a line every this many nodes, naming the phase,
#'   the depth being searched and the nodes so far. Zero, the default, prints
#'   nothing. A hard scramble can spend a minute inside phase 3, where silence
#'   is indistinguishable from a hang; this is for telling them apart.
#' @param prune_depth_bonus Levels to build phase 3's prune table beyond the
#'   usual half of the search depth. Zero is the standard rule. Building
#'   deeper costs time up front and sharpens the heuristic; this is the dial
#'   for measuring whether that trade pays on a given cube.
#' @param orientations Whole-cube rotations to try, as words. The reduction is
#'   run once per entry and the cube rotated back afterwards, because phases 1
#'   and 2 cannot see the wings: among their equally short routes some leave
#'   phase 3 one move from done and others leave it twenty-eight out, and
#'   nothing in those phases prefers either. Defaults to all 24.
#' @param stop_at_first Return the first orientation that reduces the cube,
#'   rather than trying them all and keeping the shortest path. \code{TRUE} by
#'   default: every orientation costs a full reduction, and one that cannot
#'   finish spends the whole \code{node_budget} before saying so, so the
#'   exhaustive form can cost twenty-four times the budget on a single cube.
#'   Set \code{FALSE} for the shortest answer the rotations can give, and
#'   expect it to take considerably longer.
#' @param budget_steps Rungs of the node budget, as fractions of
#'   \code{node_budget}, used when \code{stop_at_first} is \code{TRUE}. Every
#'   orientation is tried at the first rung before any is tried at the second,
#'   which keeps a cheap success cheap whichever rotation it needed -- an
#'   orientation only spends the full budget once all of them have failed at
#'   less. Ignored when \code{stop_at_first} is \code{FALSE}, where comparing
#'   path lengths requires every orientation to have run at the same budget.
#'   Set to \code{1} for a single rung at the full budget. See
#'   \code{R/budget_ladder.R} for the measurements.
#' @param max_orientations Try only this many of \code{orientations}. Fewer is
#'   faster in direct proportion; the risk is that a cube whose only workable
#'   rotation was cut off stops reducing at all.
#' @return Character vector of moves taking the cube to a reduced state, empty
#'   if the search did not get there within the limits.
#' @export
#' @seealso \code{\link{cube_kociemba4}} for the whole solve,
#'   \code{\link{cube_is_reduced}} for the test this aims at,
#'   \code{\link{cube_solve4}} for the solver that always finishes
#' @examples
#' set.seed(7)
#' s <- generate_state(group = cube_group(4), n_moves = 5)
#' \donttest{
#' path <- cube_kociemba4_reduce(s)
#'
#' m <- cube_moves(4)
#' names(m) <- cube_move_names(4)
#' for (mv in path) s <- s[m[[mv]]]
#' cube_is_reduced(s)
#' }
cube_kociemba4_reduce <- function(state, max_depth1 = 10L, max_depth2 = 12L,
                                  max_depth3 = 14L, node_budget = 5e7,
                                  progress_every = 0, prune_depth_bonus = 0L,
                                  orientations = .cube4_orientations,
                                  stop_at_first = TRUE,
                                  budget_steps = c(0.1, 1),
                                  max_orientations = length(orientations)) {
  state <- as.integer(state)
  if (length(state) != 96L) {
    stop("cube_kociemba4_reduce: a 4x4x4 state has 96 stickers, got ",
         length(state), call. = FALSE)
  }

  one <- function(rot, budget = node_budget) {
    if (!nzchar(rot)) {
      return(cube_kociemba4_reduce_cpp(state, as.integer(max_depth1),
                                       as.integer(max_depth2),
                                       as.integer(max_depth3),
                                       as.numeric(budget),
                                       as.numeric(progress_every),
                                       as.integer(prune_depth_bonus)))
    }
    # Turn the cube, reduce, and turn it back. A whole-cube rotation moves no
    # piece relative to another, so the reduction it finds is a reduction of
    # the cube that was handed in -- with the rotation and its inverse carried
    # in the path so the answer applies to the cube as it stands.
    # The rotation is expanded into the package's own moves once, and undone by
    # inverting that expansion rather than by inverting the notation. One
    # source of truth: whatever cube_expand_word() means by "x y", its reverse
    # with each move inverted undoes exactly that, and no second parser can
    # disagree with the first.
    fwd <- cube_expand_word(rot, 4L)
    back <- .cube4_invert_moves(fwd)
    turned <- state[cube_wide_word(rot, 4L)]
    red <- cube_kociemba4_reduce_cpp(turned, as.integer(max_depth1),
                                     as.integer(max_depth2),
                                     as.integer(max_depth3),
                                     as.numeric(budget),
                                     as.numeric(progress_every),
                                     as.integer(prune_depth_bonus))
    if (!length(red) && !cube_is_reduced(turned)) return(character(0))
    c(fwd, red, back)
  }

  # How many of the list to try at all. Trying fewer is the cheapest lever
  # there is: every orientation costs a full reduction, and an orientation that
  # does not finish costs the whole node_budget before it says so.
  if (max_orientations < length(orientations)) {
    orientations <- orientations[seq_len(max(1L, max_orientations))]
  }

  if (length(orientations) <= 1L) return(one(if (length(orientations)) orientations[[1]] else ""))

  # With stop_at_first, the orientations are swept at a small budget before any
  # is tried at a large one -- see R/budget_ladder.R for why, and for the
  # measurements. It matters more here than it does in cube_solve4_cascade():
  # this list is twenty-four orientations rather than four, so a cube that
  # fails them all at 5e7 nodes pays about seventeen minutes at the rate that
  # measurement was taken at.
  if (stop_at_first) {
    hit <- .budget_sweep(
      as.list(orientations),
      .budget_rungs(node_budget, budget_steps),
      function(rot, budget) {
        p <- one(rot, budget)
        # An empty path from a cube that is already reduced is a success; from
        # one that is not, it is the search saying it got nowhere.
        if (!length(p) && !cube_is_reduced(state)) return(list(ok = FALSE))
        list(ok = TRUE, value = p)
      })
    return(if (is.null(hit)) character(0) else hit)
  }

  # Without it, the loop keeps the shortest of all the orientations, and a
  # ladder cannot help: every orientation has to run at the full budget for the
  # comparison to mean anything, so the cost this mode pays is the cost of what
  # it is asking for. Measured, the progress counter climbs to fifty million
  # and resets seven times over on one cube.
  #
  # What stop_at_first costs by comparison is length, not correctness: any
  # orientation that reduces the cube reduces it, and the rotations are carried
  # in the path. How much length it costs is what bench_orientation_cost.R
  # measures.
  best <- NULL
  for (rot in orientations) {
    p <- one(rot)
    if (!length(p) && !cube_is_reduced(state)) next
    if (is.null(best) || length(p) < length(best)) best <- p
    # Nothing beats an empty path, so stop rather than turn the cube 23 more
    # times to confirm it.
    if (!length(best)) break
  }
  if (is.null(best)) character(0) else best
}

# Undo a word: reverse the order and invert each move. The alphabet is quarter
# turns named with an optional prime, so inverting one is adding or removing
# that prime and nothing else.
.cube4_invert_moves <- function(moves) {
  if (!length(moves)) return(character(0))
  rev(ifelse(grepl("'$", moves), sub("'$", "", moves), paste0(moves, "'")))
}

# The 24 orientations of the cube, as words in the rotation moves.
#
# Which one the cube is handed over in is not a detail. Phases 1 and 2 search
# for centres and centres-by-axis and their coordinates do not see the wings at
# all, so among the many shortest routes to their goal, some leave the wings
# where phase 3 can finish in one move and others leave them twenty-eight moves
# out -- and nothing in those phases prefers the first kind. They return
# whichever the move ordering reached first.
#
# Measured on one cube, six moves from solved (bench_handover.R), reducing it
# in each orientation and asking what phase 3 then had to do:
#
#     x x, z z, z'   phase 3 finished in 0 moves
#     y y           16
#     y', x y'      18
#     (no rotation) 23   <- what the solver used to take
#     z             28
#     x, x'         phase 3 could not finish at all
#
# Phase 3's own goal set already contains all 24 rotations of the solved cube
# (kociemba4.h), so rotating does not change the question it is asked -- only
# which shortest answer phases 1 and 2 happen to give it.
.cube4_orientations <- c(
  "", "x", "x x", "x'", "y", "y y", "y'", "z", "z z", "z'",
  "x y", "x y y", "x y'", "x' y", "x' y'", "y x", "y x'",
  "y y x", "z y", "z y'", "z' y", "x z", "y z", "y' z")

#' Solve a 4x4x4 by Four-Phase Search
#'
#' The three reduction phases of \code{\link{cube_kociemba4_reduce}}, and then
#' the cube --- now a 3x3x3 with fat pieces --- handed to
#' \code{\link{cube_kociemba}}. That is the fourth phase, and it is the only one
#' that lives on this side of the C++ boundary: the reduced cube is squeezed
#' from 96 stickers to 54, solved, and the answer lifted back to 4x4x4 moves.
#'
#' The same limits apply as to the reduction --- see
#' \emph{What this reaches today} in \code{\link{cube_kociemba4_reduce}}. Where
#' this returns a path it is a short one, a few dozen moves against the several
#' hundred \code{\link{cube_solve4}} takes; where it returns nothing,
#' \code{\link{cube_solve4}} is what to call.
#'
#' @param state Integer vector of 96 stickers, a 4x4x4 state.
#' @param max_depth1,max_depth2,max_depth3 Deepest each reduction phase may
#'   search to.
#' @param node_budget Nodes each phase may visit before giving up.
#' @param progress_every Print a line every this many nodes, naming the phase,
#'   the depth being searched and the count so far; 0 for silence. Phase 3 can
#'   spend tens of millions of nodes on a hard cube, which is minutes of no
#'   output at all, and a caller cannot tell that from a hang without this.
#' @param stop_at_first,budget_steps,max_orientations Passed to
#'   \code{\link{cube_kociemba4_reduce}}: whether to accept the first rotation
#'   that reduces the cube rather than the shortest of all of them, the rungs
#'   of the node budget the rotations are swept at, and how
#'   many rotations to try. The defaults favour time over length, because the
#'   exhaustive form pays the full node budget once per rotation.
#' @return List with components:
#'   \item{path}{Character vector of moves, the whole solution}
#'   \item{found}{Logical, whether the cube ended solved}
#'   \item{reduction}{Character vector, the part of the path that reduced it}
#'   \item{cube3}{Character vector, the part that solved the reduced cube}
#'   \item{failure}{Empty string, or which step did not finish}
#' @export
#' @seealso \code{\link{cube_kociemba4_reduce}},
#'   \code{\link{cube_kociemba4_report}}, \code{\link{cube_solve4}}
#' @examples
#' set.seed(7)
#' s <- generate_state(group = cube_group(4), n_moves = 5)
#' \donttest{
#' res <- cube_kociemba4(s)
#' res$found
#' length(res$path)
#' }
cube_kociemba4 <- function(state, max_depth1 = 10L, max_depth2 = 12L,
                           max_depth3 = 14L, node_budget = 5e7,
                           progress_every = 0, stop_at_first = TRUE,
                           budget_steps = c(0.1, 1),
                           max_orientations = length(.cube4_orientations)) {
  state <- as.integer(state)
  if (length(state) != 96L) {
    stop("cube_kociemba4: a 4x4x4 state has 96 stickers, got ",
         length(state), call. = FALSE)
  }

  empty <- function(failure) {
    list(path = character(0), found = FALSE, reduction = character(0),
         cube3 = character(0), failure = failure)
  }

  red <- cube_kociemba4_reduce(state, max_depth1, max_depth2, max_depth3,
                               node_budget, progress_every,
                               stop_at_first = stop_at_first,
                               budget_steps = budget_steps,
                               max_orientations = max_orientations)
  # An already-reduced cube gives an empty reduction, which is a success and
  # not a failure; the two are told apart by the state, not by the path length.
  if (!length(red) && !cube_is_reduced(state)) {
    return(empty("reduction did not finish"))
  }

  moves <- cube_moves(4)
  names(moves) <- cube_move_names(4)
  cur <- state
  for (mv in red) cur <- cur[moves[[mv]]]

  # Squeezed to 54 stickers the cube is a 3x3x3, and the two-phase solver
  # finishes it. No parity can arrive here -- phase 3 could not have produced
  # one -- so unlike cube_solve4 there is no repair step between.
  st3 <- cube_colour_state(cube_squeeze_cpp(cur), 3)
  p3 <- cube_kociemba(st3)
  if (!length(p3) && !identical(st3, cube_identity(3))) {
    return(list(path = red, found = FALSE, reduction = red,
                cube3 = character(0),
                failure = "3x3x3 phase did not finish"))
  }

  lifted <- cube_lift_path_cpp(p3)$path
  for (mv in lifted) cur <- cur[moves[[mv]]]

  list(path = c(red, lifted), found = cube_is_colour_solved(cur),
       reduction = red, cube3 = lifted, failure = "")
}

#' What the Last 4x4x4 Reduction Did
#'
#' As \code{\link{cube_kociemba_report}} is to the 3x3x3, so this is to the
#' three reduction phases: which of them finished, which ran out, and what each
#' cost. A phase that reports \code{"exhausted"} is one to raise
#' \code{node_budget} for; \code{"no_solution"} within the depth given is a
#' different fact and wants \code{max_depth} raised instead.
#'
#' Phase 4 does not appear here. It is \code{\link{cube_kociemba}} on the
#' squeezed cube, and \code{\link{cube_kociemba_report}} is what reports it.
#'
#' @return List with \code{phase1}, \code{phase2}, \code{phase3} --- each one of
#'   \code{"found"}, \code{"no_solution"} or \code{"exhausted"} --- and
#'   \code{phase1_nodes}, \code{phase2_nodes}, \code{phase3_nodes}.
#' @export
#' @seealso \code{\link{cube_kociemba4_reduce}}, \code{\link{cube_kociemba4}}
#' @examples
#' \donttest{
#' set.seed(7)
#' s <- generate_state(group = cube_group(4), n_moves = 5)
#' cube_kociemba4_reduce(s)
#' cube_kociemba4_report()
#' }
cube_kociemba4_report <- function() {
  cube_kociemba4_last_cpp()
}
