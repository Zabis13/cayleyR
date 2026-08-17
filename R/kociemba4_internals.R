# Documentation and exports for the 4x4x4 phase functions.
#
# The functions themselves are generated into RcppExports.R by
# Rcpp::compileAttributes(), which rewrites that file wholesale -- so their
# roxygen blocks cannot live there. They live here instead, attached to the
# generated names with @rdname and @name, which is what lets roxygen2 export
# and document a function it did not see defined.
#
# These are the phases as the solver runs them, one call each, with the
# solver's own state carried in a singleton between calls. They were internal
# while the reduction was being measured and are exported now that
# cube_solve4_cascade() and the diagnostics in inst/examples depend on them
# from outside the package.

#' The 4x4x4 reduction phases, one call at a time
#'
#' The reduction that turns a 4x4x4 into a 3x3x3 runs in three phases, and
#' these expose them individually. \code{cube_kociemba4_reduce} runs all three
#' and reports only the result; these let a caller stop after any one of them,
#' inspect what it produced, and decide what to do next -- which is what
#' \code{\link{cube_solve4_cascade}} does, and what every diagnostic in
#' \code{inst/examples} was written against.
#'
#' @param state Integer vector of 96 stickers.
#' @param upto_phase Run phase 1 only (\code{1}) or phases 1 and 2 (\code{2}).
#' @param max_depth1,max_depth2,max_depth3 Deepest search each phase may reach.
#' @param node_budget Nodes a phase may visit before giving up. The outcome
#'   \code{"exhausted"} means this ran out, which is a statement about the
#'   budget and not about the cube: no path was found and none was ruled out.
#' @param prune_depth_bonus Extra levels to fill the prune table beyond the
#'   default of half the search depth. Measured: each level costs about
#'   twenty-four times the last in fill time and does not move the bound off
#'   zero, because the entries collide before the depth matters.
#' @param use_exact_centres Use the exact centre table (\code{src/centre_table.h})
#'   alongside the hash table, taking the larger of the two bounds. The centre
#'   table is complete -- 352,800 arrangements, every one with its own entry --
#'   so it cannot return the false zeroes the hash table returns when another
#'   state claims the slot.
#' @param phase Which phase's goal to test, 1, 2 or 3.
#'
#' @return
#' \code{cube_kociemba4_phase12_cpp} returns the moves as a character vector.
#'
#' \code{cube_kociemba4_phase3_cpp} returns a list: \code{found},
#' \code{path}, \code{generators} (the phase's own move words, which expand to
#' \code{path}), \code{states_perm} and \code{states_ori} (the states the
#' search passed through, one row per generator), \code{outcome},
#' \code{best_bound}, \code{nodes} and the prune counters. On failure the path
#' and states describe the branch the table rated closest, which is where the
#' effort went rather than progress towards a solution.
#'
#' \code{cube_at_phase_goal_cpp} returns a single logical.
#'
#' \code{cube_phase3_coord_cpp} returns phase 3's coordinate broken into parts,
#' including \code{prune_bound} -- which is only meaningful once the table has
#' been built, so a fresh process reports it as 0 or 1 for everything.
#'
#' \code{cube_to_pieces4_cpp} returns \code{perm} and \code{ori}: the 56-piece
#' vector, with corners at 1:8, wings at 9:32 and centres at 33:56. Values
#' carry the same offsets, so a wing's number runs 8..31 rather than 0..23.
#'
#' \code{cube_wing_geometry_cpp} returns the wing geometry: \code{partner},
#' \code{dedge}, \code{primary}, \code{primary_in_dedge} and
#' \code{speffz_to_slot}, all measured from the cube rather than tabulated.
#'
#' \code{cube_kociemba4_tables_cpp} returns one entry per phase describing its
#' prune table: \code{size}, \code{filled}, \code{built_depth},
#' \code{n_collisions}, \code{waste_ratio} and the per-depth counts.
#'
#' @seealso \code{\link{cube_kociemba4_reduce}} for all three phases at once,
#'   \code{\link{cube_solve4_cascade}} for the bounded version that falls back.
#'
#' @name kociemba4_phases
NULL

#' @rdname kociemba4_phases
#' @name cube_kociemba4_phase12_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_kociemba4_phase3_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_at_phase_goal_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_phase3_coord_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_to_pieces4_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_wing_geometry_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_kociemba4_tables_cpp
#' @export
NULL

#' @rdname kociemba4_phases
#' @name cube_kociemba4_fill_phase3_cpp
#' @export
NULL

#' Reduce a 4x4x4 by the human method
#'
#' Builds the centres, then pairs the wings with the algorithms speedcubers
#' use -- the setup-and-pair method, including the last-pair case and the OLL
#' parity. The algorithms are listed in edge_algs() in src/cube_edges.h;
#' nothing here searches for a move, it chooses among measured ones.
#'
#' What it does not have is a fixed order. Each round takes whichever
#' (setup, algorithm) pair leaves the most wings paired, which is where a
#' greedy method can reach a position none of its algorithms improve. The
#' script inst/examples/diag_pair_edges_stall.R measures how often that
#' happens and what shape the position has.
#'
#' @param state Integer vector of 96 stickers.
#' @return A list with \code{found}, \code{path}, \code{stages},
#'   \code{states} (the cube after each stage) and \code{failure} -- the reason
#'   it stopped, empty when it did not.
#' @seealso \code{\link{cube_solve4}}, which runs this and then finishes the
#'   3x3x3 it leaves behind.
#' @name cube_reduce_cpp
#' @export
NULL
