#' Full Breadth-First Search Over the Cayley Graph
#'
#' Explores every state reachable from `start_state` by repeatedly applying the
#' allowed operations, recording the graph distance of each state from the
#' start. Unlike \code{\link{sparse_bfs}}, no pruning is applied: the whole
#' reachable component is enumerated.
#'
#' Celestial coordinates are a property of a path rather than of a state, since
#' the same state can be reached by many different operation sequences. The
#' `nL`, `nR`, `nX` counters reported here are those of the shortest path BFS
#' happened to find first, and `theta`, `phi`, `omega` are derived from them via
#' \code{\link{convert_LRX_to_celestial}}.
#'
#' @param start_state Integer vector, the state to explore from
#' @param k Integer, parameter for the reverse-prefix operation
#' @param moves Character vector of allowed operations, e.g. c("L", "R", "X")
#'   or c("1", "2", "3") (default: all three)
#' @return Data frame with one row per reachable state:
#'   \item{state_str}{State as an underscore-separated key}
#'   \item{dist}{Graph distance from `start_state`}
#'   \item{nL, nR, nX}{Operation counts along the BFS shortest path}
#'   \item{theta, phi, omega}{Celestial coordinates derived from those counts}
#' @seealso \code{\link{cayley_graph_diameter}}, \code{\link{sparse_bfs}}
#' @export
#' @examples
#' d <- cayley_bfs_full(1:5, k = 3)
#' nrow(d)
#' table(d$dist)
cayley_bfs_full <- function(start_state, k, moves = c("L", "R", "X")) {
  cayley_bfs_full_cpp(
    as.integer(start_state),
    as.integer(k),
    as.character(moves)
  )
}

#' Cayley Graph Diameter and Maximally Distant State Pairs
#'
#' Computes the diameter of the Cayley graph component reachable from
#' `start_state`, together with the pairs of states realising it and the
#' eccentricity of each vertex.
#'
#' Two methods are available. `"all_pairs"` runs a BFS from every vertex and
#' yields the true diameter and every diametral pair; its cost grows as the
#' number of vertices times the cost of one BFS, which in practice limits it to
#' permutations of roughly size 8 or below. `"from_start"` runs a single BFS and
#' reports the eccentricity of `start_state` and the pairs `(start_state, v)`
#' realising it; this equals the diameter only when the graph is
#' vertex-transitive, but it scales to much larger graphs.
#'
#' @param start_state Integer vector, the state to explore from
#' @param k Integer, parameter for the reverse-prefix operation
#' @param moves Character vector of allowed operations (default: c("L","R","X"))
#' @param method Either "all_pairs" (default, exact) or "from_start" (single
#'   BFS, exact only for vertex-transitive graphs)
#' @param max_pairs Numeric, maximum number of pairs to materialise in
#'   `pairs_df` (default `Inf`). `n_pairs` always reports the honest total.
#' @param verbose Logical; if TRUE, prints progress during the sweep
#' @return List containing:
#'   \item{diameter}{Integer, the graph diameter (or start eccentricity)}
#'   \item{n_vertices}{Number of reachable states}
#'   \item{n_pairs}{Total number of maximally distant pairs found}
#'   \item{truncated}{Logical, TRUE if `pairs_df` was capped by `max_pairs`}
#'   \item{pairs_df}{Data frame of maximally distant pairs, with the celestial
#'     coordinates of both endpoints (`from_*` and `to_*` columns)}
#'   \item{ecc}{Data frame of per-vertex eccentricities (all `NA` except the
#'     start vertex when `method = "from_start"`)}
#'   \item{bfs}{The full BFS data frame from `start_state`, as returned by
#'     \code{\link{cayley_bfs_full}}}
#'   \item{dist_hist}{Data frame of distance-from-start counts}
#'   \item{method}{The method actually used}
#' @seealso \code{\link{cayley_bfs_full}}
#' @export
#' @examples
#' res <- cayley_graph_diameter(1:5, k = 3)
#' res$diameter
#' head(res$pairs_df)
cayley_graph_diameter <- function(start_state, k,
                                  moves = c("L", "R", "X"),
                                  method = c("all_pairs", "from_start"),
                                  max_pairs = Inf,
                                  verbose = FALSE) {
  method <- match.arg(method)

  raw <- cayley_graph_diameter_cpp(
    as.integer(start_state),
    as.integer(k),
    as.character(moves),
    if (method == "from_start") 1L else 0L,
    as.numeric(max_pairs),
    isTRUE(verbose)
  )

  bfs <- raw$bfs
  coord_cols <- c("state_str", "nL", "nR", "nX", "theta", "phi", "omega")

  # C++ returns 0-based vertex ids indexing into the BFS frame; widen them into
  # the coordinates of both endpoints so pairs_df stands on its own.
  i_from <- raw$pair_from + 1L
  i_to <- raw$pair_to + 1L

  from_part <- bfs[i_from, coord_cols, drop = FALSE]
  to_part <- bfs[i_to, coord_cols, drop = FALSE]
  names(from_part) <- paste0("from_", names(from_part))
  names(to_part) <- paste0("to_", names(to_part))

  pairs_df <- cbind(
    from_part,
    to_part,
    data.frame(dist = raw$pair_dist),
    stringsAsFactors = FALSE
  )
  rownames(pairs_df) <- NULL

  ecc <- data.frame(
    state_str = bfs$state_str,
    ecc = raw$ecc,
    stringsAsFactors = FALSE
  )

  list(
    diameter = raw$diameter,
    n_vertices = raw$n_vertices,
    n_pairs = raw$n_pairs,
    truncated = raw$truncated,
    pairs_df = pairs_df,
    ecc = ecc,
    bfs = bfs,
    dist_hist = data.frame(
      dist = raw$hist_dist,
      count = raw$hist_count
    ),
    method = method
  )
}
