#' Batch Weighted A* With a Learned Heuristic
#'
#' The search half of DeepCubeA. \code{\link{cube_adi_train}} learns a distance
#' to solved; this walks the graph using that distance as its heuristic, and it
#' is what turns an estimator into a solver.
#'
#' @section Why not simply walk downhill:
#' \code{\link{cube_adi_solve}} takes the best-looking child at every step and
#' never reconsiders. That is enough near the goal, where the network is exact,
#' and fails past it: a state whose true distance the network underrates draws
#' the walk in, every child then looks worse than the parent, and the walk is
#' stuck with no way back. A search keeps the alternatives it passed over on a
#' list and returns to them when the branch it chose stops paying, so a wrong
#' estimate costs time rather than the solve.
#'
#' @section The weight:
#' Nodes are ordered by \eqn{f = \lambda g + h} --- \eqn{g} moves spent to
#' reach the node, \eqn{h} the network's guess at what remains. \eqn{\lambda =
#' 1} is textbook A*, which returns the shortest path the heuristic admits but
#' expands far more nodes; \eqn{\lambda = 0} ignores the cost so far and is
#' pure greedy best-first, fast and prone to wandering. DeepCubeA runs around
#' 0.6, buying much shorter runtimes for slightly longer solutions, and that is
#' the default here. Note that the learned heuristic is not admissible in any
#' case, so even \eqn{\lambda = 1} carries no optimality guarantee --- the
#' weight trades length against time, it does not switch a guarantee on.
#'
#' @section Why a batch:
#' The expensive part of an iteration is not the queue, it is the forward pass.
#' Scoring one node's children is a batch of \code{n_moves}, which leaves a GPU
#' idle; so each iteration pops the \code{batch} best nodes at once, expands all
#' of them, and scores every child in a single call. The cost of an iteration is
#' then nearly flat in \code{batch}, and a wider batch means fewer of them. It
#' also loosens the ordering --- the 1000th-best node is expanded alongside the
#' best --- which is the same trade the weight makes, and the reason a very wide
#' batch can lengthen solutions.
#'
#' @param net A trained \code{cube_adi_net} from \code{\link{cube_adi_train}}
#' @param state Integer vector, the scrambled state
#' @param weight Weight \eqn{\lambda} on the cost so far. 0 is greedy
#'   best-first, 1 is A*.
#' @param batch Nodes expanded per iteration
#' @param max_nodes Give up after this many nodes have been expanded
#' @param batch_size Batch size the network was compiled at; children are padded
#'   out to a multiple of it, as in \code{\link{cube_adi_solve}}
#' @param verbose \code{TRUE} to report each iteration
#' @return List with \code{solved}, \code{path} (move names), \code{length},
#'   \code{nodes} (expanded) and \code{iterations}
#' @seealso \code{\link{cube_adi_solve}} for the greedy walk
#' @export
cube_adi_astar <- function(net, state, weight = 0.6, batch = 1000L,
                           max_nodes = 1e6, batch_size = 256L,
                           verbose = FALSE) {
  adi_require_ggml()
  if (!inherits(net, "cube_adi_net")) stop("net must be a cube_adi_net")
  g       <- net$group
  n_moves <- net$n_moves
  cur     <- as.integer(state)
  if (length(cur) != net$state_len)
    stop("state has length ", length(cur), " but the group works on length ",
         net$state_len)

  if (all(cur == seq_along(cur)))
    return(list(solved = TRUE, path = character(0), length = 0L,
                nodes = 0L, iterations = 0L))

  ## The open list is three parallel vectors plus one matrix of states, grown
  ## by doubling. A data.frame would be tidier and would copy the whole thing
  ## on every append.
  cap    <- max(4096L, as.integer(batch) * n_moves * 2L)
  states <- matrix(0L, cap, net$state_len)
  gcost  <- numeric(cap)       # moves spent reaching the node
  fcost  <- numeric(cap)       # weight * g + h, the ordering key
  parent <- integer(cap)       # index of the node this came from, 0 for root
  pmove  <- integer(cap)       # move taken from that parent, 1-based
  open   <- logical(cap)       # still on the list, i.e. not yet expanded

  states[1L, ] <- cur
  gcost[1L] <- 0
  fcost[1L] <- 0
  parent[1L] <- 0L
  pmove[1L] <- 0L
  open[1L] <- TRUE
  n_used <- 1L

  ## Closed list: key -> row index, so a node reached again by a shorter path
  ## can be found and reopened rather than merely rejected.
  seen <- new.env(hash = TRUE, parent = emptyenv(), size = 1e5L)
  assign(as.character(cube_adi_keys(matrix(cur, nrow = 1L))), 1L, envir = seen)

  grow <- function(extra) {
    if (n_used + extra <= cap) return(invisible(NULL))
    new_cap <- cap
    while (n_used + extra > new_cap) new_cap <- new_cap * 2L
    states <<- rbind(states, matrix(0L, new_cap - cap, net$state_len))
    gcost  <<- c(gcost,  numeric(new_cap - cap))
    fcost  <<- c(fcost,  numeric(new_cap - cap))
    parent <<- c(parent, integer(new_cap - cap))
    pmove  <<- c(pmove,  integer(new_cap - cap))
    open   <<- c(open,   logical(new_cap - cap))
    cap    <<- new_cap
    invisible(NULL)
  }

  ## Walk the parent chain back from a node to the root.
  trace_path <- function(node) {
    moves <- integer(0)
    while (node > 1L || parent[node] != 0L) {
      moves <- c(pmove[node], moves)
      node  <- parent[node]
      if (node == 0L) break
    }
    g$moves[moves]
  }

  expanded <- 0L
  it <- 0L

  while (expanded < max_nodes) {
    it <- it + 1L
    live <- which(open[seq_len(n_used)])
    if (!length(live)) break

    ## Pop the `batch` best. Only the head of the order matters, but `partial`
    ## is not worth the care it needs -- its indices have to sit inside the
    ## vector being ordered, which is a condition on data the caller controls.
    take <- if (length(live) <= batch) live else
      live[order(fcost[live])[seq_len(batch)]]
    open[take] <- FALSE
    expanded <- expanded + length(take)

    ch <- cube_adi_children(g$ptr, states[take, , drop = FALSE])
    kids <- ch$children
    n_kid <- nrow(kids)

    ## A solved child ends it, and the check comes before the network is asked:
    ## the answer is known, and the forward pass over the rest is wasted work.
    hit <- which(ch$solved)
    if (length(hit)) {
      k  <- hit[1L]
      ## Row k of the children is child (k-1) %% n_moves + 1 of parent
      ## take[(k-1) %/% n_moves + 1] -- the state-major layout cube_adi_children
      ## documents.
      pi_ <- take[(k - 1L) %/% n_moves + 1L]
      mv  <- (k - 1L) %% n_moves + 1L
      path <- c(trace_path(pi_), g$moves[mv])
      if (verbose)
        cat(sprintf("iter %d | solved in %d moves | %d nodes expanded\n",
                    it, length(path), expanded))
      return(list(solved = TRUE, path = path, length = length(path),
                  nodes = expanded, iterations = it))
    }

    kid_g <- rep(gcost[take], each = n_moves) + 1
    kid_h <- adi_value_of(net$value, kids, batch_size, net$arch, net$layout)
    kid_f <- weight * kid_g + kid_h
    keys  <- as.character(cube_adi_keys(kids))

    ## Within one batch the same state can turn up as a child of several
    ## parents. Keep the cheapest copy of each before touching the closed list,
    ## or the later duplicates reopen what the earlier ones just wrote.
    best_in_batch <- !duplicated(keys[order(kid_g)])[order(order(kid_g))]
    cand <- which(best_in_batch)

    add_ix <- integer(0)
    for (k in cand) {
      key <- keys[k]
      old <- seen[[key]]
      if (is.null(old)) {
        add_ix <- c(add_ix, k)
      } else if (kid_g[k] < gcost[old]) {
        ## Reached again by a shorter route: rewrite the node in place and put
        ## it back on the list so its own children are re-derived from the
        ## cheaper cost.
        gcost[old]  <- kid_g[k]
        fcost[old]  <- kid_f[k]
        parent[old] <- take[(k - 1L) %/% n_moves + 1L]
        pmove[old]  <- (k - 1L) %% n_moves + 1L
        open[old]   <- TRUE
      }
    }

    if (length(add_ix)) {
      grow(length(add_ix))
      slots <- n_used + seq_along(add_ix)
      states[slots, ] <- kids[add_ix, , drop = FALSE]
      gcost[slots]  <- kid_g[add_ix]
      fcost[slots]  <- kid_f[add_ix]
      parent[slots] <- take[(add_ix - 1L) %/% n_moves + 1L]
      pmove[slots]  <- (add_ix - 1L) %% n_moves + 1L
      open[slots]   <- TRUE
      for (i in seq_along(add_ix)) assign(keys[add_ix[i]], slots[i], envir = seen)
      n_used <- n_used + length(add_ix)
    }

    if (verbose)
      cat(sprintf("iter %4d | expanded %7d | open %7d | best f %.2f h %.2f\n",
                  it, expanded, sum(open[seq_len(n_used)]),
                  min(fcost[which(open[seq_len(n_used)])]), min(kid_h)))
  }

  list(solved = FALSE, path = character(0), length = NA_integer_,
       nodes = expanded, iterations = it)
}
