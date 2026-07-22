#' Length of the Sorted Run on the Ring
#'
#' Returns the length of the run \code{1, 2, ..., r} currently sitting
#' consecutively on the ring, starting wherever value 1 happens to be. This is
#' the quantity phase 1 of \code{\link{human_algorithm}} maximises: each
#' insertion move appends one value to the run.
#'
#' Because the ring is cyclic, the position of value 1 is irrelevant -- only
#' the consecutive run following it is counted. A fully sorted state gives
#' \code{n}; a state where 2 does not follow 1 gives 1.
#'
#' @param state Integer vector, a permutation state
#' @return Integer, length of the sorted run (0 if value 1 is absent)
#' @export
#' @seealso \code{\link{human_phase1_rank}}, \code{\link{human_algorithm}}
#' @examples
#' run_length(1:20)                  # 20, fully sorted
#' run_length(c(1:5, 20:16, 6:15))   # 5
run_length <- function(state) {
  run_length_cpp(as.integer(state))
}

#' Rank Candidate Moves by the Phase 1 Criterion
#'
#' Exposes phase 1 of \code{\link{human_algorithm}} as a navigator: instead of
#' committing to a move, it reports the moves phase 1 would consider from the
#' given state, each scored. A search can use this to choose its own direction
#' while still being guided by the human method.
#'
#' The candidates are \emph{composite} moves -- "rotate the ring so the flipper
#' covers a chosen window, then flip" -- which is the unit phase 1 actually
#' works in. Ranking the three raw operations instead would give no signal: a
#' single rotation changes neither the run nor the gap. One candidate is
#' produced per window offset, plus the placing move itself when the gap
#' already equals \code{k}. Windows that would overlap the finished run are
#' dropped, exactly as in phase 1.
#'
#' With \code{sorted = TRUE} the rows come back in phase 1's own order of
#' preference: \code{run} descending, then \code{gap_cost} ascending, then the
#' shorter word.
#'
#' An empty data frame is returned once the run has grown far enough that only
#' the tail is left: the insertion move no longer fits there, and the tail
#' needs the 3-cycles of phase 2 instead.
#'
#' @param state Integer vector, a permutation state
#' @param k Integer, length of the reverse-prefix (flipper) operation
#' @param sorted Logical, return rows in phase 1 preference order (default TRUE)
#' @return A data.frame with one row per candidate move:
#'   \item{ops}{Character, the operation word, comma-separated ("1"/"2"/"3")}
#'   \item{len}{Integer, number of operations in the word}
#'   \item{run}{Integer, \code{\link{run_length}} after the move; higher is better}
#'   \item{gap_cost}{Integer, \code{|gap(m-1, m) - k|} afterwards; lower is better}
#'   \item{places}{Logical, whether the move actually appends a value to the run}
#' @export
#' @seealso \code{\link{run_length}}, \code{\link{human_algorithm}}
#' @examples
#' set.seed(1)
#' s <- generate_state(20, k = 4, n_moves = 50)
#' human_phase1_rank(s, k = 4)
human_phase1_rank <- function(state, k = 4L, sorted = TRUE) {
  cand <- human_phase1_rank_cpp(as.integer(state), as.integer(k))
  if (sorted && nrow(cand) > 0) {
    cand <- cand[order(-cand$run, cand$gap_cost, cand$len), , drop = FALSE]
    rownames(cand) <- NULL
  }
  cand
}

#' Follow the Phase 1 Navigator to the Tail
#'
#' Repeatedly applies the move \code{\link{human_phase1_rank}} prefers, growing
#' the sorted run until phase 1 runs out of applicable moves -- that is, until
#' only the tail is left. The tail itself is not touched; finishing it needs
#' either the 3-cycles of \code{\link{human_algorithm}} or a search such as
#' \code{\link{find_path_iterative}}.
#'
#' The walk is greedy: at each step the single best-ranked candidate is taken.
#'
#' @param state Integer vector, the state to navigate from
#' @param k Integer, length of the reverse-prefix (flipper) operation
#' @param max_steps Integer, safety cap on navigator steps (default 2000)
#' @param trace Logical, collect a per-step data.frame (default FALSE)
#' @return List with components:
#'   \item{state}{Integer vector, the state reached}
#'   \item{path}{Character vector of operations applied}
#'   \item{run}{Integer, sorted run length reached}
#'   \item{trace}{data.frame of per-step progress, or NULL}
#' @export
#' @seealso \code{\link{human_phase1_rank}}, \code{\link{human_algorithm}}
#' @examples
#' set.seed(42)
#' s <- generate_state(20, k = 4, n_moves = 200)
#' nav <- human_phase1_navigate(s, k = 4)
#' nav$run
human_phase1_navigate <- function(state, k = 4L, max_steps = 2000L,
                                  trace = FALSE) {
  st <- as.integer(state)
  k <- as.integer(k)
  n <- length(st)

  path <- character(0)
  rows <- list()

  for (step in seq_len(max_steps)) {
    r <- run_length(st)
    if (r >= n) break

    cand <- human_phase1_rank(st, k, sorted = TRUE)
    if (nrow(cand) == 0) break  # tail reached: phase 1 has nothing left to aim at

    best <- cand[1, ]
    ops <- strsplit(best$ops, ",", fixed = TRUE)[[1]]
    st <- as.integer(apply_operations(st, ops, k, compute_coords = FALSE)$state)
    path <- c(path, ops)

    if (trace) {
      rows[[length(rows) + 1]] <- data.frame(
        step = step,
        run_before = r,
        run_after = run_length(st),
        gap_cost = best$gap_cost,
        ops = best$ops,
        len = best$len,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    state = st,
    path = path,
    run = run_length(st),
    trace = if (length(rows)) do.call(rbind, rows) else NULL
  )
}
