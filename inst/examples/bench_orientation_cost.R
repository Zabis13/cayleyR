# What each orientation costs, and whether a small budget would lose anything.
#
# The finding this measures. cube_kociemba4_reduce does not run one search: it
# runs one per orientation of the cube, twenty-four of them, and each is allowed
# the full node_budget. At the default that is up to 1.2 billion nodes for one
# cube, which is what the progress output showed -- a counter climbing to fifty
# million, resetting to five, and climbing again, seven times over.
#
# The perverse part is why the loop cannot stop early. It keeps the SHORTEST
# path, not the first (R/cube_kociemba.R): a rotation the phases happen to like
# can leave phase 3 one move from done while another leaves it twenty-eight out,
# and nothing in phases 1 and 2 prefers either -- their coordinates do not see
# the wings at all. So finding a solution does not end the search; only an empty
# path does.
#
# The proposal is to give each orientation a small budget instead of the full
# one, on the reasoning that a good orientation finishes in hundreds of nodes
# and a bad one does not finish at all. That reasoning hides an assumption:
# that the distribution is bipolar, cheap or hopeless with nothing in between.
# If some orientations solve at two to ten million nodes, a one-million cap
# scores them as failures -- and on a cube whose only workable orientation sits
# in that middle band, the cube stops being solved at all.
#
# The same shape has already appeared today: in bench_reduce_budget.R a cube
# reduced at 50 million and again at 200 million, with a real difference in
# between. Bipolarity is a hypothesis here, not a given.
#
# So this measures, per orientation and over several cubes:
#
#   the node cost of each success, binned, to show the distribution's shape
#   how many orientations succeed at all
#   the rank of the first success in the loop's own order, which says whether
#     the order is already good or whether the cheap ones are found last
#   what the cheapest and the shortest orientations are, which need not agree
#
# The reading that would make a small budget safe is a gap: a cluster of cheap
# successes, a cluster of failures, and nothing occupying the middle.

suppressMessages(library(cayleyR))

N  <- 4L
mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

n_states <- 5L
n_moves  <- 6L

# Per-orientation budget for the measurement itself. Generous on purpose: the
# question is where the successes actually fall, and a cap set at the value
# under consideration would answer it by assumption.
budget <- 5e7

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

solved <- seq_len(96L)

orientations <- cayleyR:::.cube4_orientations

hr("setup")
cat(sprintf("cubes        : %d scrambles of %d quarter turns\n", n_states, n_moves))
cat(sprintf("orientations : %d, in the loop's own order\n", length(orientations)))
cat(sprintf("budget       : %s nodes per orientation\n",
            format(budget, big.mark = ",", scientific = FALSE)))
cat("\nThe loop keeps the shortest path rather than the first, so it pays for\n")
cat("every orientation on every cube. Cost here is what one orientation costs.\n")

set.seed(2026)

rows <- list()

for (i in seq_len(n_states)) {
  scr   <- sample(names(mv), n_moves, replace = TRUE)
  state <- replay(solved, scr)

  hr(sprintf("cube %d", i))
  cat(sprintf("  scramble : %s\n\n", paste(scr, collapse = " ")))
  cat(sprintf("  %4s %-8s %-10s %8s %14s %9s\n",
              "rank", "rot", "outcome", "moves", "p3 nodes", "secs"))

  for (k in seq_along(orientations)) {
    rot <- orientations[[k]]

    t0 <- proc.time()[["elapsed"]]
    # One orientation at a time, by handing the loop a list of length one --
    # which is the branch in cube_kociemba4_reduce that skips the loop entirely.
    p <- try(cube_kociemba4_reduce(state, node_budget = budget,
                                   orientations = rot)$path, silent = TRUE)
    el <- proc.time()[["elapsed"]] - t0

    if (inherits(p, "try-error")) {
      ok <- FALSE; np <- NA_integer_
      rep3 <- list(phase3 = "error", phase3_nodes = NA_real_)
    } else {
      rep3 <- cube_kociemba4_report()
      ok <- length(p) > 0 && cube_is_reduced(replay(state, p))
      np <- if (ok) length(p) else NA_integer_
    }

    cat(sprintf("  %4d %-8s %-10s %8s %14s %9.1f\n", k,
                if (nzchar(rot)) rot else "(-)",
                if (ok) "reduced" else rep3$phase3,
                if (is.na(np)) "-" else np,
                format(rep3$phase3_nodes, big.mark = ",", scientific = FALSE),
                el))
    flush.console()

    rows[[length(rows) + 1L]] <- data.frame(
      cube = i, rank = k, rot = rot, ok = ok, moves = np,
      nodes = rep3$phase3_nodes, outcome = rep3$phase3, secs = el,
      stringsAsFactors = FALSE)
  }
}

tab <- do.call(rbind, rows)

# ---- The distribution, which is the whole point -----------------------------
hr("what a success costs")

wins <- tab[tab$ok, ]
cat(sprintf("  orientations that reduced : %d of %d  (%.0f%%)\n",
            nrow(wins), nrow(tab), 100 * nrow(wins) / nrow(tab)))

if (nrow(wins) > 0) {
  brk <- c(0, 1e4, 1e5, 1e6, 1e7, 5e7, Inf)
  lab <- c("under 10K", "10K - 100K", "100K - 1M", "1M - 10M",
           "10M - 50M", "over 50M")
  b <- cut(wins$nodes, breaks = brk, labels = lab, right = FALSE)
  cat("\n  successes by node cost:\n")
  for (l in lab) {
    n <- sum(b == l, na.rm = TRUE)
    cat(sprintf("    %-12s %4d  %s\n", l, n, strrep("#", n)))
  }

  mid <- sum(wins$nodes >= 1e6, na.rm = TRUE)
  cat(sprintf("\n  successes costing 1M nodes or more : %d of %d\n",
              mid, nrow(wins)))
  cat("  These are the ones a small per-orientation budget would discard.\n")
}

# ---- Is the loop's order any good? -----------------------------------------
hr("where the first success falls in the order")

first <- tapply(seq_len(nrow(tab)), tab$cube, function(ix) {
  d <- tab[ix, ]
  w <- which(d$ok)
  if (length(w) == 0) NA_integer_ else d$rank[w[1]]
})
cat(sprintf("  first success at rank : %s\n",
            paste(ifelse(is.na(first), "none", first), collapse = " ")))
cat(sprintf("  cubes with no success : %d of %d\n",
            sum(is.na(first)), n_states))

# ---- Cheapest against shortest ---------------------------------------------
#
# The loop pays for all twenty-four in order to keep the shortest. Whether that
# is worth it depends on how much shorter the best is than the first available.
hr("cheapest orientation against shortest")

cat(sprintf("  %4s %10s %10s %12s %12s\n",
            "cube", "n wins", "first len", "shortest len", "cheapest len"))
for (i in seq_len(n_states)) {
  d <- tab[tab$cube == i & tab$ok, ]
  if (nrow(d) == 0) {
    cat(sprintf("  %4d %10d %10s %12s %12s\n", i, 0L, "-", "-", "-"))
    next
  }
  cheapest <- d$moves[which.min(d$nodes)]
  cat(sprintf("  %4d %10d %10d %12d %12d\n", i, nrow(d), d$moves[1],
              min(d$moves), cheapest))
}

hr("what this says")
cat("  A small per-orientation budget is safe only if the successes cluster\n")
cat("  well below it with nothing in the middle band. Read the histogram: mass\n")
cat("  in 1M - 10M is the warning sign, and the count printed under it is\n")
cat("  exactly what a 1M cap would throw away.\n")
cat("\n")
cat("  If instead the successes are cheap and plentiful, the bigger win is\n")
cat("  not the budget at all -- it is that the loop pays for twenty-four\n")
cat("  orientations to keep the shortest path. Whether the shortest is worth\n")
cat("  twenty-three extra searches is the last column above.\n")
