#!/usr/bin/env Rscript
# Can phase 3 pair the wings from nothing, and what does it cost?
#
# The trace in diag_trace_phases.R showed phases 1 and 2 handing phase 3 a cube
# with zero of the twelve dedges paired -- fewer than the scramble itself left.
# That looks damning, but it may be perfectly normal: reduction methods do not
# ask the centre phases to preserve pairs, and pairing the wings is precisely
# what phase 3 exists to do. twips's phases very likely break pairs too.
#
# So the question is not "do phases 1 and 2 break pairs" but "is phase 3 able
# to pair them from scratch at a sensible cost". Those lead to opposite fixes:
#
#   phase 3 handles it cheaply   then breaking pairs is fine, and the fault is
#                                in the handover -- phase 2's output, or the
#                                route it takes to get there
#   phase 3 cannot handle it     then the fault is phase 3's own: its
#                                heuristic does not measure the cost of
#                                rebuilding pairs, and no change to phases 1
#                                and 2 will help
#
# Phase 3 is therefore run here on states reached WITHOUT phases 1 and 2, so
# nothing about the handover can be blamed. Two kinds of input:
#
#   walked   from the solved cube, a few moves of phase 3's OWN generators. Its
#            goal is reachable by construction, and the distance is bounded by
#            the walk -- the fairest possible test of the search
#   handed   what phases 1 and 2 actually produce, for comparison
#
# The walked states are the control. If phase 3 cannot solve a cube it is six
# of its own moves away from, the problem is inside phase 3 and has nothing to
# do with what came before.
#
# Run with:  Rscript inst/examples/diag_phase3_from_scratch.R
#            Rscript inst/examples/diag_phase3_from_scratch.R 10   # 10 per depth

library(cayleyR)

args    <- commandArgs(trailingOnly = TRUE)
per_len <- if (length(args) >= 1) as.integer(args[[1]]) else 5L

N           <- 4L
node_budget <- 2e6

set.seed(2026)

mv <- cube_moves(N)
names(mv) <- cube_move_names(N)

hr <- function(title) {
  cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")
}

replay <- function(state, path) {
  for (m in path) state <- state[mv[[m]]]
  state
}

pairs_made <- function(s) {
  p <- cube_pieces(4)
  e <- p[p$n_stickers == 2L, ]
  st <- lapply(strsplit(e$stickers, ","), as.integer)
  key <- vapply(st, function(i) paste(sort((i - 1L) %/% 16L), collapse = "-"),
                character(1))
  sum(vapply(split(st, key), function(g2) {
    a <- sort((s[g2[[1]]] - 1L) %/% 16L)
    b <- sort((s[g2[[2]]] - 1L) %/% 16L)
    identical(a, b)
  }, logical(1)))
}

# Phase 3's generators, as words of the package's alphabet. Taken from the
# solver rather than written out here, so this cannot drift from what the
# phase actually searches with.
g3 <- cayleyR:::cube_phase_generators_cpp(3L)$names

apply_word <- function(state, word) {
  for (tok in strsplit(trimws(word), " +")[[1]]) {
    if (grepl("2$", tok) && !tok %in% names(mv)) {
      base <- sub("2$", "", tok)
      state <- state[mv[[base]]]
      state <- state[mv[[base]]]
    } else {
      state <- state[mv[[tok]]]
    }
  }
  state
}

hr("setup")
cat("Phase 3 run on states it can reach by construction, so nothing about\n")
cat("the handover from phases 1 and 2 can be blamed for a failure.\n\n")
cat("phase 3 generators : ", length(g3), "\n", sep = "")
cat("node budget        : ", format(node_budget, scientific = FALSE,
                                    big.mark = ","), "\n", sep = "")

hr("walked back from solved, using phase 3's own moves")
cat("A cube d generator-moves from solved has a phase 3 solution at most d\n")
cat("moves long. Anything else is the search failing at its own job.\n\n")
cat(sprintf("  %-6s %-4s %-8s %-13s %-12s %s\n",
            "depth", "n", "solved", "wings paired", "nodes", "moves"))

rows <- list()
for (d in c(2L, 4L, 6L, 8L, 10L)) {
  for (k in seq_len(per_len)) {
    st <- cube_identity(N)
    for (j in seq_len(d)) st <- apply_word(st, sample(g3, 1L))

    r <- cayleyR:::cube_kociemba4_phase3_cpp(st, node_budget = node_budget)
    rows[[length(rows) + 1L]] <- data.frame(
      kind = "walked", depth = d, cube = k,
      pairs_before = pairs_made(st),
      solved = isTRUE(r$found), nodes = r$nodes,
      moves = length(r$path), outcome = r$outcome,
      stringsAsFactors = FALSE)
  }
  d_rows <- do.call(rbind, rows[vapply(rows, function(x) x$depth == d, logical(1))])
  cat(sprintf("  %-6d %-4d %-8s %-13.1f %-12s %s\n", d, per_len,
              sprintf("%d/%d", sum(d_rows$solved), nrow(d_rows)),
              mean(d_rows$pairs_before),
              format(round(mean(d_rows$nodes)), scientific = FALSE,
                     big.mark = ","),
              if (any(d_rows$solved))
                sprintf("%.1f", mean(d_rows$moves[d_rows$solved])) else "-"))
  flush.console()
}

hr("what phases 1 and 2 actually hand over")
cat("The same search, on real output of phases 1 and 2, for comparison.\n\n")
cat(sprintf("  %-4s %-13s %-8s %-12s %s\n",
            "n", "wings paired", "solved", "nodes", "moves"))

for (k in seq_len(per_len * 2L)) {
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = node_budget)
  s2 <- replay(s, p12)
  r <- cayleyR:::cube_kociemba4_phase3_cpp(s2, node_budget = node_budget)

  rows[[length(rows) + 1L]] <- data.frame(
    kind = "handed", depth = NA_integer_, cube = k,
    pairs_before = pairs_made(s2),
    solved = isTRUE(r$found), nodes = r$nodes,
    moves = length(r$path), outcome = r$outcome,
    stringsAsFactors = FALSE)

  cat(sprintf("  %-4d %-13d %-8s %-12s %s\n", k, pairs_made(s2),
              if (isTRUE(r$found)) "yes" else "no",
              format(r$nodes, scientific = FALSE, big.mark = ","),
              if (isTRUE(r$found)) length(r$path) else "-"))
  flush.console()
}

tab <- do.call(rbind, rows)

hr("by how many pairs phase 3 started with")

# The question this run exists to answer, read straight off the data: does the
# cost of phase 3 track the number of pairs it is given?
by_pairs <- do.call(rbind, lapply(split(tab, tab$pairs_before), function(d) {
  data.frame(pairs = d$pairs_before[1], cubes = nrow(d),
             solved = sprintf("%d/%d", sum(d$solved), nrow(d)),
             mean_nodes = round(mean(d$nodes)),
             stringsAsFactors = FALSE)
}))
print(by_pairs, row.names = FALSE)

hr("verdict")

walked <- tab[tab$kind == "walked", ]
handed <- tab[tab$kind == "handed", ]

cat(sprintf("walked states (goal reachable by construction) : %d of %d solved\n",
            sum(walked$solved), nrow(walked)))
cat(sprintf("states handed over by phases 1 and 2           : %d of %d solved\n",
            sum(handed$solved), nrow(handed)))

zero <- walked[walked$pairs_before == 0, ]
if (nrow(zero) > 0) {
  cat(sprintf("\nwalked states with zero pairs                  : %d of %d solved",
              sum(zero$solved), nrow(zero)))
  cat(sprintf("  (mean %s nodes)\n",
              format(round(mean(zero$nodes)), scientific = FALSE,
                     big.mark = ",")))
}

cat("\n")
deep <- walked[walked$depth >= 6L, ]
if (nrow(deep) > 0 && mean(deep$solved) < 0.5) {
  cat("Phase 3 cannot solve cubes it is a few of its OWN moves away from.\n")
  cat("Nothing about phases 1 and 2 is involved -- these states were built by\n")
  cat("walking phase 3's generators back from solved, so a short solution is\n")
  cat("guaranteed to exist. The fault is inside phase 3: its prune table does\n")
  cat("not measure the cost of pairing wings, so the search has no gradient to\n")
  cat("follow and explores the tree at close to its full branching factor.\n")
  cat("\nBreaking pairs in phases 1 and 2 is therefore NOT the bug. It is what\n")
  cat("reduction does. Phase 3 is supposed to pair them and cannot.\n")
} else if (nrow(deep) > 0 && mean(deep$solved) > 0.8 &&
           nrow(handed) > 0 && mean(handed$solved) < 0.5) {
  cat("Phase 3 handles its own states well and fails on what phases 1 and 2\n")
  cat("hand it. So the search is sound and the handover is not: phase 2 ends\n")
  cat("somewhere phase 3 finds hard, even though phase 3 can reach its goal\n")
  cat("from comparable states. Compare the wing-pair counts in the table\n")
  cat("above -- if the handed states are not worse by that measure, the\n")
  cat("difference is in the centres or the parity bit, not the pairing.\n")
} else {
  cat("Phase 3 solves both kinds at a similar rate. Whatever separates the\n")
  cat("failures, it is not the number of pairs it starts with -- read the\n")
  cat("table above for what does move with the node count.\n")
}
