#!/usr/bin/env Rscript
# Why phase 3 finishes on the states it reaches itself and not on the ones it
# is handed.
#
# diag_phase3_from_scratch.R settled two things. Phase 3's search is sound: 25
# of 25 on states walked back from solved with its own generators, including
# depth 10 at 527 nodes. And the number of wing pairs is not what separates the
# failures -- four pairs solved 6 of 6 while eight pairs solved 14 of 15, and
# the walked states that solve carry the same pair counts as the handed states
# that do not.
#
# So the difference is qualitative, and it must live in what phase 3's
# coordinate actually sees:
#
#   centres            24 centre pieces, by colour
#   wings              24 wings, canonicalised so that swapping the halves of a
#                      pair is not a difference
#   one parity bit     corner parity plus dedge parity
#
# This compares the two kinds of state across exactly those three, against the
# nearest of phase 3's 24 goals -- being far from one of them means nothing
# when 23 others are equally valid.
#
# It also reads the prune table's own verdict on each state. That is the part
# worth watching: if the table calls a state close while the search cannot
# finish from it, the table is underestimating, and the search has no gradient
# to follow -- which is exactly what 2,000,000 nodes with no answer looks like.
#
# Run with:  Rscript inst/examples/diag_phase3_coord.R
#            Rscript inst/examples/diag_phase3_coord.R 20

library(cayleyR)

args <- commandArgs(trailingOnly = TRUE)
n_try <- if (length(args) >= 1) as.integer(args[[1]]) else 12L

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

# One row of the comparison: the coordinate, the table's opinion, the outcome.
probe <- function(state, kind, label) {
  co <- cayleyR:::cube_phase3_coord_cpp(state)
  r  <- cayleyR:::cube_kociemba4_phase3_cpp(state, node_budget = node_budget)
  data.frame(
    kind = kind, label = label,
    pairs = pairs_made(state),
    centre_mismatch = co$centre_mismatch,
    wing_mismatch = co$wing_mismatch,
    parity_bit = co$parity_bit,
    goal_bit = co$goal_parity_bit,
    prune_bound = co$prune_bound,
    solved = isTRUE(r$found), nodes = r$nodes,
    moves = length(r$path),
    stringsAsFactors = FALSE)
}

hr("setup")
cat("Phase 3's coordinate is centres, canonicalised wings and one parity bit.\n")
cat("Mismatches are counted against the nearest of its 24 goals.\n")
cat("prune_bound is what the table says the distance is.\n")

rows <- list()

hr("states phase 3 reaches with its own generators")
cat(sprintf("  %-6s %-6s %-8s %-7s %-7s %-7s %-8s %s\n",
            "depth", "pairs", "centres", "wings", "bit", "bound", "solved",
            "nodes"))
for (d in c(4L, 8L, 12L)) {
  for (k in seq_len(4L)) {
    st <- cube_identity(N)
    for (j in seq_len(d)) st <- apply_word(st, sample(g3, 1L))
    r <- probe(st, "walked", paste0("depth ", d))
    rows[[length(rows) + 1L]] <- r
    cat(sprintf("  %-6d %-6d %-8d %-7d %-7d %-7d %-8s %s\n", d, r$pairs,
                r$centre_mismatch, r$wing_mismatch, r$parity_bit,
                r$prune_bound, if (r$solved) "yes" else "NO",
                format(r$nodes, scientific = FALSE, big.mark = ",")))
    flush.console()
  }
}

hr("states phases 1 and 2 hand over")
cat(sprintf("  %-6s %-6s %-8s %-7s %-7s %-7s %-8s %s\n",
            "n", "pairs", "centres", "wings", "bit", "bound", "solved",
            "nodes"))
for (k in seq_len(n_try)) {
  repeat {
    w <- sample(cube_move_names(N), 6L, replace = TRUE)
    s <- replay(cube_identity(N), w)
    if (!identical(s, cube_identity(N))) break
  }
  p12 <- cayleyR:::cube_kociemba4_phase12_cpp(s, upto_phase = 2L,
                                              node_budget = node_budget)
  s2 <- replay(s, p12)
  r <- probe(s2, "handed", paste(w, collapse = " "))
  rows[[length(rows) + 1L]] <- r
  cat(sprintf("  %-6d %-6d %-8d %-7d %-7d %-7d %-8s %s\n", k, r$pairs,
              r$centre_mismatch, r$wing_mismatch, r$parity_bit,
              r$prune_bound, if (r$solved) "yes" else "NO",
              format(r$nodes, scientific = FALSE, big.mark = ",")))
  flush.console()
}

tab <- do.call(rbind, rows)

hr("solved against unsolved, by each part of the coordinate")

# The comparison the run exists for. Each measure, averaged over the states
# phase 3 finished and the ones it did not. A measure that differs between the
# two groups is a candidate; one that does not is ruled out.
cmp <- function(col) {
  a <- tab[[col]][tab$solved]
  b <- tab[[col]][!tab$solved]
  cat(sprintf("  %-16s solved %6.1f   unsolved %6.1f   %s\n", col,
              if (length(a)) mean(a) else NA_real_,
              if (length(b)) mean(b) else NA_real_,
              if (length(a) && length(b) &&
                  abs(mean(a) - mean(b)) > 0.5) "<- differs" else ""))
}
for (col in c("pairs", "centre_mismatch", "wing_mismatch", "parity_bit",
              "prune_bound")) {
  cmp(col)
}

hr("the parity bit on its own")

# Checked separately because it is the one part of the coordinate that a
# generator can flip. A bit that disagrees with the goal is not by itself a
# blocker -- the search can flip it -- so what matters is whether the failures
# sit on one value and the successes on the other.
t_bit <- table(bit = tab$parity_bit, solved = tab$solved)
print(t_bit)
cat("\ngoal's own bit: ", tab$goal_bit[1], "\n", sep = "")

hr("what the table says versus what happened")

# The sharpest reading available. prune_bound is the table's claim about the
# distance; nodes is what the search actually spent. A state the table calls
# close that costs millions is a state the heuristic is wrong about.
cat(sprintf("  %-8s %-6s %-12s %s\n", "bound", "cubes", "solved", "mean nodes"))
for (b in sort(unique(tab$prune_bound))) {
  d <- tab[tab$prune_bound == b, ]
  cat(sprintf("  %-8d %-6d %-12s %s\n", b, nrow(d),
              sprintf("%d/%d", sum(d$solved), nrow(d)),
              format(round(mean(d$nodes)), scientific = FALSE,
                     big.mark = ",")))
}

hr("verdict")

un <- tab[!tab$solved, ]
so <- tab[tab$solved, ]

if (nrow(un) == 0) {
  cat("Nothing failed in this sample; raise the count to catch one.\n")
} else {
  cat(sprintf("%d of %d states failed.\n\n", nrow(un), nrow(tab)))

  # Which measure separates them best. Not by the gap between the means --
  # these columns are on different scales, and the widest gap belongs to
  # whichever happens to be measured in the biggest units. What matters is
  # whether the two groups overlap at all: a measure whose solved range and
  # unsolved range do not meet decides the outcome, and one whose ranges sit
  # inside each other decides nothing however far apart the averages are.
  sep <- vapply(c("pairs", "centre_mismatch", "wing_mismatch", "parity_bit",
                  "prune_bound"),
                function(col) {
                  a <- so[[col]]; b <- un[[col]]
                  if (!length(a) || !length(b)) return(0)
                  # Fraction of pairs (solved, unsolved) ordered the same way.
                  # 1 means a threshold splits the groups perfectly, 0.5 means
                  # the measure says nothing.
                  gt <- mean(outer(a, b, ">")); lt <- mean(outer(a, b, "<"))
                  eq <- mean(outer(a, b, "=="))
                  max(gt, lt) + eq / 2
                }, numeric(1))

  cat("how cleanly each measure splits solved from unsolved\n")
  cat("(1.0 = a threshold separates them perfectly, 0.5 = tells you nothing)\n\n")
  for (nm in names(sort(sep, decreasing = TRUE))) {
    cat(sprintf("  %-16s %.2f    solved %6.1f   unsolved %6.1f\n", nm,
                sep[[nm]], mean(so[[nm]]), mean(un[[nm]])))
  }

  best <- names(which.max(sep))
  cat(sprintf("\nThe measure that separates them is %s.\n", best))

  if (best == "prune_bound" || max(sep) < 0.8) {
    cat("\nThe table's own bound decides the outcome, and no part of the state\n")
    cat("does. That points at the table rather than at the cube.\n")

    # The reading that follows from how get() works, checked against the
    # table's depth rather than asserted.
    bd <- cayleyR:::cube_kociemba4_tables_cpp()$phase3$built_depth
    cat(sprintf("\nThe phase 3 table is built to depth %d. get() returns\n", bd))
    cat(sprintf("built_depth + 1 = %d for any state it has never reached, so a\n",
                bd + 1))
    cat(sprintf("bound of %d does not mean \"%d moves away\" -- it means \"not in\n",
                bd + 1, bd + 1))
    cat("the table\". Every such state gets the same score, the heuristic goes\n")
    cat("flat, and the search has no gradient left to follow. That is the\n")
    cat("difference between 500 nodes and two million.\n")
    cat("\nThe depth the table is filled to is set in kociemba_core.h:\n")
    cat("    int fill_to = limit / 2 + limits.prune_depth_bonus;\n")
    cat("Half the search depth. prune_depth_bonus raises it, and phase 3 is\n")
    cat("the only phase it reaches -- inst/examples/bench_phase3_bonus.R is\n")
    cat("the measurement.\n")
  } else {
    cat("\nThat is the thing to chase: fix it in one failing state, leaving\n")
    cat("the other two parts of the coordinate alone, and see whether phase 3\n")
    cat("then finishes quickly. If it does, the cause is confirmed.\n")
  }
}
