#!/usr/bin/env Rscript
# Finding a path on the 3x3x3 cube with find_path_iterative().
#
# BFS is hopeless here: 4.3e19 states, and even a depth-8 sphere does not fit in
# memory. find_path_iterative() does not try to enumerate anything. It grows two
# clouds of states -- one from the scramble, one from the target -- by walking
# random words, and looks for a state both clouds have reached. A hit gives the
# whole path at once: the ops that led there from the start, plus the inverse of
# the ops that led there from the target.
#
# When no state is shared, the cycle ends by picking a BRIDGE on each side: the
# state closest to the other side's cloud. The next cycle restarts from those
# bridges, so the two clouds are dragged toward each other. That is the loop --
# grow, intersect, bridge, repeat.
#
# Run with:  Rscript inst/examples/demo_cube3_path_iterative.R

library(cayleyR)

N <- 3L
g <- cube_group(N)
id <- cube_identity(N)

hr <- function(title) cat("\n== ", title, " ", strrep("-", max(0, 58 - nchar(title))), "\n", sep = "")

# ---------------------------------------------------------------- the problem

hr("the search space")

cat("group             :", g$name, "\n")
cat("state length      :", length(id), "stickers\n")
cat("moves             :", length(group_moves(g)), "\n")
cat("states            : 4.3e19 -- enumeration is off the table\n")

# generate_state walks the group, so the state it returns is always one a real
# cube can reach -- a random permutation of 1:54 almost never is. The walk is
# long on purpose: after a few hundred random quarter turns the state is deep in
# the graph, with no short word back that anyone knows how to name.
set.seed(11)
n_moves <- sample(200:1000, 1)
target <- generate_state(group = g, n_moves = n_moves)

cat("\nscramble          :", n_moves, "random quarter turns\n")
cat("stickers moved    :", sum(target != id), "of", length(id), "\n")
cat("colour-solved     :", cube_is_colour_solved(target, N), "\n")

# ---------------------------------------------------------------- the search

hr("running the search")

# combo_length is how far each random word walks, n_samples how many are drawn
# per cycle, n_top how many of them are kept and expanded into states. Longer
# combos cover more ground per cycle, which is what a deeply scrambled target
# needs -- there is no nearby state to overshoot.
t0 <- Sys.time()
res <- find_path_iterative(id, target, group = g,
                           combo_length = 20L, n_samples = 300L, n_top = 15L,
                           max_iterations = 10L, opd = TRUE, verbose = TRUE)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

# ---------------------------------------------------------------- the result

hr("the result")

cat("found             :", res$found, "\n")
cat("cycles used       :", res$cycles, "\n")
cat("elapsed           :", sprintf("%.1f s", elapsed), "\n")

if (res$found) {
  cat("path length       :", length(res$path), "moves\n")
  cat("path              :", paste(res$path, collapse = " "), "\n")

  # The only claim worth making: replay it and see if the cube lands on target.
  cat("replays to target : ",
      identical(group_apply(g, id, res$path), target), "\n", sep = "")

  # The path is a path, not an optimal one. The search stops at the first
  # intersection it can validate, and a random walk rarely arrives the short way.
  # God's number for the quarter-turn metric is 26: every cube state is at most
  # 26 moves from solved. Nothing here comes close, and nothing here tries to.
  cat("\nthe scramble took", n_moves, "moves out; the search found a walk of",
      length(res$path), "\nback to it. Neither is short -- an optimal solver would",
      "work in\n26 moves or fewer. This finds A path, not THE path.\n")
} else {
  cat("\nno intersection was found in", res$cycles, "cycles. The two clouds\n")
  cat("never landed on a common state. Retry with a different seed, more\n")
  cat("samples (n_samples), or more cycles (max_iterations).\n")
}

# ---------------------------------------------------------------- the bridges

hr("the bridges")

# Each cycle leaves one bridge per side. Reading them in order shows the search
# working: the ops recorded on a bridge are the segment of the final path that
# was traversed during that cycle.
cat("the search kept", length(res$bridge_states_start), "bridges on the start side,",
    length(res$bridge_states_final), "on the final side\n")
cat("(entry 1 on each side is the root -- the scramble and the target themselves)\n\n")

show_bridges <- function(bridges, label) {
  cat(label, ":\n", sep = "")
  for (i in seq_along(bridges)) {
    b <- bridges[[i]]
    n_ops <- if (is.null(b$ops)) 0L else length(b$ops)
    cat(sprintf("  cycle %d : %2d stickers off solved, reached by %d ops\n",
                b$cycle, sum(b$state != id), n_ops))
  }
}
show_bridges(res$bridge_states_start, "start side")
cat("\n")
show_bridges(res$bridge_states_final, "final side")

# ---------------------------------------------------------------- the knobs

hr("the knobs that matter")

cat("combo_length   how far each random word walks. Long words cover more\n")
cat("               ground but overshoot a nearby target.\n")
cat("n_samples      random words drawn per cycle; n_top how many are kept.\n")
cat("               More of both = denser clouds = likelier intersection,\n")
cat("               at a linear cost in time and memory.\n")
cat("opd            after a bridge is chosen, restrict the cycle to the combos\n")
cat("               that actually passed through it. Prunes hard.\n")
cat("keep_states    FALSE (default) drops each cycle's states once its bridge\n")
cat("               is recorded, so memory stays flat across cycles. TRUE\n")
cat("               keeps everything and lets intersections cross cycles.\n")
cat("one_sided      expand the final side once, then freeze it and advance\n")
cat("               from the start alone.\n")

cat("\nnote: distance_method must stay \"manhattan\" here. The \"human\" and\n")
cat("\"breakpoints\" heuristics score closeness on a ring, which is a TopSpin\n")
cat("notion with no counterpart on a cube; the function refuses them.\n")

cat("\n")
