#!/usr/bin/env Rscript
# Teaching a network to solve the 4x4x4 without showing it a single solution.
#
# The awkward thing about learning a distance-to-solved is that nobody knows
# what it is. Labelling a state with the length of the scramble that made it is
# the obvious move and it is wrong: that scramble is one path to the state, not
# the shortest, so the label is an upper bound that gets looser with depth, and
# a network trained on it learns the looseness too.
#
# Autodidactic iteration gets the labels from the network itself. A state's
# target is min over moves of 1 + v(child) -- one move, plus what the network
# currently believes about the best child. Circular, except that a child which
# is already solved counts as zero whatever the network says. So states one
# move from solved get an exact target immediately, their neighbours get an
# almost exact one from them, and correctness spreads outward. That is also why
# scrambles are drawn uniformly over depth and the loss has no weighting in it:
# the curriculum is in the targets, not the sampling.
#
# Run with:  Rscript inst/examples/demo_cube4_adi.R

library(cayleyR)

if (!requireNamespace("ggmlR", quietly = TRUE)) {
  stop("this demo needs ggmlR for the networks")
}

ITERATIONS   <- 200L
BATCH_STATES <- 5000L
MAX_DEPTH    <- 14L
BATCH_SIZE   <- 256L

set.seed(3)
g <- cube_group(4)

# ---- 1. The networks -------------------------------------------------------

# Value and policy are separate models, not two heads on one trunk: ggml_fit
# trains a single output, so a shared trunk would need a multi-output loss that
# ggmlR does not have yet. The cost is a second forward pass per move; the
# benefit is that each network trains under the loss that suits it.
net <- cube_adi_model(g, embed_dim = 32L, hidden = c(1024L, 512L),
                      backend = "auto")
print(net)

# ---- 2. Training -----------------------------------------------------------

cat("\ntraining", ITERATIONS, "iterations x", BATCH_STATES, "states\n\n")
t0  <- Sys.time()
net <- cube_adi_train(net, iterations = ITERATIONS,
                      batch_states = BATCH_STATES,
                      max_depth = MAX_DEPTH,
                      batch_size = BATCH_SIZE,
                      verbose = TRUE)
cat(sprintf("\ntrained in %.0f s\n", as.numeric(Sys.time() - t0, units = "secs")))

# ---- 3. Calibration --------------------------------------------------------

# What the network predicts against how far the scramble walked. These are not
# the same number and should not be: a random walk of 14 quarter turns lands
# closer to solved than 14 moves away, because it crosses its own tracks. A
# network reading well below the scramble depth at depth is the sign that it
# learned the distance rather than the label.
sc <- cayleyR:::cube_adi_scramble(g$ptr, 3000L, MAX_DEPTH)
pv <- as.numeric(ggmlR::ggml_predict(
  net$value, matrix(as.integer(sc$states) - 1L, nrow = nrow(sc$states)),
  batch_size = BATCH_SIZE))

cat("\nscramble depth -> mean predicted distance\n")
print(round(tapply(pv, sc$depth, mean), 2))

# ---- 4. Solving ------------------------------------------------------------

# Greedy descent on the value network, one forward pass per move, refusing to
# stand anywhere it has already been. Banning the inverse of the last move is
# not enough -- four of the same quarter turn also return to where they started
# -- so the walk remembers the states it has visited, one rule covering every
# length of cycle.
cat("\nsolving\n")
for (d in c(3L, 6L, 10L, MAX_DEPTH)) {
  s    <- cayleyR:::cube_adi_scramble(g$ptr, 30L, d)
  ok   <- 0L
  lens <- integer(0)
  for (i in seq_len(30L)) {
    r <- cube_adi_solve(net, s$states[i, ], budget = 100L,
                        batch_size = BATCH_SIZE)
    if (r$solved) {
      ok   <- ok + 1L
      lens <- c(lens, length(r$path))
    }
  }
  cat(sprintf("  depth %2d: solved %2d/30   mean path %.1f\n", d, ok,
              if (length(lens)) mean(lens) else NA_real_))
}
