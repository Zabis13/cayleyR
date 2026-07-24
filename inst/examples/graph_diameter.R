# Diameter of a Cayley graph and its maximally distant state pairs.
#
# Everything you normally want to change lives in the PARAMETERS block below.
#
# Run with:  Rscript inst/examples/graph_diameter.R

library(cayleyR)

# ---------------------------------------------------------------- PARAMETERS

N <- 5                      # permutation size; the graph has up to N! vertices
K <- 3                      # reverse-prefix length for the X operation
MOVES <- c("L", "R", "X")   # allowed operations
START <- 1:N                # state to explore from

# "all_pairs"  - BFS from every vertex: the true diameter and every diametral
#                pair. Cost grows as |V| BFS runs, so keep N at 8 or below.
# "from_start" - a single BFS: eccentricity of START and the pairs (START, v)
#                realising it. Exact only if the graph is vertex-transitive,
#                but scales to much larger N.
METHOD <- "all_pairs"

MAX_PAIRS <- Inf            # cap on pairs listed in pairs_df (Inf = no cap)
SHOW_PAIRS <- 10            # how many pairs to print below
VERBOSE <- TRUE             # progress output during the sweep

# --------------------------------------------------------------------- RUN

cat("Cayley graph: n =", N, " k =", K,
    " moves =", paste(MOVES, collapse = ","),
    " method =", METHOD, "\n")
cat("Start state:", paste(START, collapse = " "), "\n\n")

t0 <- Sys.time()
res <- cayley_graph_diameter(
  start_state = START,
  k = K,
  moves = MOVES,
  method = METHOD,
  max_pairs = MAX_PAIRS,
  verbose = VERBOSE
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

# ------------------------------------------------------------------ REPORT

cat("\n--- Graph ---\n")
cat("Reachable vertices:", res$n_vertices, "of", factorial(N), "possible\n")
cat("Diameter:          ", res$diameter, "\n")
cat("Maximally distant pairs:", res$n_pairs, "\n")
cat("Elapsed:           ", round(elapsed, 2), "s\n")

cat("\n--- Distance distribution from start ---\n")
print(res$dist_hist, row.names = FALSE)

if (METHOD == "all_pairs") {
  cat("\n--- Eccentricity distribution ---\n")
  print(as.data.frame(table(ecc = res$ecc$ecc)), row.names = FALSE)
  cat("Radius:", min(res$ecc$ecc), " Diameter:", max(res$ecc$ecc), "\n")
}

cat("\n--- Maximally distant pairs ---\n")
if (res$truncated) {
  cat("(pairs_df capped at", MAX_PAIRS, "of", res$n_pairs, "total)\n")
}
show_n <- min(SHOW_PAIRS, nrow(res$pairs_df))
print(
  res$pairs_df[seq_len(show_n),
               c("from_state_str", "to_state_str", "dist",
                 "from_theta", "from_phi", "to_theta", "to_phi")],
  row.names = FALSE,
  digits = 4
)
if (nrow(res$pairs_df) > show_n) {
  cat("... and", nrow(res$pairs_df) - show_n, "more (see res$pairs_df)\n")
}

cat("\n--- Farthest states from start, with coordinates ---\n")
far <- res$bfs[res$bfs$dist == max(res$bfs$dist), ]
print(utils::head(far, SHOW_PAIRS), row.names = FALSE, digits = 4)

# `res` also holds:
#   res$bfs      - every reachable state: dist, nL/nR/nX, theta/phi/omega
#   res$ecc      - per-vertex eccentricity
#   res$pairs_df - all diametral pairs with coordinates of both endpoints
