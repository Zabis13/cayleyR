# How well can a coordinate predict graph distance?
#
# A coordinate is useful if states that are close in the Cayley graph land
# close in coordinate space, for ANY pair of states -- not just for pairs
# involving the identity. This script measures that directly: it builds the
# exact pairwise distance matrix by BFS, then scores each candidate coordinate
# against it.
#
# BFS here is the yardstick, not the coordinate. The point of a coordinate is
# to work on graphs too large to enumerate; BFS on a small graph is how we
# find out whether it does.
#
# Run with:  Rscript inst/examples/coord_diagnostics.R

library(cayleyR)

# ---------------------------------------------------------------- PARAMETERS

N <- 5                      # permutation size
K <- 3                      # reverse-prefix length
MOVES <- c("L", "R", "X")   # allowed operations

MAX_VERTICES <- 1200        # above this, sample sources instead of using all
N_PAIRS <- 3000             # pairs sampled for the correlation scores
MDS_DIMS <- c(2, 3, 5, 10)  # embedding dimensions to probe
SEED <- 1

# --------------------------------------------------------------- GROUND TRUTH

set.seed(SEED)

bfs <- cayley_bfs_full(1:N, k = K, moves = MOVES)
S <- do.call(rbind, lapply(strsplit(bfs$state_str, "_"), as.integer))
nv <- nrow(S)
idx <- setNames(seq_len(nv), bfs$state_str)

cat("Cayley graph: n =", N, " k =", K,
    " moves =", paste(MOVES, collapse = ","), "\n")
cat("Reachable vertices:", nv, "\n")

# Distance matrix, one BFS per source. Rows are sources, columns all vertices.
sources <- if (nv > MAX_VERTICES) sort(sample(nv, MAX_VERTICES)) else seq_len(nv)
D <- matrix(NA_integer_, length(sources), nv)
for (t in seq_along(sources)) {
  bi <- cayley_bfs_full(S[sources[t], ], k = K, moves = MOVES)
  D[t, idx[bi$state_str]] <- bi$dist
}
Dsq <- D[, sources, drop = FALSE]      # square block, for MDS
cat("Diameter:", max(D), "  mean pairwise distance:",
    round(mean(Dsq[upper.tri(Dsq)]), 2), "\n\n")

# ------------------------------------------------- VERTEX-TRANSITIVITY CHECK

# In a Cayley graph d(a,b) depends only on the "difference" a^-1 b, so a
# distance-from-identity function is enough to score any pair. Verify that
# numerically before relying on it.
inv_perm <- function(p) { q <- integer(length(p)); q[p] <- seq_along(p); q }
comp <- function(p, q) p[q]                    # (p o q)(i) = p[q[i]]

d_from_e <- setNames(bfs$dist, bfs$state_str)
errs <- 0
for (t in 1:400) {
  i <- sample(length(sources), 1); j <- sample(nv, 1)
  ab <- comp(inv_perm(S[sources[i], ]), S[j, ])
  key <- paste(ab, collapse = "_")
  if (!is.null(d_from_e[[key]]) && d_from_e[[key]] != D[i, j]) errs <- errs + 1
}
cat("Vertex-transitivity  d(a,b) == d(e, a^-1 b):",
    if (errs == 0) "holds (0/400 mismatches)" else paste(errs, "/400 MISMATCHES"), "\n\n")

# ------------------------------------------------------- CANDIDATE FEATURES

# Each candidate is a generic permutation statistic applied to a^-1 b, which
# is the vertex-transitive way to turn a distance-from-identity measure into a
# pairwise one.
pair_features <- function(a, b) {
  n <- length(a)
  r <- comp(inv_perm(a), b)

  inv <- sum(outer(seq_len(n), seq_len(n), "<") & outer(r, r, ">"))
  brk <- sum(diff(r) != 1L)
  cyc_brk <- sum(((diff(c(r, r[1])) - 1L) %% n) != 0L)

  seen <- logical(n); ncyc <- 0
  for (s in seq_len(n)) {
    if (seen[s]) next
    ncyc <- ncyc + 1; p <- s
    while (!seen[p]) { seen[p] <- TRUE; p <- r[p] }
  }

  # smallest Hamming distance over all rotations: the only candidate here that
  # respects the ring geometry the L/R generators impose
  ring_h <- min(vapply(0:(n - 1),
                       function(s) sum(r != (((seq_len(n) - 1 + s) %% n) + 1)),
                       numeric(1)))

  c(manhattan     = sum(abs(r - seq_len(n))),
    hamming       = sum(r != seq_len(n)),
    breakpoints   = brk,
    cyc_break     = cyc_brk,
    inversions    = inv,
    transposition = n - ncyc,
    ring_hamming  = ring_h)
}

ii <- sample(length(sources), N_PAIRS, TRUE)
jj <- sample(nv, N_PAIRS, TRUE)
keep <- sources[ii] != jj
ii <- ii[keep]; jj <- jj[keep]
truth <- D[cbind(ii, jj)]

FEAT <- t(vapply(seq_along(ii),
                 function(t) pair_features(S[sources[ii[t]], ], S[jj[t], ]),
                 numeric(7)))

# Fraction of vertex pairs a coordinate ranks in the wrong order. 0.5 is
# coin-flipping; 0 is perfect.
order_violations <- function(x) {
  a <- sample(length(truth), 4000, TRUE)
  b <- sample(length(truth), 4000, TRUE)
  ok <- truth[a] != truth[b]
  mean(sign(truth[a] - truth[b])[ok] != sign(x[a] - x[b])[ok])
}

cat("--- Candidate coordinates, scored on a^-1 b ---\n")
cat(sprintf("%-14s %8s %9s %10s\n", "feature", "corr", "spearman", "violations"))
for (f in colnames(FEAT)) {
  set.seed(SEED + 1)
  cat(sprintf("%-14s %8.3f %9.3f %10.3f\n", f,
              cor(truth, FEAT[, f]),
              cor(truth, FEAT[, f], method = "spearman"),
              order_violations(FEAT[, f])))
}

fit <- lm(truth ~ FEAT)
set.seed(SEED + 1)
cat(sprintf("\nall features combined: R^2 = %.3f  violations = %.3f\n",
            summary(fit)$r.squared, order_violations(fitted(fit))))

# ------------------------------------------------------------ HOW MANY DIMS?

# Classical MDS is given the whole distance matrix and fits coordinates
# optimally, so its score at k dimensions is an upper bound on what ANY
# k-dimensional formula can achieve.
cat("\n--- Upper bound: optimal embedding at k dimensions (classical MDS) ---\n")
ev <- cmdscale(as.dist(Dsq), k = max(MDS_DIMS), eig = TRUE)$eig
up <- upper.tri(Dsq)
cat(sprintf("%4s %12s %8s %10s\n", "k", "variance", "corr", "violations"))
for (k in MDS_DIMS) {
  X <- cmdscale(as.dist(Dsq), k = k)
  Dh <- as.matrix(dist(X))
  set.seed(SEED + 2)
  a <- sample(sum(up), 4000, TRUE); b <- sample(sum(up), 4000, TRUE)
  t1 <- Dsq[up][a]; t2 <- Dsq[up][b]
  ok <- t1 != t2
  cat(sprintf("%4d %11.1f%% %8.3f %10.3f\n", k,
              100 * sum(pmax(ev[1:k], 0)) / sum(pmax(ev, 0)),
              cor(Dsq[up], Dh[up]),
              mean(sign(t1 - t2)[ok] != sign(Dh[up][a] - Dh[up][b])[ok])))
}

cat("\n--- Information floor ---\n")
cat("bits needed to name one vertex: log2(", nv, ") =", round(log2(nv), 2), "\n")
cat("distinct (nL,nR,nX) triples along BFS paths:",
    length(unique(paste(bfs$nL, bfs$nR, bfs$nX))), "for", nv, "vertices\n")

cat("\n--- Pairwise distance distribution ---\n")
print(table(Dsq[up]))
