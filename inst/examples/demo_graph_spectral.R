#!/usr/bin/env Rscript
# Visualise the whole Cayley graph in SPECTRAL coordinates.
#
# Companion to demo_graph_celestial.R. Instead of placing each state by the
# (theta, phi, omega) of the BFS path that reached it -- which collapses most
# distinct states onto a shared integer-lattice point and produces the
# rectangular grid you see there -- this places each state by the eigenvectors
# of the graph Laplacian. Those are a property of the STATE, not of the path,
# so every state gets a distinct position (0 collisions) and the layout shows
# the intrinsic geometry of the graph rather than the lattice of move counters.
#
# The coordinates are the 1st..3rd non-trivial Laplacian eigenvectors (the
# Fiedler vector and the next two): the lowest-frequency modes, i.e. the
# smoothest embedding that respects the edge structure.
#
# Run with:  Rscript inst/examples/demo_graph_spectral.R

library(cayleyR)
library(cgvR)

# ---------------------------------------------------------------- PARAMETERS

N <- 6                      # permutation size; graph has up to N! vertices
K <- 4                      # reverse-prefix length
MOVES <- c("L", "R", "X")   # allowed operations
START <- 1:N                # state to explore from

NORMALISED <- TRUE          # TRUE: normalised Laplacian D^-1/2 (D-A) D^-1/2
                            # FALSE: combinatorial Laplacian D - A

# "spectral" : raw eigenvectors as coordinates (all modes weighted equally).
#              Radius in the cloud is NOT graph distance; the picture is a
#              symmetric ball with the start near the centre.
# "diffusion": diffusion map -- each eigenvector is scaled by exp(-t*lambda),
#              so slow (low-lambda) modes dominate and Euclidean distance in the
#              coordinates approximates the diffusion distance on the graph.
#              Larger DIFF_T = coarser view (fewer effective modes).
EMBEDDING <- "diffusion"
DIFF_T    <- 8              # diffusion time; only used when EMBEDDING="diffusion"

COORDS <- c(2, 3, 4)        # which eigenvector columns to use as x,y,z
                            # (1 = constant null mode, so start at 2)
SHOW_PAIRS <- 3             # how many diametral pairs to highlight (0 = none)
SCALE <- 10                 # overall spread of the positions
SEED <- 1

# --------------------------------------------------------------------- GRAPH

set.seed(SEED)

bfs <- cayley_bfs_full(START, k = K, moves = MOVES)
nv <- nrow(bfs)
idx <- setNames(seq_len(nv), bfs$state_str)
S <- do.call(rbind, lapply(strsplit(bfs$state_str, "_"), as.integer))

cat(sprintf("Cayley graph: n=%d k=%d moves=%s\n", N, K,
            paste(MOVES, collapse = ",")))
cat(sprintf("Vertices: %d of %d possible\n", nv, factorial(N)))

# Edges: one per (state, operation), deduplicated as undirected pairs.
ef <- integer(0); et <- integer(0)
for (i in seq_len(nv)) {
  for (op in MOVES) {
    child <- apply_operations(S[i, ], op, K)$state
    j <- idx[[paste(child, collapse = "_")]]
    ef <- c(ef, i); et <- c(et, j)
  }
}
ekey <- ifelse(ef < et, paste(ef, et), paste(et, ef))
keep <- !duplicated(ekey) & ef != et
edges <- cbind(ef[keep], et[keep])
cat(sprintf("Edges: %d\n", nrow(edges)))

# ---------------------------------------------------- SPECTRAL POSITIONS

# Adjacency from the edge list.
A <- matrix(0, nv, nv)
A[edges] <- 1
A[edges[, 2:1]] <- 1
deg <- rowSums(A)

L <- diag(deg) - A
if (NORMALISED) {
  dm <- 1 / sqrt(deg)
  L <- (dm * L) * rep(dm, each = nv)   # D^-1/2 L D^-1/2
}

ev <- eigen(L, symmetric = TRUE)
o <- order(ev$values)                  # ascending: smoothest modes first
vals <- ev$values[o]
vecs <- ev$vectors[, o, drop = FALSE]

cat(sprintf("Spectral gap (lambda_2): %.4f\n", vals[2]))
cat(sprintf("Lowest eigenvalues: %s\n",
            paste(round(vals[1:min(8, nv)], 4), collapse = ", ")))
# A degenerate lambda_2 (repeated) means the choice of eigenvectors within that
# eigenspace is arbitrary -- the picture is one valid rotation of many.
mult <- sum(abs(vals - vals[2]) < 1e-6)
if (mult > 1)
  cat(sprintf("Note: lambda_2 has multiplicity %d; embedding basis is arbitrary within it.\n",
              mult))

if (EMBEDDING == "spectral") {
  # Raw eigenvectors as coordinates. Equal weight on all chosen modes; radius in
  # the cloud carries no distance information (the graph is vertex-transitive,
  # so no node is a geometric centre) -- distances between nodes do.
  pos0 <- vecs[, COORDS, drop = FALSE]

} else {
  # Diffusion map CENTRED ON THE START. Naively scaling the (degenerate) modes
  # 2..4 by exp(-t*lambda) does nothing -- they share a lambda, so the radius
  # only rescales and far-from-start nodes stay near the centre. What actually
  # makes distance-from-start show up as radius is the diffusion distance to the
  # start node v0: coordinate i becomes exp(-t*lambda_i) * (psi_i(v) - psi_i(v0)),
  # whose Euclidean norm is exactly the diffusion distance to v0 and grows with
  # graph distance from the start.
  v0 <- 1L
  w  <- exp(-DIFF_T * vals[-1])                      # all non-trivial modes
  D  <- sweep(vecs[, -1, drop = FALSE], 2, vecs[v0, -1], `-`)  # psi(v) - psi(v0)
  D  <- sweep(D, 2, w, `*`)                          # weighted difference

  # Pick the 3 modes carrying the most diffusion energy relative to the start,
  # so the 3-D radius tracks distance-from-start instead of an arbitrary slice.
  energy <- colSums(D^2)
  sel    <- order(energy, decreasing = TRUE)[1:3]
  pos0   <- D[, sel, drop = FALSE]

  dd <- sqrt(rowSums(D^2))                           # full diffusion distance
  cat(sprintf("Diffusion map centred on start: t=%.1f\n", DIFF_T))
  cat(sprintf("  corr(radius, BFS distance from start) = %.3f\n",
              cor(sqrt(rowSums(pos0^2)), bfs$dist)))
  cat(sprintf("  corr(full diffusion distance, BFS distance) = %.3f\n",
              cor(dd, bfs$dist)))
}

rmax <- max(sqrt(rowSums(pos0^2)))
if (rmax > 0) pos0 <- pos0 / rmax * SCALE

# In spectral coordinates distinct states get distinct positions, so no jitter.
dup <- sum(duplicated(round(pos0, 9)))
cat(sprintf("Nodes sharing a position: %d of %d\n", dup, nv))

pos <- pos0

# ------------------------------------------------------------------ RENDER

v <- cgv_viewer(1280, 720,
                sprintf("cayleyR: TopSpin(%d,%d) in %s coords", N, K, EMBEDDING))
cgv_background(v, "black")

sizes <- pmax(4, 14 - bfs$dist * 1.0)
cgv_set_graph(v, seq_len(nv), edges,
              positions   = pos,
              node_values = as.double(bfs$dist),
              node_sizes  = as.double(sizes))

# Start vertex in green.
cgv_highlight_path(v, 1L, color = "#00FF66", node_scale = 3.0)

# Diametral pairs.
if (SHOW_PAIRS > 0) {
  res <- cayley_graph_diameter(START, k = K, moves = MOVES,
                               method = "from_start")
  cat(sprintf("Diameter: %d   pairs at that distance: %d\n",
              res$diameter, res$n_pairs))
  far <- which(bfs$dist == res$diameter)
  for (t in seq_len(min(SHOW_PAIRS, length(far)))) {
    cgv_highlight_path(v, far[t], color = "#3388FF", node_scale = 3.0)
  }
  cat(sprintf("Highlighted %d farthest node(s) in blue\n",
              min(SHOW_PAIRS, length(far))))
}

cm <- colMeans(pos)
dd <- max(sqrt(rowSums(sweep(pos, 2, cm)^2))) * 2
cgv_camera(v, position = cm + c(dd * 0.7, dd * 0.6, dd * 0.8), target = cm)

cat("Right mouse drag = rotate, scroll = zoom. Close window to exit.\n")
cgv_run(v)
