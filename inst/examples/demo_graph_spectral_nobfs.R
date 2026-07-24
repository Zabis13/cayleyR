#!/usr/bin/env Rscript
# Visualise the whole Cayley graph in SPECTRAL / DIFFUSION coordinates,
# WITHOUT calling cayley_bfs_full.
#
# demo_graph_spectral.R gets the vertex set from a BFS. It doesn't need to:
# the vertices of a Cayley graph are the group elements reachable from the
# start, and the edges are given algebraically by op . v. This script builds
# the vertex set by closing the start's orbit under L/R/X to a fixed point
# (iterative expansion, not a levelled BFS), derives the adjacency from the same
# op . v rule, and places nodes by the Laplacian spectrum -- none of which needs
# graph distances.
#
# Because there is no BFS, there are no BFS levels. Colour and the "far" node
# highlight are driven by the DIFFUSION DISTANCE to the start instead, which is
# available directly from the spectral embedding.
#
# Run with:  Rscript inst/examples/demo_graph_spectral_nobfs.R

library(cayleyR)
library(cgvR)

# ---------------------------------------------------------------- PARAMETERS

N <- 6                      # permutation size; graph has up to N! vertices
K <- 4                      # reverse-prefix length
MOVES <- c("L", "R", "X")   # allowed operations
START <- 1:N                # state to explore from

NORMALISED <- TRUE          # TRUE: normalised Laplacian D^-1/2 (D-A) D^-1/2
                            # FALSE: combinatorial Laplacian D - A

# "spectral" : raw eigenvectors as coordinates; symmetric ball, start central.
# "diffusion": diffusion map centred on the start; radius tracks distance from
#              the start. Larger DIFF_T = coarser view.
EMBEDDING <- "diffusion"
DIFF_T    <- 8

COORDS <- c(2, 3, 4)        # eigenvector columns for x,y,z in "spectral" mode
SHOW_FAR <- 3               # highlight this many farthest-from-start nodes
SCALE <- 10
ORBIT <- TRUE               # TRUE: camera auto-orbits the cloud's centre
SEED <- 1

# ------------------------------------------------- VERTICES BY ORBIT CLOSURE

set.seed(SEED)

# op . v without BFS: the same three operations, applied to a state vector.
apply_op <- function(s, op, k) {
  n <- length(s)
  if (op == "L") return(c(s[-1], s[1]))                     # shift left
  if (op == "R") return(c(s[n], s[-n]))                     # shift right
  if (op == "X") return(c(rev(s[seq_len(k)]), s[-seq_len(k)]))  # reverse prefix
  stop("unknown op: ", op)
}
key <- function(s) paste(s, collapse = "_")

# Close the start's orbit under the generators: keep applying every op to every
# newly discovered state until nothing new appears. This is set closure to a
# fixed point -- it yields the same vertex set as a full BFS, but records no
# distances or levels.
seen   <- new.env(parent = emptyenv())
states <- list()
frontier <- list(as.integer(START))
assign(key(START), TRUE, envir = seen)
states[[1]] <- as.integer(START)

while (length(frontier) > 0) {
  nxt <- list()
  for (s in frontier) {
    for (op in MOVES) {
      c_ <- apply_op(s, op, K)
      kk <- key(c_)
      if (!exists(kk, envir = seen, inherits = FALSE)) {
        assign(kk, TRUE, envir = seen)
        states[[length(states) + 1]] <- c_
        nxt[[length(nxt) + 1]] <- c_
      }
    }
  }
  frontier <- nxt
}

S  <- do.call(rbind, states)
nv <- nrow(S)
idx <- setNames(seq_len(nv), apply(S, 1, key))

cat(sprintf("Cayley graph: n=%d k=%d moves=%s\n", N, K,
            paste(MOVES, collapse = ",")))
cat(sprintf("Vertices: %d of %d possible (orbit closure, no BFS)\n",
            nv, factorial(N)))

# Edges from op . v, deduplicated as undirected pairs.
ef <- integer(0); et <- integer(0)
for (i in seq_len(nv)) {
  for (op in MOVES) {
    j <- idx[[key(apply_op(S[i, ], op, K))]]
    ef <- c(ef, i); et <- c(et, j)
  }
}
ekey <- ifelse(ef < et, paste(ef, et), paste(et, ef))
keep <- !duplicated(ekey) & ef != et
edges <- cbind(ef[keep], et[keep])
cat(sprintf("Edges: %d\n", nrow(edges)))

# ---------------------------------------------------- SPECTRAL POSITIONS

A <- matrix(0, nv, nv)
A[edges] <- 1
A[edges[, 2:1]] <- 1
deg <- rowSums(A)

L <- diag(deg) - A
if (NORMALISED) {
  dm <- 1 / sqrt(deg)
  L <- (dm * L) * rep(dm, each = nv)
}

ev <- eigen(L, symmetric = TRUE)
o <- order(ev$values)
vals <- ev$values[o]
vecs <- ev$vectors[, o, drop = FALSE]

cat(sprintf("Spectral gap (lambda_2): %.4f\n", vals[2]))
cat(sprintf("Lowest eigenvalues: %s\n",
            paste(round(vals[1:min(8, nv)], 4), collapse = ", ")))
mult <- sum(abs(vals - vals[2]) < 1e-6)
if (mult > 1)
  cat(sprintf("Note: lambda_2 has multiplicity %d; embedding basis is arbitrary within it.\n",
              mult))

# The start vertex is row 1 (it seeded the closure).
v0 <- 1L

# Diffusion distance from every node to the start, over all non-trivial modes.
w_all  <- exp(-DIFF_T * vals[-1])
Dcent  <- sweep(vecs[, -1, drop = FALSE], 2, vecs[v0, -1], `-`)
Dcent  <- sweep(Dcent, 2, w_all, `*`)
diff_dist <- sqrt(rowSums(Dcent^2))          # replaces the old BFS level

if (EMBEDDING == "spectral") {
  pos0 <- vecs[, COORDS, drop = FALSE]
} else {
  # Radius = diffusion distance to the start; keep the 3 modes carrying the most
  # diffusion energy relative to v0 so the 3-D radius tracks that distance.
  energy <- colSums(Dcent^2)
  sel    <- order(energy, decreasing = TRUE)[1:3]
  pos0   <- Dcent[, sel, drop = FALSE]
  cat(sprintf("Diffusion map centred on start: t=%.1f\n", DIFF_T))
  cat(sprintf("  corr(3-D radius, full diffusion distance) = %.3f\n",
              cor(sqrt(rowSums(pos0^2)), diff_dist)))
}

rmax <- max(sqrt(rowSums(pos0^2)))
if (rmax > 0) pos0 <- pos0 / rmax * SCALE

dup <- sum(duplicated(round(pos0, 9)))
cat(sprintf("Nodes sharing a position: %d of %d\n", dup, nv))

pos <- pos0

# ------------------------------------------------------------------ RENDER

v <- cgv_viewer(1280, 720,
                sprintf("cayleyR: TopSpin(%d,%d) in %s coords (no BFS)",
                        N, K, EMBEDDING))
cgv_background(v, "black")

# Colour by diffusion distance from the start; size shrinks with it.
sz <- 4 + 10 * (1 - diff_dist / max(diff_dist))
cgv_set_graph(v, seq_len(nv), edges,
              positions   = pos,
              node_values = as.double(diff_dist),
              node_sizes  = as.double(sz))

# Start vertex in green.
cgv_highlight_path(v, v0, color = "#00FF66", node_scale = 3.0)

# Farthest-from-start nodes (by diffusion distance) in blue -- the BFS-free
# analogue of the diametral highlight.
if (SHOW_FAR > 0) {
  far <- order(diff_dist, decreasing = TRUE)[seq_len(SHOW_FAR)]
  for (fi in far) cgv_highlight_path(v, fi, color = "#3388FF", node_scale = 3.0)
  cat(sprintf("Highlighted %d farthest-from-start node(s) in blue\n", SHOW_FAR))
}

cm <- colMeans(pos)
dd <- max(sqrt(rowSums(sweep(pos, 2, cm)^2))) * 2
cgv_camera(v, position = cm + c(dd * 0.7, dd * 0.6, dd * 0.8), target = cm)

if (ORBIT) {
  # Auto-orbit around the cloud's centre. The camera target is already cm, so
  # the built-in orbit mode spins the view around the middle of the graph.
  cgv_camera_mode(v, "orbit")
  cat("Camera orbiting the centre. Scroll = zoom. Close window to exit.\n")
} else {
  cat("Right mouse drag = rotate, scroll = zoom. Close window to exit.\n")
}
cgv_run(v)
