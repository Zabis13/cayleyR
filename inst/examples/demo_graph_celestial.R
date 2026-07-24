#!/usr/bin/env Rscript
# Visualise the whole Cayley graph in its own celestial coordinates.
#
# Every reachable state is placed by the (theta, phi, omega) that
# cayley_bfs_full records along the BFS path that first reached it, colour is
# the BFS level, and the diameter pairs are highlighted. Two placements are
# available: the raw celestial embedding, or an FR layout seeded from it.
#
# Note the coordinates are a property of the path, not of the state: many
# states share a (theta, phi, omega) triple and land on the same point. The
# jitter below breaks that degeneracy so the layout and the octree can tell
# them apart -- if the picture looks like a handful of tight bundles rather
# than a graph, that collapse is what you are seeing.
#
# Run with:  Rscript inst/examples/demo_graph_celestial.R

library(cayleyR)
library(cgvR)

# ---------------------------------------------------------------- PARAMETERS

N <- 6                      # permutation size; graph has up to N! vertices
K <- 4                      # reverse-prefix length
MOVES <- c("L", "R", "X")   # allowed operations
START <- 1:N                # state to explore from

# "celestial" places nodes straight at their (theta, phi, omega), showing the
# coordinates as they are. "fr" runs a force layout seeded by them, which
# opens the bundles up into something readable as a graph.
LAYOUT <- "celestial"

PROJECTION <- "sphere"      # "sphere": omega as radius, theta/phi as angles
                            # "plane" : stereographic z = tan(theta/2)*e^(i*phi),
                            #           BFS level on the third axis

SHOW_PAIRS <- 3             # how many diametral pairs to highlight (0 = none)
JITTER <- 0.05              # nudge coincident nodes apart; 0 to disable
SCALE <- 10                 # overall spread of the initial positions
FR_ITER <- 200L             # layout iterations when LAYOUT = "fr"
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

# ---------------------------------------------------- CELESTIAL POSITIONS

theta <- bfs$theta; phi <- bfs$phi; omega <- bfs$omega

if (PROJECTION == "plane") {
  # Stereographic projection onto the complex plane, exactly the z that
  # convert_LRX_to_celestial returns; BFS level becomes the vertical axis.
  rad <- tan(theta / 2)
  pos0 <- cbind(rad * cos(phi), rad * sin(phi), bfs$dist)
} else {
  # Celestial sphere: omega is the radius, theta/phi the usual angles.
  pos0 <- cbind(omega * sin(theta) * cos(phi),
                omega * sin(theta) * sin(phi),
                omega * cos(theta))
}

rmax <- max(sqrt(rowSums(pos0^2)))
if (rmax > 0) pos0 <- pos0 / rmax * SCALE

# Distinct states routinely share a coordinate triple; without a nudge they sit
# on top of each other and the Barnes-Hut octree cannot separate them.
dup <- sum(duplicated(round(pos0, 9)))
cat(sprintf("Nodes sharing a position: %d of %d\n", dup, nv))
if (JITTER > 0) pos0 <- pos0 + matrix(rnorm(nv * 3, sd = JITTER), ncol = 3)

pos <- if (LAYOUT == "fr") {
  cgv_layout_fr_bh(nv, edges, n_iter = FR_ITER, init = pos0)
} else {
  pos0
}

# ------------------------------------------------------------------ RENDER

v <- cgv_viewer(1280, 720,
                sprintf("cayleyR: TopSpin(%d,%d) in celestial coords", N, K))
cgv_background(v, "black")

# Colour by BFS level, size shrinking with distance from the start.
sizes <- pmax(4, 14 - bfs$dist * 1.0)
cgv_set_graph(v, seq_len(nv), edges,
              positions   = pos,
              node_values = as.double(bfs$dist),
              node_sizes  = as.double(sizes))

# Start vertex in green.
cgv_highlight_path(v, 1L, color = "#00FF66", node_scale = 3.0)

# Diametral pairs: the two ends of the graph, which is what the whole
# exercise is about. Each pair is drawn as a two-node highlight.
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
