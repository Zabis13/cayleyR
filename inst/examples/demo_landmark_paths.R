#!/usr/bin/env Rscript
# Visualise the paths from the identity to ten structural landmark states.
#
# Unlike demo_graph_spectral.R this never enumerates the whole Cayley graph --
# at n = 12 that is 12! vertices. Only the states actually visited by the ten
# paths are kept, and they are laid out by the spectrum of the SUBGRAPH they
# induce: the path steps themselves plus every L/R/X edge that happens to run
# between two visited states. Because the layout comes from the eigenvectors of
# that subgraph's Laplacian, states that are genuinely close in the Cayley graph
# end up close in the picture, and the ten routes appear as distinct arms.
#
# LAYOUT switches between that diffusion map, the raw spectral embedding, and
# the package's own celestial coordinates.
#
# Each path is produced by human_algorithm() and then compressed by
# short_path_bfs(), the same pipeline used to solve a real puzzle.
#
# Run with:  Rscript inst/examples/demo_landmark_paths.R

library(cayleyR)
library(cgvR)

# ---------------------------------------------------------------- PARAMETERS

N <- 20L                    # permutation size (human_algorithm needs n >= k+6)
K <- 4L                     # reverse-prefix length
MOVES <- c("L", "R", "X")   # allowed operations
START <- seq_len(N)         # state every path starts from

DEPTH <- 5L                 # BFS depth used by short_path_bfs
ROUNDS <- 3L                # how many times to re-apply the shortener

# How the nodes are positioned.
#
# "diffusion": diffusion map centred on the start, so radius tracks distance
#              from the identity. Larger DIFF_T = coarser view.
# "spectral" : raw eigenvectors COORDS as coordinates. On the nearly cycle-free
#              path subgraph this unrolls the arms into straight rays.
# "celestial": the package's own coordinates -- the running (nL, nR, nX) counts
#              along a route map to (theta, phi, omega). A property of the PATH,
#              not of the state, so no eigen() runs and distinct states sharing
#              a triple land on one point (see JITTER).
#LAYOUT <- "diffusion"
#LAYOUT <- "spectral"
LAYOUT <- "celestial"

NORMALISED <- TRUE          # normalised Laplacian; "spectral"/"diffusion" only
DIFF_T <- 8                 # diffusion time; "diffusion" only
COORDS <- c(2, 3, 4)        # eigenvector columns; "spectral" only

# "sphere": omega is the radius and (theta, phi) the usual angles.
# "plane" : stereographic z = tan(theta/2) * e^(i*phi), with the step index as
#           the third axis -- the ten routes become rising helices.
PROJECTION <- "sphere"      # "celestial" only
JITTER <- 0.02              # nudge apart co-located nodes; "celestial" only

SCALE <- 10                 # overall spread of the positions
CROSS_EDGES <- TRUE         # also draw L/R/X edges between visited states that
                            # no path used (shows where the arms nearly touch)

# The paths alone induce an almost cycle-free subgraph, on which a diffusion map
# has nothing to diffuse along. NBHD grows the vertex set by every state within
# that many L/R/X moves of a path state, which puts the cycles back. Cost is
# roughly |paths| * 3^NBHD vertices, so 1 or 2 is the usable range; 0 keeps the
# bare paths (use LAYOUT="spectral" then).
NBHD <- 1L

SEED <- 1

# Colour per landmark, in the order returned by landmark_states(). Walks the hue
# circle so that neighbouring landmarks stay distinguishable at 25 of them.
PALETTE <- grDevices::hcl(
  h = seq(15, 375, length.out = 26)[1:25],
  c = 100, l = 68
)

# ----------------------------------------------------------------- HELPERS

shorten <- function(path, start_state, k, depth, rounds) {
  for (i in seq_len(rounds)) {
    before <- length(path)
    path <- short_path_bfs(path, start_state, k, depth)$path
    if (length(path) >= before) break
  }
  path
}

# Every state along a path, including both endpoints.
path_states <- function(start_state, path, k) {
  out <- vector("list", length(path) + 1L)
  out[[1L]] <- as.integer(start_state)
  cur <- as.integer(start_state)
  for (i in seq_along(path)) {
    cur <- as.integer(apply_operations(cur, path[i], k,
                                       compute_coords = FALSE)$state)
    out[[i + 1L]] <- cur
  }
  out
}

# --------------------------------------------------------------------- PATHS

set.seed(SEED)

lm <- landmark_states(N)
cat(sprintf("TopSpin(%d,%d), moves = %s\n", N, K, paste(MOVES, collapse = ",")))
cat(sprintf("Start: %s\n\n", paste(START, collapse = "_")))

# key -> node index, built as states are met; first path to reach a state owns
# its colour, later paths reuse the node.
idx <- new.env(hash = TRUE, parent = emptyenv())
node_key <- character(0)
node_state <- list()
node_owner <- integer(0)    # which landmark first visited this state
node_step <- integer(0)     # step along that landmark's path
node_lrx <- list()          # (nL, nR, nX) of the route that first got here

node_id <- function(state, owner, step, lrx) {
  key <- paste(state, collapse = "_")
  hit <- idx[[key]]
  if (!is.null(hit)) return(hit)
  i <- length(node_key) + 1L
  assign(key, i, envir = idx)
  node_key[i] <<- key
  node_state[[i]] <<- state
  node_owner[i] <<- owner
  node_step[i] <<- step
  node_lrx[[i]] <<- lrx
  i
}

ef <- integer(0)
et <- integer(0)
edge_owner <- integer(0)
target_node <- integer(nrow(lm))
path_len <- integer(nrow(lm))
raw_len <- integer(nrow(lm))
path_ok <- logical(nrow(lm))

# The identity is node 1, shared by every path (owner 0 = start).
invisible(node_id(as.integer(START), 0L, 0L, c(0L, 0L, 0L)))

for (i in seq_len(nrow(lm))) {
  target <- lm$state[[i]]
  t0 <- Sys.time()

  res <- human_algorithm(START, final_state = target, k = K)
  if (!isTRUE(res$found)) {
    cat(sprintf("  %-15s FAILED to solve\n", lm$name[i]))
    path_ok[i] <- FALSE
    path_len[i] <- NA_integer_
    raw_len[i] <- NA_integer_
    next
  }
  raw_len[i] <- res$length
  p <- shorten(res$path, START, K, DEPTH, ROUNDS)
  path_len[i] <- length(p)

  sts <- path_states(START, p, K)
  path_ok[i] <- identical(sts[[length(sts)]], target)

  # Running L/R/X counters: a celestial coordinate belongs to the route that
  # reached the state, not to the state itself, so it accumulates step by step.
  cnt <- c(L = 0L, R = 0L, X = 0L)
  ids <- integer(length(sts))
  ids[1L] <- node_id(sts[[1L]], i, 0L, unname(cnt))
  for (s in seq_along(p)) {
    op <- c("1" = "L", "2" = "R", "3" = "X")[[p[s]]]
    cnt[[op]] <- cnt[[op]] + 1L
    ids[s + 1L] <- node_id(sts[[s + 1L]], i, s, unname(cnt))
  }
  target_node[i] <- ids[length(ids)]

  ef <- c(ef, ids[-length(ids)])
  et <- c(et, ids[-1L])
  edge_owner <- c(edge_owner, rep(i, length(ids) - 1L))

  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %-15s %-24s raw %3d -> %3d ops  (%.2fs)%s\n",
              lm$name[i], lm$state_str[i], raw_len[i], path_len[i], el,
              ifelse(path_ok[i], "", "  [DID NOT REACH TARGET]")))
}

n_path_nodes <- length(node_key)
cat(sprintf("\nDistinct states on the paths: %d (paths total %d steps)\n",
            n_path_nodes, sum(path_len, na.rm = TRUE)))

# ------------------------------------------------------ NEIGHBOURHOOD GROWTH

# Add every state within NBHD moves of a path state. These carry owner -1, are
# drawn small, and give the arms the width a spectral layout needs -- without
# them the subgraph is a tree and the eigenvectors unroll it into straight rays.
if (NBHD > 0L) {
  frontier <- seq_len(n_path_nodes)
  for (r in seq_len(NBHD)) {
    nxt <- integer(0)
    for (v in frontier) {
      st <- node_state[[v]]
      lrx <- node_lrx[[v]]
      for (oi in seq_along(MOVES)) {
        op <- MOVES[oi]
        child <- as.integer(apply_operations(st, op, K,
                                             compute_coords = FALSE)$state)
        if (!is.null(idx[[paste(child, collapse = "_")]])) next
        clrx <- lrx
        clrx[oi] <- clrx[oi] + 1L
        nxt <- c(nxt, node_id(child, -1L, node_step[v] + 1L, clrx))
      }
    }
    frontier <- unique(nxt)
    cat(sprintf("  neighbourhood ring %d: +%d states\n", r, length(frontier)))
  }
}

nv <- length(node_key)
cat(sprintf("Vertices in the induced subgraph: %d\n", nv))

S <- do.call(rbind, node_state)

# Steps from the start along the owning path.
dist_from_start <- node_step
dist_from_start[1L] <- 0L

# ------------------------------------------------- EXTRA EDGES INSIDE THE SET

# L/R/X edges between two visited states that no path happened to use. They cost
# one apply_operations call per (state, move) and reveal how close the arms run.
if (CROSS_EDGES || NBHD > 0L) {
  cf <- integer(0); ct <- integer(0)
  for (v in seq_len(nv)) {
    for (op in MOVES) {
      child <- apply_operations(S[v, ], op, K, compute_coords = FALSE)$state
      hit <- idx[[paste(child, collapse = "_")]]
      if (!is.null(hit)) { cf <- c(cf, v); ct <- c(ct, hit) }
    }
  }
  ef <- c(ef, cf); et <- c(et, ct)
  edge_owner <- c(edge_owner, rep(0L, length(cf)))
}

ekey <- ifelse(ef < et, paste(ef, et), paste(et, ef))
keep <- !duplicated(ekey) & ef != et
edges <- cbind(ef[keep], et[keep])
cat(sprintf("Edges in the induced subgraph: %d\n", nrow(edges)))

# ------------------------------------------------------------- POSITIONS

if (LAYOUT == "celestial") {
  # Every node carries the (nL, nR, nX) counts of the route that first reached
  # it. convert_LRX_to_celestial() turns that triple into (theta, phi, omega):
  # theta is the zenith angle from the X axis, phi the azimuth in the LR plane,
  # omega the conformal energy, i.e. the length of the move vector.
  LRX <- do.call(rbind, node_lrx)
  cel <- lapply(seq_len(nv), function(i)
    convert_LRX_to_celestial(LRX[i, 1], LRX[i, 2], LRX[i, 3]))

  theta <- vapply(cel, function(z) z$theta, numeric(1))
  phi <- vapply(cel, function(z) z$phi, numeric(1))
  omega <- vapply(cel, function(z) z$omega_conformal, numeric(1))

  if (PROJECTION == "plane") {
    rad <- tan(theta / 2)
    pos0 <- cbind(rad * cos(phi), rad * sin(phi), dist_from_start)
  } else {
    pos0 <- cbind(omega * sin(theta) * cos(phi),
                  omega * sin(theta) * sin(phi),
                  omega * cos(theta))
  }
  cat(sprintf("Celestial coordinates, %s projection\n", PROJECTION))

} else {
  A <- matrix(0, nv, nv)
  A[edges] <- 1
  A[edges[, 2:1]] <- 1
  deg <- rowSums(A)
  if (any(deg == 0)) stop("isolated node in the induced subgraph")

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

  if (LAYOUT == "spectral") {
    pos0 <- vecs[, COORDS, drop = FALSE]
  } else {
    v0 <- 1L                                            # the identity
    w <- exp(-DIFF_T * vals[-1])
    D <- sweep(vecs[, -1, drop = FALSE], 2, vecs[v0, -1], `-`)
    D <- sweep(D, 2, w, `*`)
    energy <- colSums(D^2)
    sel <- order(energy, decreasing = TRUE)[1:3]
    pos0 <- D[, sel, drop = FALSE]
    cat(sprintf("Diffusion map centred on the identity: t=%.1f\n", DIFF_T))
    cat(sprintf("  corr(radius, steps from start) = %.3f\n",
                stats::cor(sqrt(rowSums(pos0^2)), dist_from_start)))
  }
}

# Centre the cloud on the origin. Every layout here places the identity at or
# near (0,0,0) and lets the paths run off in one direction, so without this the
# cloud sits off to one side and orbiting swings it around one of its own ends.
pos0 <- sweep(pos0, 2, (apply(pos0, 2, min) + apply(pos0, 2, max)) / 2)

rmax <- max(sqrt(rowSums(pos0^2)))
if (rmax > 0) pos0 <- pos0 / rmax * SCALE

# Celestial coordinates come from move COUNTS, so distinct states can share a
# triple and land on one point; the spectral layouts give each node its own.
dup <- sum(duplicated(round(pos0, 9)))
cat(sprintf("Nodes sharing a position: %d of %d\n", dup, nv))
if (LAYOUT == "celestial" && JITTER > 0) {
  pos0 <- pos0 + matrix(rnorm(nv * 3, sd = JITTER), ncol = 3)
}

pos <- pos0

# ------------------------------------------------------------------ RENDER

v <- cgv_viewer(1280, 720,
                sprintf("cayleyR: %d landmark paths, TopSpin(%d,%d)",
                        nrow(lm), N, K))
cgv_background(v, "black")

# Path states large, neighbourhood padding small.
sizes <- ifelse(node_owner >= 0L, pmax(5, 12 - dist_from_start * 0.15), 2)
cgv_set_graph(v, seq_len(nv), edges,
              positions   = pos,
              node_values = as.double(node_owner),
              node_sizes  = as.double(sizes))

# Identity in green, each landmark target in its own colour.
cgv_highlight_path(v, 1L, color = "#00FF66", node_scale = 4.0)
for (i in seq_len(nrow(lm))) {
  if (target_node[i] == 0L) next
  cgv_highlight_path(v, target_node[i],
                     color = PALETTE[(i - 1L) %% length(PALETTE) + 1L],
                     node_scale = 3.0)
}

cat("\n--- Legend ---\n")
cat(sprintf("  %-15s %s\n", "start (identity)", "#00FF66"))
for (i in seq_len(nrow(lm))) {
  cat(sprintf("  %-15s %s   %3s ops   %s\n",
              lm$name[i],
              PALETTE[(i - 1L) %% length(PALETTE) + 1L],
              ifelse(is.na(path_len[i]), "NA", path_len[i]),
              lm$state_str[i]))
}

dd <- max(sqrt(rowSums(pos^2))) * 2
cgv_camera(v, position = c(dd * 0.7, dd * 0.6, dd * 0.8), target = c(0, 0, 0))
cgv_camera_mode(v, "orbit")   # mouse turns around the centre instead of flying

cat("\nMouse drag = orbit around the centre, scroll = zoom. Close to exit.\n")
cgv_run(v)
