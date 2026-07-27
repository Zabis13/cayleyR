#!/usr/bin/env Rscript
# The network BETWEEN the landmark states, rather than the star from the
# identity out to each of them (that is demo_landmark_paths.R).
#
# Step 1 measures every one of the 25*24/2 = 300 pairwise distances with
# human_algorithm_to() + short_path_bfs(). Note human_algorithm() is the wrong
# tool here: it reaches an arbitrary target by solving BOTH endpoints down to
# 1:n and splicing the words, so every route detours through the identity and
# all the arcs pile up in the centre of the picture. human_algorithm_to()
# relabels the problem instead and goes direct, which at n = 20 also happens to
# cut the paths to roughly a third of their length.
#
# Step 2 thins the 25 landmarks down to KEEP of them by farthest-point sampling:
# start from the most distant pair, then repeatedly add whichever landmark is
# farthest from everything already chosen. That maximises the SMALLEST distance
# inside the kept set, so no two survivors sit close together -- they end up on
# opposite sides of the graph.
#
# Step 3 draws the complete network among the survivors: every path between
# every kept pair, each in the colour of the pair's lower-numbered endpoint.
#
# Run with:  Rscript inst/examples/demo_landmark_network.R

library(cayleyR)
library(cgvR)

# ---------------------------------------------------------------- PARAMETERS

N <- 50L                    # permutation size (human_algorithm needs n >= k+6)
K <- 4L                     # reverse-prefix length
MOVES <- c("L", "R", "X")   # allowed operations
START <- seq_len(N)         # the identity, drawn as a reference point

DEPTH <- 5L                 # BFS depth used by short_path_bfs
ROUNDS <- 3L                # how many times to re-apply the shortener

# "fixed" : use the pairs listed in FIXED_PAIRS verbatim.
# "maxmin": keep KEEP landmarks spread as widely as possible (farthest-point
#           sampling), then draw every path among them.
# "pairs" : search for NPAIRS disjoint PAIRS of landmarks -- 2*NPAIRS points,
#           each used exactly once -- maximising the total pair distance.
SELECT <- "fixed"
KEEP <- 25L                 # "maxmin" only: how many landmarks to keep
NPAIRS <- 6L                # "pairs" only: how many disjoint pairs
PAIR_RESTARTS <- 4000L      # "pairs" only: local-search restarts

# The six disjoint pairs found by that search at n = 20 (total 1574, shortest
# pair 233). Kept as literals so the picture is reproducible: the search is a
# local one, so a different SEED or PAIR_RESTARTS can land on a different set.
# Re-run with SELECT = "pairs" to search again after changing N or the path
# solver -- these numbers are specific to human_algorithm_to().
FIXED_PAIRS <- rbind(
  c("reverse_first",  "pair_shift"),           # 290
  c("cycles3",        "block_reverse_pairs"),  # 286
  c("alt_pairs",      "full_reverse"),         # 270
  c("block_rotate3",  "zigzag"),               # 250
  c("spiral",         "adjacent_swaps"),       # 245
  c("reverse_second", "two_cycles")            # 233
)

# "pairs"/"fixed" only: draw only the paths inside the pairs (TRUE), or every
# path among all selected landmarks (FALSE).
PAIRS_ONLY <- TRUE

SHOW_START <- FALSE          # also draw the paths from the identity to each

# TRUE: draw the convex hull the landmarks span and nothing else -- just its
#       corners and edges. The paths are still solved, since that is what places
#       each landmark, but the states along them are not shown.
# FALSE: draw the paths themselves.
# Area and volume of the hull are reported either way.
SHOW_HULL <- TRUE

# TRUE: every landmark is a corner of the figure -- the convex hull is dented
#       inwards to reach the ones that would otherwise sit inside it.
# FALSE: plain convex hull, so interior landmarks are not on the surface.
HULL_THROUGH_ALL <- TRUE

# Edges inherit the colour of the nodes they join, so these two set the whole
# wireframe as well as the points.
HULL_CORNER_COLOR <- "#FFD400"  # landmarks that are corners of the hull
HULL_INNER_COLOR <- "#4A5568"   # landmarks that fell inside it

# Layout, exactly as in demo_landmark_paths.R.
# "celestial": (nL, nR, nX) counts of the route mapped to (theta, phi, omega).
# "diffusion": diffusion map on the induced subgraph, centred on the identity.
# "spectral" : raw eigenvectors of that subgraph's Laplacian.
LAYOUT <- "celestial"

NORMALISED <- TRUE          # normalised Laplacian; "spectral"/"diffusion" only
DIFF_T <- 8                 # diffusion time; "diffusion" only
COORDS <- c(2, 3, 4)        # eigenvector columns; "spectral" only
PROJECTION <- "sphere"      # "sphere" or "plane"; "celestial" only
JITTER <- 0.02              # nudge apart co-located nodes; "celestial" only

SCALE <- 10                 # overall spread of the positions
NBHD <- 0L                  # neighbourhood padding; 0 keeps the bare paths
SEED <- 1

DIST_CSV <- "/tmp/landmark_dist_n20.csv"              # write the 25x25 matrix here, "" = do not write

# ----------------------------------------------------------------- HELPERS

shorten <- function(path, start_state, k, depth, rounds) {
  for (i in seq_len(rounds)) {
    before <- length(path)
    path <- short_path_bfs(path, start_state, k, depth)$path
    if (length(path) >= before) break
  }
  path
}

# The shortened path from one state to another, or NULL if none was found.
# human_algorithm_to() relabels the problem instead of solving both endpoints
# down to 1:n and splicing the words together, so the route does not detour
# through the identity -- which is what made every arc converge on the centre.
solve_pair <- function(from, to, k, depth, rounds) {
  res <- human_algorithm_to(from, to, k = k)
  if (!isTRUE(res$found)) return(NULL)
  shorten(res$path, from, k, depth, rounds)
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

# ------------------------------------------------- STEP 1: DISTANCE MATRIX

set.seed(SEED)

lm <- landmark_states(N)
nl <- nrow(lm)
cat(sprintf("TopSpin(%d,%d), moves = %s\n", N, K, paste(MOVES, collapse = ",")))

DM <- matrix(NA_real_, nl, nl, dimnames = list(lm$name, lm$name))
diag(DM) <- 0
pair_path <- vector("list", nl * nl)   # indexed as (i - 1) * nl + j

# With SELECT = "fixed" the pairs are already known, so only those are solved --
# measuring all 300 to report six of them is wasted work. The searching modes do
# need the whole matrix.
if (SELECT == "fixed") {
  want <- matrix(match(FIXED_PAIRS, lm$name), ncol = 2L)
  if (anyNA(want)) {
    stop("FIXED_PAIRS names not found in landmark_states(", N, "): ",
         paste(FIXED_PAIRS[is.na(want)], collapse = ", "))
  }
  todo_pairs <- lapply(seq_len(nrow(want)),
                       function(p) sort(c(want[p, 1], want[p, 2])))
} else {
  todo_pairs <- list()
  for (i in seq_len(nl - 1L)) for (j in (i + 1L):nl) {
    todo_pairs[[length(todo_pairs) + 1L]] <- c(i, j)
  }
}

cat(sprintf("%d landmarks, %d pairs to measure\n\n", nl, length(todo_pairs)))

t0 <- Sys.time()
for (t in todo_pairs) {
  i <- t[1L]; j <- t[2L]
  p <- solve_pair(lm$state[[i]], lm$state[[j]], K, DEPTH, ROUNDS)
  if (is.null(p)) next
  DM[i, j] <- DM[j, i] <- length(p)
  pair_path[[(i - 1L) * nl + j]] <- p
}
cat(sprintf("%d pairs measured in %.1fs\n", length(todo_pairs),
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))

measured <- DM[upper.tri(DM)]
measured <- measured[!is.na(measured)]
if (length(todo_pairs) > length(measured)) {
  cat(sprintf("WARNING: %d pairs unsolved\n",
              length(todo_pairs) - length(measured)))
}

# Only meaningful when every pair was measured.
if (SELECT != "fixed") {
  cat(sprintf("Distances: min %g, median %g, mean %.1f, max %g\n",
              min(measured), stats::median(measured), mean(measured),
              max(measured)))
  if (nzchar(DIST_CSV)) {
    utils::write.csv(DM, DIST_CSV)
    cat("Matrix written to", DIST_CSV, "\n")
  }
}

# --------------------------------------------------------- STEP 2: SELECTION

matched <- NULL      # set by "pairs"/"fixed": 2-column matrix of paired indices

if (SELECT == "fixed") {
  matched <- want          # already resolved and checked in step 1
  if (anyDuplicated(as.vector(matched))) {
    stop("FIXED_PAIRS reuses a landmark; the pairs must be disjoint")
  }
  sel <- sort(as.vector(matched))

  tot <- sum(vapply(seq_len(nrow(matched)),
                    function(p) DM[matched[p, 1], matched[p, 2]], 0))
  cat(sprintf("\n--- %d fixed pairs, %d landmarks (total %g) ---\n",
              nrow(matched), length(sel), tot))
  ord <- order(vapply(seq_len(nrow(matched)),
                      function(p) DM[matched[p, 1], matched[p, 2]], 0),
               decreasing = TRUE)
  for (p in ord) {
    cat(sprintf("  %-20s <-> %-20s %4g\n",
                lm$name[matched[p, 1]], lm$name[matched[p, 2]],
                DM[matched[p, 1], matched[p, 2]]))
  }

} else if (SELECT == "pairs") {
  # NPAIRS disjoint pairs, chosen to maximise the total of their distances.
  # Note this is NOT the same as taking the NPAIRS longest rows of the matrix:
  # the two ends of the graph appear in nearly every long pair, so greedily
  # taking the longest ones exhausts them at once. Maximising the total instead
  # spreads the endpoints out. Exact maximum-weight matching is overkill here,
  # so this is a local search: swap partners between pairs and substitute unused
  # landmarks until nothing improves, restarted from many random matchings.
  npair <- NPAIRS
  best <- NULL
  best_total <- -Inf
  set.seed(SEED)

  for (restart in seq_len(PAIR_RESTARTS)) {
    avail <- rep(TRUE, nl)
    P <- matrix(0L, npair, 2)
    for (p in seq_len(npair)) {
      if (restart == 1L) {                       # first restart: greedy seed
        M <- DM
        M[!avail, ] <- -1
        M[, !avail] <- -1
        diag(M) <- -1
        ij <- which(M == max(M, na.rm = TRUE), arr.ind = TRUE)[1, ]
        i <- ij[["row"]]; j <- ij[["col"]]
      } else {                                   # later restarts: random seed
        s <- sample(which(avail), 2L)
        i <- s[1L]; j <- s[2L]
      }
      P[p, ] <- c(i, j)
      avail[c(i, j)] <- FALSE
    }

    repeat {
      improved <- FALSE
      # swap partners between two pairs if that lengthens both
      for (a in seq_len(npair)) for (b in seq_len(npair)) {
        if (b <= a) next
        cur <- DM[P[a, 1], P[a, 2]] + DM[P[b, 1], P[b, 2]]
        alt1 <- DM[P[a, 1], P[b, 1]] + DM[P[a, 2], P[b, 2]]
        alt2 <- DM[P[a, 1], P[b, 2]] + DM[P[a, 2], P[b, 1]]
        if (alt1 > cur && alt1 >= alt2) {
          tmp <- P[a, 2]; P[a, 2] <- P[b, 1]; P[b, 1] <- tmp
          improved <- TRUE
        } else if (alt2 > cur) {
          tmp <- P[a, 2]; P[a, 2] <- P[b, 2]; P[b, 2] <- tmp
          improved <- TRUE
        }
      }
      # bring in an unused landmark wherever it beats the current endpoint
      rest <- setdiff(seq_len(nl), as.vector(P))
      for (a in seq_len(npair)) for (k in 1:2) for (r in rest) {
        Q <- P[a, ]
        Q[k] <- r
        if (DM[Q[1], Q[2]] > DM[P[a, 1], P[a, 2]]) {
          P[a, ] <- Q
          rest <- setdiff(seq_len(nl), as.vector(P))
          improved <- TRUE
        }
      }
      if (!improved) break
    }

    total <- sum(vapply(seq_len(npair), function(p) DM[P[p, 1], P[p, 2]], 0))
    if (total > best_total) { best_total <- total; best <- P }
  }

  matched <- best
  sel <- sort(as.vector(best))

  cat(sprintf("\n--- %d disjoint pairs, %d landmarks (total %g) ---\n",
              npair, length(sel), best_total))
  ord <- order(vapply(seq_len(npair), function(p) DM[best[p, 1], best[p, 2]], 0),
               decreasing = TRUE)
  for (p in ord) {
    cat(sprintf("  %-20s <-> %-20s %4g\n",
                lm$name[best[p, 1]], lm$name[best[p, 2]],
                DM[best[p, 1], best[p, 2]]))
  }

} else {
  # Farthest-point sampling. Seed with the most distant pair, then keep adding
  # the landmark whose closest already-chosen neighbour is as far away as
  # possible.
  far <- which(DM == max(DM, na.rm = TRUE), arr.ind = TRUE)[1, ]
  sel <- c(far[["row"]], far[["col"]])

  while (length(sel) < KEEP) {
    rest <- setdiff(seq_len(nl), sel)
    # distance from each remaining landmark to its nearest chosen one
    d_near <- vapply(rest, function(r) min(DM[r, sel], na.rm = TRUE), numeric(1))
    sel <- c(sel, rest[which.max(d_near)])
  }
  sel <- sort(sel)

  sub <- DM[sel, sel]
  sub_off <- sub[upper.tri(sub)]
  cat(sprintf("\n--- Kept %d of %d landmarks (max-min) ---\n", KEEP, nl))
  cat(sprintf("Smallest distance inside the kept set: %g (was %g over all)\n",
              min(sub_off, na.rm = TRUE), min(off, na.rm = TRUE)))
  cat(sprintf("Mean distance inside the kept set:     %.1f (was %.1f)\n",
              mean(sub_off, na.rm = TRUE), mean(off, na.rm = TRUE)))
  for (s in sel) {
    cat(sprintf("  %-20s nearest kept neighbour %g\n",
                lm$name[s], min(DM[s, setdiff(sel, s)], na.rm = TRUE)))
  }
}

# The landmarks that did NOT make it, and what crowded them out. Needs the full
# matrix, so it is skipped when only the fixed pairs were measured.
dropped <- if (SELECT == "fixed") integer(0) else setdiff(seq_len(nl), sel)
if (length(dropped)) {
  cat("\nDropped:\n")
  for (d in dropped) {
    nearest <- sel[which.min(DM[d, sel])]
    cat(sprintf("  %-20s nearest kept %s (%g)\n",
                lm$name[d], lm$name[nearest], min(DM[d, sel], na.rm = TRUE)))
  }
}

# ---------------------------------------------------- STEP 3: BUILD GRAPH

idx <- new.env(hash = TRUE, parent = emptyenv())
node_key <- character(0)
node_state <- list()
node_owner <- integer(0)    # which kept landmark's path first reached this
node_step <- integer(0)
node_lrx <- list()

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

# Walk a path, registering every state and edge along it. `base` is the L/R/X
# count already accumulated at `from`, so an arc continues its start landmark's
# counter instead of restarting at the origin.
add_path <- function(from, path, owner, base = c(0L, 0L, 0L), step0 = 0L) {
  sts <- path_states(from, path, K)
  cnt <- stats::setNames(as.integer(base), c("L", "R", "X"))
  ids <- integer(length(sts))
  ids[1L] <- node_id(sts[[1L]], owner, step0, unname(cnt))
  for (s in seq_along(path)) {
    op <- c("1" = "L", "2" = "R", "3" = "X")[[path[s]]]
    cnt[[op]] <- cnt[[op]] + 1L
    ids[s + 1L] <- node_id(sts[[s + 1L]], owner, step0 + s, unname(cnt))
  }
  ef <<- c(ef, ids[-length(ids)])
  et <<- c(et, ids[-1L])
  ids
}

# --- Absolute position of each kept landmark -------------------------------
#
# A celestial coordinate is a property of the ROUTE that reached a state, not of
# the state, so an arc drawn on its own would start at the origin no matter
# which landmark it leaves from -- which is why every arc used to pile up in one
# spot. Solving the identity -> landmark path first gives each landmark an
# absolute (nL, nR, nX), and the arcs then continue from those counters.
# These paths are only measured, never drawn.
base_lrx <- matrix(0L, nl, 3)
base_len <- integer(nl)
for (s in seq_along(sel)) {
  i <- sel[s]
  p <- solve_pair(START, lm$state[[i]], K, DEPTH, ROUNDS)
  if (is.null(p)) next
  ops <- c("1" = "L", "2" = "R", "3" = "X")[p]
  base_lrx[i, ] <- c(sum(ops == "L"), sum(ops == "R"), sum(ops == "X"))
  base_len[i] <- length(p)
}
cat("\n--- Landmark positions, from the identity ---\n")
for (s in seq_along(sel)) {
  i <- sel[s]
  cat(sprintf("  %-20s L=%3d R=%3d X=%3d  (%d ops)\n",
              lm$name[i], base_lrx[i, 1], base_lrx[i, 2], base_lrx[i, 3],
              base_len[i]))
}


landmark_node <- integer(nl)

# The identity is only registered when its own rays are drawn; otherwise it
# would sit in the graph as an isolated vertex.
if (SHOW_START) {
  invisible(node_id(as.integer(START), 0L, 0L, c(0L, 0L, 0L)))
  for (s in seq_along(sel)) {
    i <- sel[s]
    p <- solve_pair(START, lm$state[[i]], K, DEPTH, ROUNDS)
    if (is.null(p)) next
    ids <- add_path(START, p, s)
    landmark_node[i] <- ids[length(ids)]
  }
}

# Which landmark pairs get a path drawn between them.
if (!is.null(matched) && PAIRS_ONLY) {
  todo <- lapply(seq_len(nrow(matched)), function(p) matched[p, ])
} else {
  todo <- list()
  for (a in seq_along(sel)) for (b in seq_along(sel)) {
    if (b > a) todo[[length(todo) + 1L]] <- c(sel[a], sel[b])
  }
}

n_pairs_drawn <- 0L
for (t in todo) {
  i <- t[1L]; j <- t[2L]
  p <- pair_path[[(min(i, j) - 1L) * nl + max(i, j)]]
  if (is.null(p)) next
  lo <- min(i, j); hi_i <- max(i, j)
  ids <- add_path(lm$state[[lo]], p, match(lo, sel),
                  base = base_lrx[lo, ], step0 = base_len[lo])
  n_pairs_drawn <- n_pairs_drawn + 1L
  if (landmark_node[lo] == 0L) landmark_node[lo] <- ids[1L]
  if (landmark_node[hi_i] == 0L) landmark_node[hi_i] <- ids[length(ids)]
}

n_path_nodes <- length(node_key)
cat(sprintf("\nPaths drawn: %d between kept landmarks%s\n", n_pairs_drawn,
            if (SHOW_START) sprintf(" plus %d from the identity", length(sel))
            else ""))
cat(sprintf("Distinct states: %d\n", n_path_nodes))

# --- The solid the landmarks span ------------------------------------------
#
# The kept landmarks are an arbitrary cloud, not a regular polyhedron, so the
# figure they bound is the convex hull: the smallest convex body containing all
# of them. Points that end up strictly inside are not corners of it, so the hull
# can have fewer vertices than there are landmarks.
#
# The coordinates used are the ones the landmarks actually occupy in the
# picture. For the point that starts an arc that is its distance from the
# identity; for the point that ends one it is the counter accumulated along the
# arc, which is a longer route to the same state. So this measures the solid as
# drawn, not an invariant of the graph.
lm_xyz <- t(vapply(sel, function(i) {
  nd <- landmark_node[i]
  if (nd == 0L) return(c(NA_real_, NA_real_, NA_real_))
  q <- node_lrx[[nd]]
  cel <- convert_LRX_to_celestial(q[1], q[2], q[3])
  c(cel$omega_conformal * sin(cel$theta) * cos(cel$phi),
    cel$omega_conformal * sin(cel$theta) * sin(cel$phi),
    cel$omega_conformal * cos(cel$theta))
}, numeric(3)))
rownames(lm_xyz) <- lm$name[sel]
lm_xyz <- lm_xyz[stats::complete.cases(lm_xyz), , drop = FALSE]

# A convex hull would leave the landmarks that fall inside it off the surface;
# enclosing_hull_3d() dents it inwards until every one of them is a corner, so
# the figure really does pass through all the points.
hull <- if (HULL_THROUGH_ALL) enclosing_hull_3d(lm_xyz) else convex_hull_3d(lm_xyz)
cat(sprintf("\n--- %s hull of the landmarks ---\n",
            if (HULL_THROUGH_ALL) "Enclosing" else "Convex"))
if (hull$degenerate) {
  cat("  degenerate: the landmarks are coplanar, so the solid has no volume\n")
  hull_edges <- matrix(integer(0), 0L, 2L)
  hull_corner_node <- integer(0)
} else {
  cat(sprintf("  vertices: %d of %d landmarks   faces: %d (triangulated)\n",
              length(hull$vertices), nrow(lm_xyz), nrow(hull$faces)))
  cat(sprintf("  surface area: %.2f\n", hull$area))
  cat(sprintf("  volume:       %.2f\n", hull$volume))
  inside <- setdiff(seq_len(nrow(lm_xyz)), hull$vertices)
  if (length(inside)) {
    cat(sprintf("  inside the hull, not corners: %s\n",
                paste(rownames(lm_xyz)[inside], collapse = ", ")))
  }

  # Unique edges of the triangulation, mapped back to graph node ids so they
  # can be drawn alongside the arcs.
  he <- rbind(hull$faces[, c(1, 2)], hull$faces[, c(2, 3)], hull$faces[, c(3, 1)])
  hkey <- paste(pmin(he[, 1], he[, 2]), pmax(he[, 1], he[, 2]))
  he <- he[!duplicated(hkey), , drop = FALSE]
  node_of <- landmark_node[sel][match(rownames(lm_xyz), lm$name[sel])]
  hull_edges <- cbind(node_of[he[, 1]], node_of[he[, 2]])
  hull_corner_node <- node_of[hull$vertices]
  cat(sprintf("  edges drawn: %d\n", nrow(hull_edges)))
}

if (NBHD > 0L) {
  frontier <- seq_len(n_path_nodes)
  for (r in seq_len(NBHD)) {
    nxt <- integer(0)
    for (v in frontier) {
      st <- node_state[[v]]
      lrx <- node_lrx[[v]]
      for (oi in seq_along(MOVES)) {
        child <- as.integer(apply_operations(st, MOVES[oi], K,
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
S <- do.call(rbind, node_state)
dist_from_start <- node_step
dist_from_start[1L] <- 0L

# L/R/X edges between two visited states that no path happened to use.
cf <- integer(0); ct <- integer(0)
for (v in seq_len(nv)) {
  for (op in MOVES) {
    child <- apply_operations(S[v, ], op, K, compute_coords = FALSE)$state
    hit <- idx[[paste(child, collapse = "_")]]
    if (!is.null(hit)) { cf <- c(cf, v); ct <- c(ct, hit) }
  }
}
ef <- c(ef, cf); et <- c(et, ct)

if (SHOW_HULL) {
  # Only the solid itself: the landmarks and the edges between them. The paths
  # are still solved -- they are what gives each landmark its position -- but
  # the states along them are dropped so the wireframe stands on its own.
  keep_n <- landmark_node[sel]
  keep_n <- keep_n[keep_n > 0L]
  remap <- integer(nv)
  remap[keep_n] <- seq_along(keep_n)

  edges <- cbind(remap[hull_edges[, 1]], remap[hull_edges[, 2]])
  node_state <- node_state[keep_n]
  node_owner <- node_owner[keep_n]
  node_step <- node_step[keep_n]
  node_lrx <- node_lrx[keep_n]
  landmark_node[sel] <- ifelse(landmark_node[sel] > 0L,
                               remap[landmark_node[sel]], 0L)
  hull_corner_node <- remap[hull_corner_node]
  nv <- length(keep_n)
  S <- do.call(rbind, node_state)
  dist_from_start <- node_step

  cat(sprintf("Hull only: %d vertices, %d edges\n", nv, nrow(edges)))

} else {
  ekey <- ifelse(ef < et, paste(ef, et), paste(et, ef))
  keep_e <- !duplicated(ekey) & ef != et
  edges <- cbind(ef[keep_e], et[keep_e])
  cat(sprintf("Vertices: %d   edges: %d\n", nv, nrow(edges)))
}

# ------------------------------------------------------------- POSITIONS

if (LAYOUT == "celestial") {
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
    w <- exp(-DIFF_T * vals[-1])
    D <- sweep(vecs[, -1, drop = FALSE], 2, vecs[1L, -1], `-`)
    D <- sweep(D, 2, w, `*`)
    sel3 <- order(colSums(D^2), decreasing = TRUE)[1:3]
    pos0 <- D[, sel3, drop = FALSE]
    cat(sprintf("Diffusion map, t=%.1f\n", DIFF_T))
  }
}

pos0 <- sweep(pos0, 2, (apply(pos0, 2, min) + apply(pos0, 2, max)) / 2)
rmax <- max(sqrt(rowSums(pos0^2)))
if (rmax > 0) pos0 <- pos0 / rmax * SCALE

dup <- sum(duplicated(round(pos0, 9)))
cat(sprintf("Nodes sharing a position: %d of %d\n", dup, nv))
if (LAYOUT == "celestial" && JITTER > 0) {
  pos0 <- pos0 + matrix(rnorm(nv * 3, sd = JITTER), ncol = 3)
}
pos <- pos0

# ------------------------------------------------------------------ RENDER

ns <- length(sel)
PALETTE <- grDevices::hcl(h = seq(15, 375, length.out = ns + 1L)[seq_len(ns)],
                          c = 100, l = 68)

v <- cgv_viewer(1280, 720,
                sprintf("cayleyR: %d landmarks, %d paths, TopSpin(%d,%d)",
                        ns, n_pairs_drawn, N, K))
cgv_background(v, "black")

if (SHOW_HULL) {
  # Showing the solid, so what matters is which landmarks are corners of it and
  # which fell inside -- the pairing that produced them is no longer the point.
  is_corner <- seq_len(nv) %in% hull_corner_node
  # Kept small: oversized spheres swallow the wireframe they sit on.
  sizes <- ifelse(is_corner, 6, 4)
} else {
  sizes <- ifelse(node_owner >= 0L, pmax(4, 10 - dist_from_start * 0.1), 2)
}

if (SHOW_HULL) {
  # Edges take their colour from the nodes they join, so setting the node
  # colours explicitly is what actually recolours the wireframe.
  rgba <- function(hex) c(grDevices::col2rgb(hex)[, 1], 255)
  node_colors <- t(vapply(is_corner,
                          function(cn) rgba(if (cn) HULL_CORNER_COLOR
                                            else HULL_INNER_COLOR),
                          numeric(4)))
  cgv_set_graph(v, seq_len(nv), edges,
                positions   = pos,
                node_colors = matrix(as.integer(node_colors), ncol = 4L),
                node_sizes  = as.double(sizes))
} else {
  cgv_set_graph(v, seq_len(nv), edges,
                positions   = pos,
                node_values = as.double(node_owner),
                node_sizes  = as.double(sizes))
}

# In hull mode nothing is highlighted: cgv_highlight_path() keeps only the last
# path it was given, so calling it once per edge lit a single one and left the
# rest untouched. The wireframe takes its colour from node_colors above instead.
if (!SHOW_HULL) {
  if (SHOW_START) cgv_highlight_path(v, 1L, color = "#00FF66", node_scale = 4.0)
  for (s in seq_along(sel)) {
    nd <- landmark_node[sel[s]]
    if (nd == 0L) next
    cgv_highlight_path(v, nd, color = PALETTE[s], node_scale = 3.5)
  }
}

cat("\n--- Kept landmarks ---\n")
if (SHOW_HULL) {
  cat(sprintf("  corner of the hull: %s   inside it: %s\n",
              HULL_CORNER_COLOR, HULL_INNER_COLOR))
  for (s in seq_along(sel)) {
    nd <- landmark_node[sel[s]]
    corner <- nd > 0L && is_corner[nd]
    cat(sprintf("  %-20s %-7s  %s\n", lm$name[sel[s]],
                if (corner) "corner" else "inside", lm$state_str[sel[s]]))
  }
} else {
  if (SHOW_START) cat(sprintf("  %-20s %s\n", "start (identity)", "#00FF66"))
  for (s in seq_along(sel)) {
    cat(sprintf("  %-20s %s   %s\n", lm$name[sel[s]], PALETTE[s],
                lm$state_str[sel[s]]))
  }
}

dd <- max(sqrt(rowSums(pos^2))) * 2
cgv_camera(v, position = c(dd * 0.7, dd * 0.6, dd * 0.8), target = c(0, 0, 0))
cgv_camera_mode(v, "orbit")

cat("\nMouse drag = orbit around the centre, scroll = zoom. Close to exit.\n")
cgv_run(v)
