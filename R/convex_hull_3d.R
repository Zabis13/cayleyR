#' Convex Hull of a 3-D Point Cloud
#'
#' Builds the convex hull of a set of points in three dimensions and reports its
#' triangular faces, surface area and volume. Implemented directly so the
#' package keeps its single dependency on Rcpp; for the handful of points a
#' landmark study produces, the incremental algorithm below is more than fast
#' enough.
#'
#' The construction is the classical incremental one. A non-degenerate starting
#' tetrahedron is found first, then the remaining points are added one at a
#' time: every face the new point can "see" (the point lies on the outer side of
#' its plane) is removed, and the boundary of the hole left behind -- the
#' horizon -- is joined to the new point. Points that see no face are already
#' inside and are skipped.
#'
#' Volume is the sum of the signed tetrahedra spanned by each face and an
#' interior reference point; because every face is oriented outwards, the signs
#' agree and the total is the enclosed volume.
#'
#' @param pts Numeric matrix with three columns, one row per point.
#' @param tol Numeric, distance below which a point counts as lying on a plane.
#'   Scaled by the spread of the cloud, so the default suits any magnitude.
#' @return A list with components:
#'   \item{faces}{Integer matrix, one row per triangular face, holding indices
#'     into \code{pts} in counter-clockwise order seen from outside}
#'   \item{vertices}{Integer vector, the rows of \code{pts} that are hull
#'     vertices}
#'   \item{area}{Numeric, total surface area}
#'   \item{volume}{Numeric, enclosed volume}
#'   \item{degenerate}{Logical, \code{TRUE} when the points are coplanar or
#'     fewer than four were supplied, in which case volume is 0}
#' @export
#' @examples
#' cube <- as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1)))
#' h <- convex_hull_3d(cube)
#' h$area      # 6
#' h$volume    # 1
convex_hull_3d <- function(pts, tol = 1e-9) {
  pts <- as.matrix(pts)
  if (ncol(pts) != 3L) stop("convex_hull_3d: pts must have three columns")
  np <- nrow(pts)

  empty <- list(faces = matrix(integer(0), 0L, 3L),
                vertices = integer(0), area = 0, volume = 0,
                degenerate = TRUE)
  if (np < 4L) return(empty)

  # Absolute tolerance from the spread of the cloud, so the caller does not have
  # to know the scale of the coordinates.
  span <- max(apply(pts, 2, function(z) diff(range(z))))
  if (span <= 0) return(empty)
  eps <- tol * max(1, span)

  # --- starting tetrahedron ---------------------------------------------
  # Two distinct points, then one off that line, then one off that plane.
  i1 <- 1L
  i2 <- NA_integer_
  for (i in 2:np) {
    if (sum((pts[i, ] - pts[i1, ])^2) > eps^2) { i2 <- i; break }
  }
  if (is.na(i2)) return(empty)

  d12 <- pts[i2, ] - pts[i1, ]
  i3 <- NA_integer_
  best <- eps
  for (i in seq_len(np)) {
    if (i == i1 || i == i2) next
    cr <- c(d12[2] * (pts[i, 3] - pts[i1, 3]) - d12[3] * (pts[i, 2] - pts[i1, 2]),
            d12[3] * (pts[i, 1] - pts[i1, 1]) - d12[1] * (pts[i, 3] - pts[i1, 3]),
            d12[1] * (pts[i, 2] - pts[i1, 2]) - d12[2] * (pts[i, 1] - pts[i1, 1]))
    m <- sqrt(sum(cr^2))
    if (m > best) { best <- m; i3 <- i }
  }
  if (is.na(i3)) return(empty)

  nrm <- function(a, b, c) {
    u <- pts[b, ] - pts[a, ]
    v <- pts[c, ] - pts[a, ]
    c(u[2] * v[3] - u[3] * v[2],
      u[3] * v[1] - u[1] * v[3],
      u[1] * v[2] - u[2] * v[1])
  }

  nv <- nrm(i1, i2, i3)
  i4 <- NA_integer_
  best <- eps
  for (i in seq_len(np)) {
    if (i == i1 || i == i2 || i == i3) next
    h <- abs(sum(nv * (pts[i, ] - pts[i1, ]))) / sqrt(sum(nv^2))
    if (h > best) { best <- h; i4 <- i }
  }
  if (is.na(i4)) {
    # All points lie in one plane: the hull is flat, so there is no volume.
    return(c(empty, list()))
  }

  # Orient the four faces outwards with respect to the centroid.
  faces <- rbind(c(i1, i2, i3), c(i1, i3, i4), c(i1, i4, i2), c(i2, i4, i3))
  centre <- colMeans(pts[c(i1, i2, i3, i4), , drop = FALSE])
  orient <- function(f) {
    n <- nrm(f[1], f[2], f[3])
    if (sum(n * (pts[f[1], ] - centre)) < 0) f[c(1, 3, 2)] else f
  }
  faces <- t(apply(faces, 1, orient))

  # --- incremental insertion --------------------------------------------
  outside <- function(f, p) {
    n <- nrm(f[1], f[2], f[3])
    m <- sqrt(sum(n^2))
    if (m < eps) return(-Inf)
    sum(n * (pts[p, ] - pts[f[1], ])) / m
  }

  for (p in seq_len(np)) {
    if (p %in% c(i1, i2, i3, i4)) next
    vis <- vapply(seq_len(nrow(faces)),
                  function(f) outside(faces[f, ], p) > eps, logical(1))
    if (!any(vis)) next                       # already inside the hull

    # The horizon: edges of visible faces not shared with another visible face.
    vf <- faces[vis, , drop = FALSE]
    edges <- rbind(vf[, c(1, 2)], vf[, c(2, 3)], vf[, c(3, 1)])
    key <- paste(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2]))
    horizon <- edges[!(key %in% key[duplicated(key)]), , drop = FALSE]

    faces <- faces[!vis, , drop = FALSE]
    if (nrow(horizon)) {
      faces <- rbind(faces, cbind(horizon[, 1], horizon[, 2], p))
    }
  }

  if (!nrow(faces)) return(empty)

  # --- area and volume ---------------------------------------------------
  ref <- colMeans(pts[unique(as.vector(faces)), , drop = FALSE])
  area <- 0
  vol <- 0
  for (f in seq_len(nrow(faces))) {
    a <- pts[faces[f, 1], ]
    b <- pts[faces[f, 2], ]
    c3 <- pts[faces[f, 3], ]
    cr <- nrm(faces[f, 1], faces[f, 2], faces[f, 3])
    area <- area + sqrt(sum(cr^2)) / 2
    vol <- vol + abs(sum((a - ref) * cr)) / 6
  }

  list(faces = matrix(as.integer(faces), ncol = 3L),
       vertices = sort(unique(as.vector(faces))),
       area = area,
       volume = vol,
       degenerate = FALSE)
}
