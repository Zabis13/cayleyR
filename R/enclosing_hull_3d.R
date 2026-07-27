#' Enclosing Hull Through Every Point
#'
#' Builds a closed triangulated surface whose vertices are \emph{all} the given
#' points, not just the ones on the convex boundary.
#'
#' \code{\link{convex_hull_3d}} returns the smallest convex body containing the
#' cloud, so any point strictly inside it is not a vertex of the result. When
#' the point set is meant to be read as the corners of one figure, that is the
#' wrong answer: some of the corners go missing. This function starts from the
#' convex hull and then, for each interior point in turn, replaces the triangle
#' whose plane it sits closest under with three triangles meeting at that point.
#' The surface is dented inwards there, so the body stops being convex, but
#' every supplied point ends up on it.
#'
#' Because the surface is no longer convex, the volume is computed as the signed
#' sum of tetrahedra over the oriented faces, which stays correct for any closed
#' surface that does not intersect itself.
#'
#' @param pts Numeric matrix with three columns, one row per point.
#' @param tol Numeric, tolerance passed through to \code{\link{convex_hull_3d}}.
#' @return A list with the same components as \code{\link{convex_hull_3d}} plus
#'   \item{pushed}{Integer vector, the points that had to be pulled onto the
#'     surface, i.e. those that were interior to the convex hull}
#' @export
#' @seealso \code{\link{convex_hull_3d}}
#' @examples
#' set.seed(1)
#' p <- rbind(as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1))), c(0.5, 0.5, 0.5))
#' h <- enclosing_hull_3d(p)
#' length(h$vertices)   # 9: the interior point is a vertex too
enclosing_hull_3d <- function(pts, tol = 1e-9) {
  pts <- as.matrix(pts)
  np <- nrow(pts)

  hull <- convex_hull_3d(pts, tol = tol)
  if (hull$degenerate) return(c(hull, list(pushed = integer(0))))

  faces <- hull$faces
  inside <- setdiff(seq_len(np), hull$vertices)
  pushed <- integer(0)

  nrm <- function(f) {
    u <- pts[f[2], ] - pts[f[1], ]
    v <- pts[f[3], ] - pts[f[1], ]
    c(u[2] * v[3] - u[3] * v[2],
      u[3] * v[1] - u[1] * v[3],
      u[1] * v[2] - u[2] * v[1])
  }

  for (p in inside) {
    # The face this point is nearest to from within: distances are negative for
    # an interior point, so the largest one is the closest plane.
    d <- vapply(seq_len(nrow(faces)), function(f) {
      n <- nrm(faces[f, ])
      m <- sqrt(sum(n^2))
      if (m < tol) return(-Inf)
      sum(n * (pts[p, ] - pts[faces[f, 1], ])) / m
    }, numeric(1))

    hit <- which.max(d)
    tri <- faces[hit, ]
    # Split that triangle around p, keeping the outward orientation of each
    # piece, which dents the surface inwards to reach the point.
    faces <- rbind(faces[-hit, , drop = FALSE],
                   c(tri[1], tri[2], p),
                   c(tri[2], tri[3], p),
                   c(tri[3], tri[1], p))
    pushed <- c(pushed, p)
  }

  # Signed volume: valid for any closed oriented surface, convex or not.
  ref <- colMeans(pts)
  area <- 0
  vol <- 0
  for (f in seq_len(nrow(faces))) {
    n <- nrm(faces[f, ])
    area <- area + sqrt(sum(n^2)) / 2
    vol <- vol + sum((pts[faces[f, 1], ] - ref) * n) / 6
  }

  list(faces = matrix(as.integer(faces), ncol = 3L),
       vertices = sort(unique(as.vector(faces))),
       area = area,
       volume = abs(vol),
       degenerate = FALSE,
       pushed = pushed)
}
