#' Convert LRX Counts to Celestial Coordinates
#'
#' Maps cumulative operation counts (Left, Right, Reverse) to spherical
#' celestial coordinates via stereographic projection.
#'
#' @param nL Integer, cumulative count of left shift operations
#' @param nR Integer, cumulative count of right shift operations
#' @param nX Integer, cumulative count of reverse operations
#' @return List with components:
#'   \item{z}{Complex number, stereographic projection coordinate}
#'   \item{z_bar}{Complex conjugate of z}
#'   \item{theta}{Numeric, zenith angle (from X axis)}
#'   \item{phi}{Numeric, azimuthal angle (in LR plane)}
#'   \item{omega_conformal}{Numeric, conformal energy (magnitude of momentum vector)}
#' @export
#' @examples
#' coords <- convert_LRX_to_celestial(10, 5, 3)
#' coords$theta
#' coords$phi
convert_LRX_to_celestial <- function(nL, nR, nX) {
  momentum_squared <- nL^2 + nR^2 + nX^2
  r <- sqrt(momentum_squared)
  omega_conformal <- r

  if (r < 1e-10) {
    return(list(
      z = 0 + 0i,
      z_bar = 0 + 0i,
      theta = 0,
      phi = 0,
      omega_conformal = 0
    ))
  }

  theta <- acos(nX / r)
  phi <- atan2(nR, nL)
  z <- exp(1i * phi) * tan(theta / 2)

  return(list(
    z = z,
    z_bar = Conj(z),
    theta = theta,
    phi = phi,
    omega_conformal = omega_conformal
  ))
}

#' Angular Distance Between Two Celestial Points
#'
#' Computes the angular distance on the celestial sphere between two points
#' given as coordinate lists (each with a `z` component).
#'
#' @param result1 List with component `z` (complex number)
#' @param result2 List with component `z` (complex number)
#' @return Numeric, angular distance in radians
#' @export
#' @examples
#' c1 <- convert_LRX_to_celestial(10, 5, 3)
#' c2 <- convert_LRX_to_celestial(1, 1, 2)
#' calculate_angular_distance_z(c1, c2)
calculate_angular_distance_z <- function(result1, result2) {
  z1 <- result1$z
  z2 <- result2$z

  numerator <- abs(z1 - z2)^2
  denominator <- (1 + abs(z1)^2) * (1 + abs(z2)^2)

  cos_theta <- 1 - 2 * (numerator / denominator)
  cos_theta <- max(-1, min(1, cos_theta))

  acos(cos_theta)
}

#' Midpoint Between Two Celestial Coordinates
#'
#' Computes the midpoint on the celestial sphere between two coordinate
#' sets by averaging Cartesian unit-sphere positions and re-projecting.
#'
#' @param coords1 List with theta, phi, omega_conformal (and optionally nL, nR, nX)
#' @param coords2 List with theta, phi, omega_conformal (and optionally nL, nR, nX)
#' @return List with theta, phi, z, z_bar, omega_conformal, nL, nR, nX
#' @export
#' @examples
#' c1 <- convert_LRX_to_celestial(10, 5, 3)
#' c2 <- convert_LRX_to_celestial(1, 1, 2)
#' mid <- calculate_midpoint_z(c1, c2)
#' mid$theta
calculate_midpoint_z <- function(coords1, coords2) {
  theta1 <- coords1$theta
  phi1 <- coords1$phi
  theta2 <- coords2$theta
  phi2 <- coords2$phi

  x1 <- sin(theta1) * cos(phi1)
  y1 <- sin(theta1) * sin(phi1)
  z1 <- cos(theta1)

  x2 <- sin(theta2) * cos(phi2)
  y2 <- sin(theta2) * sin(phi2)
  z2 <- cos(theta2)

  x_mid <- (x1 + x2) / 2
  y_mid <- (y1 + y2) / 2
  z_mid <- (z1 + z2) / 2

  r_mid <- sqrt(x_mid^2 + y_mid^2 + z_mid^2)
  if (r_mid < 1e-10) r_mid <- 1e-10
  x_mid <- x_mid / r_mid
  y_mid <- y_mid / r_mid
  z_mid <- z_mid / r_mid

  theta_mid <- acos(z_mid)
  phi_mid <- atan2(y_mid, x_mid)
  z_complex <- exp(1i * phi_mid) * tan(theta_mid / 2)
  omega_mid <- (coords1$omega_conformal + coords2$omega_conformal) / 2

  nL_mid <- if (!is.null(coords1$nL) && !is.null(coords2$nL)) (coords1$nL + coords2$nL) / 2 else NA
  nR_mid <- if (!is.null(coords1$nR) && !is.null(coords2$nR)) (coords1$nR + coords2$nR) / 2 else NA
  nX_mid <- if (!is.null(coords1$nX) && !is.null(coords2$nX)) (coords1$nX + coords2$nX) / 2 else NA

  return(list(
    theta = theta_mid,
    phi = phi_mid,
    z = z_complex,
    z_bar = Conj(z_complex),
    omega_conformal = omega_mid,
    nL = nL_mid,
    nR = nR_mid,
    nX = nX_mid
  ))
}

#' Find Closest State to Target Coordinates
#'
#' Searches a table of reachable states for the state whose celestial
#' coordinates are closest to a target coordinate set.
#'
#' @param reachable_states Data frame or Arrow Table with columns theta, phi,
#'   omega_conformal, and V-columns for state
#' @param target_coords List with component `z` (complex number)
#' @param v_cols Character vector of V-column names
#' @return Single-row data frame of the closest state (with angular_distance column added)
#' @export
#' @examples
#' # Typically used with output from get_reachable_states
#' # find_closest_to_coords(states_df, target_coords, paste0("V", 1:n))
find_closest_to_coords <- function(reachable_states, target_coords, v_cols) {
  if (inherits(reachable_states, "ArrowTabular")) {
    reachable_states <- as.data.frame(reachable_states)
  }

  if (!"omega_conformal" %in% colnames(reachable_states)) {
    stop("reachable_states must contain coordinate columns (omega_conformal, theta, phi)")
  }

  target_z_real <- Re(target_coords$z)
  target_z_imag <- Im(target_coords$z)
  target_z_abs <- abs(target_coords$z)

  distances <- vapply(seq_len(nrow(reachable_states)), function(i) {
    z_current <- exp(1i * reachable_states$phi[i]) * tan(reachable_states$theta[i] / 2)
    z1_real <- Re(z_current)
    z1_imag <- Im(z_current)
    abs_z1 <- abs(z_current)

    numerator <- (z1_real - target_z_real)^2 + (z1_imag - target_z_imag)^2
    denominator <- (1 + abs_z1^2) * (1 + target_z_abs^2)

    cos_theta <- 1 - 2 * (numerator / denominator)
    cos_theta <- max(-1, min(1, cos_theta))
    acos(cos_theta)
  }, numeric(1))

  reachable_states$angular_distance <- distances

  n_top <- max(1, floor(nrow(reachable_states) * 0.1))
  top_indices <- order(distances)[1:min(n_top, nrow(reachable_states))]
  top_states <- reachable_states[top_indices, , drop = FALSE]

  if ("step" %in% colnames(top_states) && any(!is.na(top_states$step))) {
    best_state <- top_states[which.min(top_states$step), , drop = FALSE]
  } else {
    best_state <- top_states[1, , drop = FALSE]
  }

  return(best_state)
}
