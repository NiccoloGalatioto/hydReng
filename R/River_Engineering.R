#------------------------------------------------------------------------------
# Block size according to Stevens and Simons (Bezzola, 2012)
#------------------------------------------------------------------------------

#' Calculate dimensions of rip rap block size
#'
#' Calculates the dimensions and mass of a rip rap block based on slope
#' geometry, water table levels, and material properties.
#'
#' @usage block_size(h, h_z, J, gamma, psi, geo = NULL, S = 1.15, Theta_c = 0.047,
#'   s = 2.65, ret = "all")
#'
#' @param h Numeric. Global maximum water table level above riverbed [m].
#' @param h_z Numeric. Local water table level above the regarded block [m].
#' @param J Numeric. Bottom slope [-].
#' @param gamma Numeric or NULL. Angle of bank slope [degrees]. Use NULL if
#'   specifying \code{geo}.
#' @param geo Numeric vector of length 2 or NULL. Slope geometry as a triangle:
#'   c(vertical length, horizontal length) [-]. If given, \code{gamma} is ignored.
#' @param psi Numeric. Inner friction angle [degrees]. Values between 50 and 55
#'   are recommended (Bezzola 2012).
#' @param S Numeric. Safety factor, default is 1.15 [-].
#' @param Theta_c Numeric. Shear stress parameter, default is 0.047 [-].
#' @param s Numeric. Relative density of blocks, default is 2.65 [-].
#' @param ret Character. Result to return: \code{"all"} (default), \code{"D"}, or \code{"b"}.
#'
#'
#' @return
#' If \code{ret = "all"}, returns a list with:
#' \item{D}{Diameter of block [m]}
#' \item{m}{Mass of block [kg]}
#' \item{a}{a-axis length [m]}
#' \item{b}{b-axis length [m]}
#' \item{c}{c-axis length [m]}
#'
#' Otherwise returns the requested single value:
#' \itemize{
#'  \item \code{"D"} Diameter of block [m]
#'  \item \code{"b"} b-axis length [m]
#' }
#'
#' @references
#' Bezzola (2012). Flussbau, Vorlesungsmanuskript, ETH Zuerich
#'
#' @examples
#' # Calculate block size at bottom of slope with given slope angle
#' block_size(h = 5, h_z = 5, J = 0.0015, gamma = 33.69, psi = 50)
#'
#' # Calculate block size with slope geometries 2:3
#' block_size(h = 5, h_z = 5, J = 0.0015, gamma = NULL, psi = 50, geo = c(2, 3))
#'
#' # Calculate block size at middle of slope with slope geometries 2:3
#' block_size(h = 5, h_z = 2.5, J = 0.0015, gamma = NULL, psi = 50, geo = c(2, 3))
#' @export


block_size <- function(h, h_z, J, gamma, psi,
                       geo = NULL, S = 1.15, Theta_c = 0.047,
                       s = 2.65, ret = "all") {

  if (!is.null(geo)) {
    if (!is.null(gamma)) {
      warning("If 'geo' is specified, set gamma = NULL")
    }

    xrad <- atan(geo[1] / geo[2])
    gamma <- 180 * xrad / pi
  }

  if (h_z > 0.77 * h) {
    H <- 0.77 * h
  } else {
    H <- h_z
  }

  D <- (H * J) /
    (Theta_c * (s - 1) * cos(gamma * pi / 180) *
       (1 / S - S * tan(gamma * pi / 180)^2 / tan(psi * pi / 180)^2))

  m <- round((1 / 6) * D^3 * pi * s * 1e3)

  if (ret == "all") {
    res <- list(
      D = round(D, 2),
      m = m,
      a = round(D / 0.68, 2),
      b = round(D / 0.91, 2),
      c = round(D / 1.30, 2)
    )
    return(res)

  } else if (ret == "D") {
    return(round(D, 2))

  } else if (ret == "b") {
    return(D / 0.91)
  }
}


#------------------------------------------------------------------------------
# Superelevation of water table in curve (Bezzola 2012, Kap. 11.4)
#------------------------------------------------------------------------------
#' Superelevation of water table in curve
#'
#' Calculates the superelevation of the water table in a river curve.
#'
#' @param w Numeric. Horizontal sole width [m].
#' @param rm Numeric. Curve radius from center to the middle of the river [m].
#' @param v Numeric. Flow velocity [m/s].
#' @param S Numeric. Safety factor, default is 1.5.
#'
#' @return Numeric. The difference between mean water level and superelevation [m].
#'
#' @references
#' Bezzola (2012). Flussbau, Vorlesungsmanuskript, ETH Zuerich
#'
#' @examples
#' # Calculate superelevation
#' wt_sup(w = 30, rm = 200, v = 5)
#' @export

wt_sup <- function(w, rm, v, S = 1.5) {
  dh <- S * (w / rm) * (v^2 / (2 * 9.81))
  return(dh)
}

#------------------------------------------------------------------------------
# Shear stress calculation
#------------------------------------------------------------------------------

#' Shear stress, shear velocity, and dimensionless shear stress
#'
#' Calculates shear stress, shear velocity, and dimensionless shear stress
#' based on water depth, slope, and grain size.
#'
#' @param h0 Numeric. Total water depth [m].
#' @param J Numeric. Bottom slope [-].
#' @param dm Numeric or NULL. Median grain size (\code{d50}) of sediment [mm].
#' @param h Numeric or NULL. Local water depth at the point of interest [m].
#'   If \code{NULL}, considered equal to \code{h0}.
#' @param rho Numeric. Density of water [kg/m3], default is 1000.
#'
#' @return A named list with components:
#' \item{tau}{Shear stress [N/m2].}
#' \item{U}{Shear velocity [m/s].}
#' \item{tau_st}{Dimensionless shear stress [-], if \code{dm} is provided,
#' otherwise \code{NA}.}
#'
#' @references
#' Bezzola (2012). Flussbau, Vorlesungsmanuskript, ETH Zuerich
#'
#' @examples
#' # Calculate shear stress at bank bottom
#' shear_str(h0 = 3.31, J = 0.0022)$tau
#'
#' # Calculate shear stress at bank middle
#' shear_str(h0 = 3.31, J = 0.0022, h = 1.6)$tau
#'
#' # Calculate dimensionless shear stress
#' shear_str(h0 = 3.31, J = 0.0022, dm = 100)$tau_st
#' @export


shear_str <- function(h0, J, dm = NULL, h = NULL, rho = 1000) {

  if (is.numeric(h)) {
    tau <- rho * 9.81 * h0 * J * (1 - ((h0 - h) / h0))
  } else {
    tau <- rho * 9.81 * h0 * J
  }

  # Shear velocity
  U <- sqrt(tau / rho)

  # Dimensionless shear stress
  if (is.numeric(dm)) {
    tau_st <- tau / (1650 * 9.81 * (dm / 1000))
    return(list(tau = tau, U = U, tau_st = tau_st))
  } else {
    return(list(tau = tau, U = U))
  }
}
