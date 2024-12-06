#' @title Sediment transport capacity according to Smart and Jaeggi
#' @description Calculates the sediment transport capacity, i.e. solids volume,
#' per time unit. The formula is valid for slopes between 0.005 and 0.2.
#' @usage Gb_SJ(d30, dm, d90, J, Rs, um, B, t_crit = 0.05, rho_s = 2650, s_value = 2.65)
#' @param d30 Grain size distribution parameter (d30) [m]
#' @param dm Median grain size (dm) [m]
#' @param d90 Grain size distribution parameter (d90) [m]
#' @param J Slope of the bed [-]
#' @param Rs Hydraulic radius [m]
#' @param um Mean flow velocity [m/s]
#' @param B Bottom width [m]
#' @param t_crit Critical shear stress [-]
#' @param rho_s Density of bedload material [kg/m³]
#' @param s_value Density of sediment [-]
#' @return Gb Sediment transport rate [kg/s]
#' @examples
#' d30 <- 0.05
#' dm <- 0.1
#' d90 <- 0.2
#' J <- 0.03
#' Rs <- 1
#' um <- 2
#' B <- 3
#'
#' Gb_SJ(d30 = 0.05, dm = 0.10, d90 = 0.2, J = 0.03, Rs = 1, um = 2, B = 5)
#' @export

Gb_SJ <- function(d30, dm, d90, J, Rs, um, B, t_crit = 0.05, rho_s = 2650, s_value = 2.65) {

  Gb <- max(0, 4 * rho_s / (s_value - 1) * (d90 / d30)^0.2 * J^1.6 * Rs * um * B *
              (1 - t_crit * (s_value - 1) * dm / (Rs * J)))

  return(Gb)
}
