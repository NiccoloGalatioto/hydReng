#' @title Sediment transport cappacity according to Smart and Jaeggi
#' @description Calculates the sediment transport cappacity,i.e. solids-volume per time unity. The forumla is valid for slopes between 0.005 and 0.2.
#' @usage Gb_SJ(d30, dm, d90, J, Rs, um, B,tcrit=0.05,rho_s=2650,s=2.65)
#' @param d30 d30 of grain size distribution [m]
#' @param dm dm of grain size distribution [m]
#' @param d90 d90 of grain size distribution [m]
#' @param J slope [-]
#' @param Rs hydraulic radius [m]
#' @param um mean flow velocity [m/s]
#' @param B bottom width [m]
#' @param tcrit critical shear stress
#' @param rho_s density of beadload
#' @param s density
#' @return Gb sediment transport rate [kg/s]
#' @examples
#' d30 <- 0.05
#' dm <- 0.1
#' d90 <- 0.2
#' J <- 0.03
#' Rs <- 1
#' um <- 2
#' B <- 3
#'
#' Gb_SJ(d30=0.05, dm=0.10, d90=0.2, J=0.03, Rs=1, um=2, B=5)

#' @export
#'





Gb_SJ <- function(d30,dm,d90,J,Rs,um,B,tcrit=0.05,rho_s=2650,s=2.65){

  Gb <- max(0, 4*rho_s/(s-1) * (d90/d30)^0.2 * J^1.6 *Rs* um * B * (1-tcrit*(s-1)*dm/(Rs*J)))

  return(Gb)

}
