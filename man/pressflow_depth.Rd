% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pressflow.R
\name{pressflow_depth}
\alias{pressflow_depth}
\title{Backwater Height Upstream A Inlet Under Pressure (Bernoulli)}
\usage{
pressflow_depth(
  z0, z1, Q, Di = NULL, h = NULL, b = NULL, L, ks = NULL, kst,
  xi_e = 0.5, nu = 1.14e-6, calc_lam = "kst"
)
}
\arguments{
\item{z0}{Absolute height of upper gate – upstream of the inlet [m.a.s.l].}

\item{z1}{Absolute height of the pipe/rectangle vertical middle axis at
lower gate [m.a.s.l].}

\item{Q}{Flow [m^3/s].}

\item{Di}{Diameter of pipe [m]. If Di is specified, h and b must be NULL.}

\item{h}{Height of rectangle [m]. If h is specified, Di must be NULL.}

\item{b}{Width of rectangle [m]. If b is specified, Di must be NULL.}

\item{L}{Length of pipe [m].}

\item{ks}{Equivalent sand roughness [m].}

\item{kst}{Roughness [m^(1/3)/s].}

\item{xi_e}{Entrance loss [-]. Default = 0.5.}

\item{nu}{Kinematic viscosity [m^2/s]. Default = 1.14e-6.}

\item{calc_lam}{Defines if lambda should be calculated with ks or kst.}
}
\value{
Returns the backwater height upstream the inlet:
\describe{
  \item{h0}{Water depth upstream the inlet [m].}
  \item{v}{Flow velocity [m/s].}
}
}
\description{
Calculates the backwater height upstream of an inlet (pipe or
rectangle) under pressure (Bernoulli). The outlet is not submerged, e.g.,
the exit loss equals 0.
}
\examples{
# Flow in a pipe under pressure with ks value
pressflow_depth(z0 = 415, z1 = 413, Q = 5.18, L = 20, Di = 1,
                ks = 0.01, calc_lam = "ks")

# Flow in a rectangle under pressure with kst value
pressflow_depth(z0 = 415, z1 = 413, Q = 13.7, L = 20, b = 2, h = 1,
                kst = 60, calc_lam = "kst")

}
