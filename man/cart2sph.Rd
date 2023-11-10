\name{cart2sph}
\alias{cart2sph}
\alias{sph2cart}
\alias{cart2pol}
\alias{pol2cart}
\title{
  Coordinate Transformations
}
\description{
  Transforms between cartesian, spherical, polar, and cylindrical coordinate
  systems in two and three dimensions.
}
\usage{
cart2sph(xyz)
sph2cart(tpr)
cart2pol(xyz)
pol2cart(prz)
}
\arguments{
  \item{xyz}{cartesian coordinates x, y, z as vector or matrix.}
  \item{tpr}{spherical coordinates theta, phi, and r as vector or matrix.}
  \item{prz}{polar coordinates phi, r or cylindrical coordinates phi, r, z
             as vector or matrix.}
}
\details{
  The spherical coordinate system used here consists of

  - \code{theta}, azimuth angle relative to the positive x-axis
  
  - \code{phi},   elevation angle measured from the reference plane
  
  - \code{r},     radial distance. i.e., distance to the origin

The polar angle, measured with respect from the polar axis, is then 
calculated as \code{pi/2 - phi}.
Note that this convention differs slightly from spherical coordinates 
\code{(r, theta, phi)} as often used in mathematics, where \code{phi} 
is the polar angle.

  \code{cart2sph} returns spherical coordinates as (theta, phi, r), and
  \code{sph2cart} expects them in this sequence.

  \code{cart2pol} returns polar coordinates (phi, r) if \code{length(xyz)==2}
  and cylindrical coordinates (phi, r, z) else. \code{pol2cart} needs them in
  this sequence and length.

  To go from cylindrical to cartesian coordinates, transform to cartesian
  coordinates first --- or write your own function, see the examples.

  All transformation functions are vectorized.
}
\value{
  All functions return a (2- or 3-dimensional) vector representing a point
  in the requested coordinate system, or a matrix with 2 or 3 named columns
  where is row represents a point. The columns are named accordingly.
}
\note{
  In Matlab these functions accept two or three variables and return two or
  three values. In R it did not appear appropriate to return coordinates as
  a list.

  These functions should be vectorized in the sense that they accept will
  accept matrices with number of rows or columns equal to 2 or 3.
}
\examples{
x <- 0.5*cos(pi/6); y <- 0.5*sin(pi/6); z <- sqrt(1 - x^2 - y^2)
(s <-cart2sph(c(x, y, z)))      # 0.5235988 1.0471976 1.0000000
sph2cart(s)                     # 0.4330127 0.2500000 0.8660254

cart2pol(c(1,1))                # 0.7853982 1.4142136
cart2pol(c(1,1,0))              # 0.7853982 1.4142136 0.0000000
pol2cart(c(pi/2, 1))            # 6.123234e-17 1.000000e+00
pol2cart(c(pi/4, 1, 1))         # 0.7071068 0.7071068 1.0000000

##  Transform spherical to cylindrical coordinates and vice versa
#   sph2cyl <- function(th.ph.r) cart2pol(sph2cart(th.ph.r))
#   cyl2sph <- function(phi.r.z) cart2sph(pol2cart(phi.r.z))
}
\keyword{ math }
