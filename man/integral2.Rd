\name{integral2}
\alias{integral2}
\alias{integral3}
\title{
  Numerically Evaluate Double  and Triple Integrals
}
\description{
  Numerically evaluate a double integral, resp. a triple integral by
  reducing it to a double integral.
}
\usage{
integral2(fun, xmin, xmax, ymin, ymax, sector = FALSE,
            reltol = 1e-6, abstol = 0, maxlist = 5000,
            singular = FALSE, vectorized = TRUE, ...)

integral3(fun, xmin, xmax, ymin, ymax, zmin, zmax,
            reltol = 1e-6, ...)
}
\arguments{
  \item{fun}{function}
  \item{xmin, xmax}{lower and upper limits of x.}
  \item{ymin, ymax}{lower and upper limits of y.}
  \item{zmin, zmax}{lower and upper limits of z.}
  \item{sector}{logical.}
  \item{reltol}{relative tolerance.}
  \item{abstol}{absolute tolerance.}
  \item{maxlist}{maximum length of the list of rectangles.}
  \item{singular}{logical; are there singularities at vertices.}
  \item{vectorized}{logical; is the function fully vectorized.}
  \item{...}{additional parameters to be passed to the function.}
}
\details{
  \code{integral2} implements the `TwoD' algorithm, that is Gauss-Kronrod
  with (3, 7)-nodes on 2D rectangles.

  The borders of the domain of integration must be finite. The limits of
  \code{y}, that is \code{ymin} and \code{ymax}, can be constants or scalar
  functions of x that describe the lower and upper boundaries. These
  functions must be vectorized.

  \code{integral2} attempts to satisfy \code{ERRBND <= max(AbsTol,RelTol*|Q|)}.
  This is absolute error control when \code{|Q|} is sufficiently small and
  relative error control when \code{|Q|} is larger.

  The function \code{fun} itself must be fully vectorized:
  It must accept arrays \code{X} and \code{Y} and return an array
  \code{Z = f(X,Y)} of corresponding values. If option \code{vectorized} is
  set to \code{FALSE} the procedure will enforce this vectorized behavior.

  With \code{sector=TRUE} the region is a generalized sector that is described
  in polar coordinates (r,theta) by

  \code{0 <= a <= theta <= b} -- a and b must be constants\cr
  \code{c <= r <= d} -- c and d can be constants or ...

  ... functions of theta that describe the lower and upper boundaries.
  Functions must be vectorized.\cr
  NOTE Polar coordinates  are used only to describe the region --
  the integrand is \code{f(x,y)} for both kinds of regions.

  \code{integral2} can be applied to functions that are singular on a boundary.
  With value \code{singular=TRUE}, this option causes \code{integral2} to use
  transformations to weaken singularities for better performance.

  \code{integral3} also accepts functions for the inner interval limits.
  \code{ymin, ymax} must be constants or functions of one variable (\code{x}),
  \code{zmin, zmax} constants or functions of two variables (\code{x, y}), all
  functions vectorized.

  The triple integral will be first integrated over the second and third 
  variable with \code{integral2}, and then integrated over a single variable 
  with \code{integral}.
}
\value{
  Returns a list with \code{Q} the integral and \code{error} the error term.
}
\references{
  Shampine, L. F. (2008). MATLAB Program for Quadrature in 2D.
  Proceedings of Applied Mathematics and Computation, 2008, pp. 266--274.
}
\author{
  Copyright (c) 2008 Lawrence F. Shampine for Matlab code and description
  of the program; adapted and converted to R by Hans W Borchers.
}
\note{
  To avoid recursion, a possibly large matrix will be used and passed between
  subprograms. A more efficient implementation may be possible.
}
\seealso{
  \code{\link{integral}}, \code{cubature:adaptIntegrate}
}
\examples{
fun <- function(x, y) cos(x) * cos(y)
integral2(fun, 0, 1, 0, 1, reltol = 1e-10)
# $Q:     0.708073418273571  # 0.70807341827357119350 = sin(1)^2
# $error: 8.618277e-19       # 1.110223e-16

##  Compute the volume of a sphere
f <- function(x, y) sqrt(1 -x^2 - y^2)
xmin <- 0; xmax <- 1
ymin <- 0; ymax <- function(x) sqrt(1 - x^2)
I <- integral2(f, xmin, xmax, ymin, ymax)
I$Q                             # 0.5236076 - pi/6 => 8.800354e-06

##  Compute the volume over a sector
I <- integral2(f, 0,pi/2, 0,1, sector = TRUE)
I$Q                             # 0.5236308 - pi/6 => 3.203768e-05

##  Integrate 1/( sqrt(x + y)*(1 + x + y)^2 ) over the triangle
##   0 <= x <= 1, 0 <= y <= 1 - x.  The integrand is infinite at (0,0).
f <- function(x,y) 1/( sqrt(x + y) * (1 + x + y)^2 )
ymax <- function(x) 1 - x
I <- integral2(f, 0,1, 0,ymax)
I$Q + 1/2 - pi/4                # -3.247091e-08

##  Compute this integral as a sector
rmax <- function(theta) 1/(sin(theta) + cos(theta))
I <- integral2(f, 0,pi/2, 0,rmax, sector = TRUE, singular = TRUE)
I$Q + 1/2 - pi/4                # -4.998646e-11

##  Examples of computing triple integrals
f0 <- function(x, y, z) y*sin(x) + z*cos(x)
integral3(f0, 0, pi, 0,1, -1,1) # - 2.0 => 0.0

f1 <- function(x, y, z) exp(x+y+z)
integral3(f1, 0, 1, 1, 2, 0, 0.5)
## [1] 5.206447                         # 5.20644655

f2 <- function(x, y, z) x^2 + y^2 + z
a <- 2; b <- 4
ymin <- function(x) x - 1
ymax <- function(x) x + 6
zmin <- -2
zmax <- function(x, y) 4 + y^2
integral3(f2, a, b, ymin, ymax, zmin, zmax)
## [1] 47416.75556                      # 47416.7555556

f3 <- function(x, y, z) sqrt(x^2 + y^2)
a <- -2; b <- 2
ymin <- function(x) -sqrt(4-x^2)
ymax <- function(x)  sqrt(4-x^2)
zmin <- function(x, y)  sqrt(x^2 + y^2)
zmax <- 2
integral3(f3, a, b, ymin, ymax, zmin, zmax)
## [1] 8.37758                          # 8.377579076269617
}
\keyword{ math }
