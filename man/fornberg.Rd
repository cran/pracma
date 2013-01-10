\name{fornberg}
\alias{fornberg}
\title{
  Fornberg's Finite Difference Approximation
}
\description{
  Finite difference approximation using Fornberg's method for the derivatives
  of order 1 to k based on irregulat grid values.
}
\usage{
fornberg(x, y, xs, k = 1)
}
\arguments{
  \item{x}{grid points on the x-axis, must be distinct.}
  \item{y}{discrete values of the function at the grid points.}
  \item{xs}{point at which to approximate (not vectorized).}
  \item{k}{order of derivative, \code{k<=length(x)-1} required.}
}
\details{
  Compute coefficients for finite difference approximation for the derivative
  of order \code{k} at \code{xs} based on grid values at points in \code{x}.
  For \code{k=0} this will evaluate the interpolating polynomial itself, but
  call it with \code{k=1}.
}
\value{
  Returns a matrix of size \code{(length(xs))}, where the (k+1)-th column
  gives the value of the k-th derivative. Especially the first column returns 
  the polynomial interpolation of the function.
}
\note{
  Fornberg's method is considered to be numerically more stable than applying
  Vandermonde's matrix.
}
\references{
  LeVeque, R. J. (2007). Finite Difference Methods for Ordinary and Partial
  Differential Equations. Society for Industrial and Applied Mathematics 
  (SIAM), Philadelphia.
}
\seealso{
  \code{\link{neville}}, \code{\link{newtonInterp}}
}
\examples{
x <- 2 * pi * c(0.0, 0.07, 0.13, 0.2, 0.28, 0.34, 0.47, 0.5, 0.71, 0.95, 1.0)
y <- sin(0.9*x)
xs <- linspace(0, 2*pi, 51)
fornb <- fornberg(x, y, xs, 10)
\dontrun{
matplot(xs, fornb, type="l")
grid()}
}
\keyword{ fitting }
