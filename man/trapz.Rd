\name{trapz}
\alias{trapz}
\alias{cumtrapz}
\alias{trapzfun}
\title{Trapezoidal Integration}
\description{
  Compute the area of a function with values \code{y} at the points
  \code{x}.
}
\usage{
  trapz(x, y)
  cumtrapz(x, y)

  trapzfun(f, a, b, maxit = 25, tol = 1e-07, ...)
}
\arguments{
  \item{x}{x-coordinates of points on the x-axis}
  \item{y}{y-coordinates of function values}
  \item{f}{function to be integrated.}
  \item{a, b}{lower and upper border of the integration domain.}
  \item{maxit}{maximum number of steps.}
  \item{tol}{tolerance; stops when improvements are smaller.}
  \item{...}{arguments passed to the function.}
}
\details{
  The points \code{(x, 0)} and \code{(x, y)} are taken as vertices of a
  polygon and the area is computed using \code{polyarea}. This approach
  matches exactly the approximation for integrating the function using the
  trapezoidal rule with basepoints \code{x}.

  \code{cumtrapz} computes the cumulative integral of \code{y} with respect 
  to \code{x} using trapezoidal integration. \code{x} and \code{y} must be
  vectors of the same length, or \code{x} must be a vector and \code{y} a
  matrix whose first dimension is \code{length(x)}.
  
  Inputs \code{x} and \code{y} can be complex.

  \code{trapzfun} realizes trapezoidal integration and stops when the
  differencefrom one step to the next is smaller than tolerance (or the
  of iterations get too big). The function will only be evaluated once
  on each node.
}
\value{
  Approximated integral of the function, discretized through the points
  \code{x, y}, from \code{min(x)} to \code{max(x)}.
  Or a matrix of the same size as \code{y}.

  \code{trapzfun} returns a lst with components \code{value} the value of
  the integral, \code{iter} the number of iterations, and \code{rel.err}
  the relative error.
}
\seealso{
  \code{\link{polyarea}}
}
\examples{
  # Calculate the area under the sine curve from 0 to pi:
  n <- 101
  x <- seq(0, pi, len = n)
  y <- sin(x)
  trapz(x, y)                       #=> 1.999835504

  # Use a correction term at the boundary: -h^2/12*(f'(b)-f'(a))
  h  <- x[2] - x[1]
  ca <- (y[2]-y[1]) / h
  cb <- (y[n]-y[n-1]) / h
  trapz(x, y) - h^2/12 * (cb - ca)  #=> 1.999999969

  # Use two complex inputs
  z  <- exp(1i*pi*(0:100)/100)
  ct <- cumtrapz(z, 1/z)
  ct[101]                           #=> 0+3.14107591i

  f <- function(x) x^(3/2)          # 
  trapzfun(f, 0, 1)                 #=> 0.4 with 11 iterations
}

\keyword{ math }
