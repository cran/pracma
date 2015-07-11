\name{line_integral}
\alias{line_integral}
\title{
  Line integral (in the complex plane)
}
\description{
  Provides complex line integrals.
}
\usage{
line_integral(fun, waypoints, method = NULL, reltol = 1e-8, ...)
}
\arguments{
  \item{fun}{integrand, complex (vectorized) function.}
  \item{method}{integration procedure, see below.}
  \item{waypoints}{complex integration: points on the integration curve.}
  \item{reltol}{relative tolerance.}
  \item{...}{additional parameters to be passed to the function.}
}
\details{
  \code{line_integral} realizes complex line integration, in this case straight
  lines between the waypoints. By passing discrete points densely along the
  curve, arbitrary line integrals can be approximated.

  \code{line_integral} will accept the same methods as \code{integral};
  default is \code{integrate} from Base R.
}
\value{
  Returns the integral, no error terms given.
}
\seealso{
  \code{\link{integral}}
}
\examples{
##  Complex integration examples
points <- c(0, 1+1i, 1-1i, 0)           # direction mathematically negative
f <- function(z) 1 / (2*z -1)
I <- line_integral(f, points)
abs(I - (0-pi*1i))                      # 0 ; residuum 2 pi 1i * 1/2

f <- function(z) 1/z
points <- c(-1i, 1, 1i, -1, -1i)
I <- line_integral(f, points)           # along a rectangle around 0+0i
abs(I - 2*pi*1i)                        #=> 0 ; residuum: 2 pi i * 1

N <- 100
x <- linspace(0, 2*pi, N)
y <- cos(x) + sin(x)*1i
J <- line_integral(f, waypoints = y)    # along a circle around 0+0i
abs(I - J)                              #=> 5.015201e-17; same residuum
}
\keyword{ math }

