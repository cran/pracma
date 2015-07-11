\name{fnorm}
\alias{fnorm}
\title{
Function Norm
}
\description{
  The \code{fnorm} function calculates several different types of function
  norms for depending on the argument \code{p}. 
}
\usage{
fnorm(f, g, x1, x2, p = 2, npoints = 100)
}
\arguments{
  \item{f, g}{functions given by name or string.}
  \item{x1, x2}{endpoints of the interval.}
  \item{p}{Numeric scalar or Inf, -Inf; default is 2.}
  \item{npoints}{number of points to be considered in the interval.}
}
\details{
  \code{fnorm} returns a scalar that gives some measure of the distance
  of two functions \code{f} and \code{g} on the interval \code{[x1, x2]}.

  It takes \code{npoints} equidistant points in the interval, computes the
  function values for \code{f} and \code{g} and applies \code{Norm} to
  their difference.

  Especially \code{p=Inf} returns the maximum norm,
  while \code{fnorm(f, g, x1, x2, p = 1, npoints) / npoints}
  would return some estimate of the mean distance.
}
\value{
  Numeric scalar (or \code{Inf}), or \code{NA} if one of these functions
  returns \code{NA}.
}
\note{
  Another kind of `mean' distance could be calculated by integrating the
  difference \code{f-g} and dividing through the length of the interval.
}
\seealso{
  \code{\link{Norm}}
}
\examples{
xp <- seq(-1, 1, length.out = 6)
yp <- runge(xp)
p5 <- polyfit(xp, yp, 5)
f5 <- function(x) polyval(p5, x)
fnorm(runge, f5, -1, 1, p = Inf)                  #=> 0.4303246
fnorm(runge, f5, -1, 1, p = Inf, npoints = 1000)  #=> 0.4326690

# Compute mean distance using fnorm:
fnorm(runge, f5, -1, 1, p = 1, 1000) / 1000       #=> 0.1094193

# Compute mean distance by integration:
fn <- function(x) abs(runge(x) - f5(x))
integrate(fn, -1, 1)$value / 2                    #=> 0.1095285
}
\keyword{ math }
