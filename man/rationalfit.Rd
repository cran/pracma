\name{rationalfit}
\alias{rationalfit}
\title{
  Rational Function Approximation
}
\description{
  Fitting a rational function to data points.
}
\usage{
rationalfit(x, y, d1 = 5, d2 = 5)
}
\arguments{
  \item{x}{numeric vector; points on the x-axis; needs to be sorted;
           at least three points required.}
  \item{y}{numeric vector; values of the assumed underlying function;
           \code{x} and \code{y} must be of the same length.}
  \item{d1, d2}{maximal degrees of numerator (\code{d1}) and denominator
                (\code{d1}) of the requested rational function.}
}
\details{
  A rational fit is a rational function of two polynomials \code{p1} and
  \code{p2} (of user specified degrees \code{d1} and \code{d2}) such that
  \code{p1(x)/p2(x)} approximates \code{y} in a least squares sense.

  \code{d1} and \code{d2} must be large enough to get a good fit and usually
  \code{d1=d2} gives good results
}
\value{
  List with components \code{p1} and \code{p2} for the polynomials in
  numerator and denominator of the rational function.
}
\note{
  This implementation will later be replaced by a `barycentric rational
  interpolation'.
}
\author{
  Copyright (c) 2006 by Paul Godfrey for a Matlab version available from the
  MatlabCentral under BSD license. R re-implementation by Hans W Borchers.
}
\references{
  Press, W. H., S. A. Teukolsky, W. T Vetterling, and B. P. Flannery (2007).
  Numerical Recipes: The Art of Numerical Computing. Third Edition,
  Cambridge University Press, New York.
}
\seealso{
  \code{\link{ratinterp}}
}
\examples{
\dontrun{
x <- linspace(0, 15, 151); y <- sin(x)/x
rA <- rationalfit(x, y, 10, 10); p1 <- rA$p1; p2 <- rA$p2
ys <- polyval(p1,x) / polyval(p2,x)
plot(x, y, type="l", col="blue", ylim=c(-0.5, 1.0))
points(x, Re(ys), col="red")  # max(abs(y-ys), na.rm=TRUE) < 1e-6
grid()

# Rational approximation of the Zeta function
x <- seq(-5, 5, by = 1/16)
y <- zeta(x)
rA <- rationalfit(x, y, 10, 10); p1 <- rA$p1; p2 <- rA$p2
ys <- polyval(p1,x) / polyval(p2,x)
plot(x, y, type="l", col="blue", ylim=c(-5, 5))
points(x, Re(ys), col="red")
grid()

# Rational approximation to the Gamma function
x <- seq(-5, 5, by = 1/32); y <- gamma(x)
rA <- rationalfit(x, y, 10, 10); p1 <- rA$p1; p2 <- rA$p2
ys <- polyval(p1,x) / polyval(p2,x)
plot(x, y, type="l", col = "blue")
points(x, Re(ys), col="red")
grid()}
}
\keyword{ fitting }
