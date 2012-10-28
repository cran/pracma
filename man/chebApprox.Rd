\name{chebApprox}
\alias{chebApprox}
\title{Chebyshev Approximation}
\description{
  Function approximation through Chebyshev polynomials (of the first kind).
}
\usage{
chebApprox(x, fun, a, b, n)
}
\arguments{
  \item{x}{Numeric vector of points within interval \code{[a, b]}.}
  \item{fun}{Function to be approximated.}
  \item{a, b}{Endpoints of the interval.}
  \item{n}{An integer \code{>= 0}.}
}
\details{
  Return approximate y-coordinates of points at x by computing the
  Chebyshev approximation of degree n for \code{fun} on the interval
  \code{[a, b]}.
}
\value{
  A numeric vector of the same length as \code{x}.
}
\references{
  Press, W. H., S. A. Teukolsky, W. T. Vetterling, and B. P. Flannery (1992).
  Numerical Recipes in C: The Art of Scientific Computing. Second Edition,
  Cambridge University Press.
}
\note{
  TODO: Evaluate the Chebyshev approximative polynomial by using the
  Clenshaw recurrence formula.
  (Not yet vectorized, that's why we still use the Horner scheme.)
}
\seealso{
\code{\link{polyApprox}}
}
\examples{
# Approximate sin(x) on [-pi, pi] with a polynomial of degree 9 !
# This polynomial has to be beaten:
# P(x) = x - 1/6*x^3 + 1/120*x^5 - 1/5040*x^7 + 1/362880*x^9

# Compare these polynomials
p1 <- rev(c(0, 1, 0, -1/6, 0, 1/120, 0, -1/5040, 0, 1/362880))
p2 <- chebCoeff(sin, -pi, pi, 9)

# Estimate the maximal distance
x  <- seq(-pi, pi, length.out = 101)
ys <- sin(x)
yp <- polyval(p1, x)
yc <- chebApprox(x, sin, -pi, pi, 9)
max(abs(ys-yp))                       # 0.006925271
max(abs(ys-yc))                       # 1.151207e-05

\dontrun{
# Plot the corresponding curves
plot(x, ys, type = "l", col = "gray", lwd = 5)
lines(x, yp, col = "navy")
lines(x, yc, col = "red")
grid()}
}
\keyword{ math }
