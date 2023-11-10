\name{ratinterp}
\alias{ratinterp}
\title{
  Rational Interpolation
}
\description{
  Burlisch-Stoer rational interpolation.
}
\usage{
ratinterp(x, y, xs = x)
}
\arguments{
  \item{x}{numeric vector; points on the x-axis; needs to be sorted;
           at least three points required.}
  \item{y}{numeric vector; values of the assumed underlying function;
           \code{x} and \code{y} must be of the same length.}
  \item{xs}{numeric vector; points at which to compute the interpolation;
            all points must lie between \code{min(x)} and \code{max(x)}.}
}
\details{
  The Burlisch-Stoer approach to rational interpolation is a recursive
  procedure (similar to the Newton form of polynomial interpolation) that
  produces a ``diagonal'' rational function, that is the degree of the
  numerator is either the same or one less than the degree of the denominator.

  Polynomial interpolation will have difficulties if some kind of singularity
  exists in the neighborhood, even if the pole occurs in the complex plane.
  For instance, Runge's function has a pole at \eqn{z = 0.2 i}, quite close
  to the interval \eqn{[-1, 1]}.
}
\value{
  Numeric vector representing values at points \code{xs}.
}
\note{
  The algorithm does not yield a simple algebraic expression for the
  rational function found.
}
\references{
  Stoer, J., and R. Bulirsch (2002). Introduction to Numerical Analysis.
  Third Edition, Springer-Verlag, New York.

  Fausett, L. V. (2008). Applied Numerical Analysis Using Matlab.
  Second Edition, Pearson Education.
}
\seealso{
  \code{\link{rationalfit}}, \code{\link{pade}}
}
\examples{
## Rational interpolation of Runge's function
x <- c(-1, -0.5, 0, 0.5, 1.0)
y <- runge(x)
xs <- linspace(-1, 1)
ys <- runge(xs)
yy <- ratinterp(x, y, xs)  # returns exactly the Runge function

\dontrun{
plot(xs, ys, type="l", col="blue", lty = 2, lwd = 3)
points(x, y)
yy <- ratinterp(x, y, xs)
lines(xs, yy, col="red")
grid()}
}
\keyword{ fitting }
