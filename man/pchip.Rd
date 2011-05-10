\name{pchip}
\alias{pchip}
\title{Hermitean Interpolation Polynomials}
\description{
  Piecewise Cubic Hermitean Interpolation Polynomials.
}
\usage{
pchip(xi, yi, x)
}
\arguments{
  \item{xi, yi}{x- and y-coordinates of supporting nodes.}
  \item{x}{x-coordinates of interpolation points.}
}
\details{
  \code{pchip} is a `shape-preserving' piecewise cubic Hermite polynomial
  approach that apptempts to determine slopes such that function values do
  not overshoot data values.
}
\value{
  Values of interpolated data at points \code{x}.
}
\references{
  Moler, C. (2004). Numerical Computing with Matlab. Revised Reprint, SIAM.
}
\author{
  Copyright of the Matlab version from Cleve Moler in his book ``Numerical
  Computing with Matlab'', Chapter 3 on Interpolation.
  R Version by Hans W. Borchers, 2011.
}
\note{
  TODO: A `pchipfun' should be provided.
}
\seealso{
\code{\link{interp1}}
}
\examples{
\dontrun{
x <- c(1, 2, 3, 4, 5, 6)
y <- c(16, 18, 21, 17, 15, 12)
plot(x, y, col="red", xlim=c(0,7), ylim=c(10,22),
     main = "Spline and 'pchip' Interpolation Polynomials")
grid()

xs <- seq(1, 6, len=51)
ys <- interp1(x, y, xs, "spline")
lines(xs, ys, col="cyan")
yp <- pchip(x, y, xs)
lines(xs, yp, col = "magenta")}
}
\keyword{ math }
