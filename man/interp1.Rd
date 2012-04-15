\name{interp1}
\alias{interp1}
\title{
  One-dimensional Interpolation
}
\description{
  One-dimensional interpolation of points.
}
\usage{
interp1(x, y, xi = x,
        method = c("constant", "linear", "nearest", "spline", "cubic"))
}
\arguments{
  \item{x}{Numeric vector; points on the x-axis; at least two points require;
           will be sorted if necessary.}
  \item{y}{Numeric vector; values of the assumed underlying function;
           \code{x} and \code{y} must be of the same length.}
  \item{xi}{Numeric vector; points at which to compute the interpolation;
            all points must lie between \code{min(x)} and \code{max(x)}.}
  \item{method}{One of ``constant", ``linear", ``nearest", ``spline", or ``cubic".}
}
\details{
  Interpolation to find \code{yi}, the values of the underlying function
  at the points in the vector \code{xi}.

  Methods can be:
  \tabular{ll}{
  \code{constant} \tab constant between points \cr
  \code{linear} \tab linear interpolation (default) \cr
  \code{nearest} \tab nearest neighbor interpolation \cr
  \code{spline} \tab cubic spline interpolation \cr
  \code{cubic} \tab cubic Hermite interpolation \cr
  }
}
\value{
  Numeric vector representing values at points \code{xi}.
}
\note{
  Method `spline' uses the ``fmm'' approach by Moler et al. and is identical
  with the Matlab option of the same name.

  The Matlab option ``cubic'' seems to have no direct correspondence in R.
  Therefore, we simply use \code{pchip} here.
}
\seealso{
\code{\link{approx}}, \code{\link{splinefun}}
}
\examples{
x <- c(0.8, 0.3, 0.1, 0.6, 0.9, 0.5, 0.2, 0.0, 0.7, 1.0, 0.4)
y <- x^2
xi <- seq(0, 1, len = 81)
yl <- interp1(x, y, xi, method = "linear")
yn <- interp1(x, y, xi, method = "nearest")
ys <- interp1(x, y, xi, method = "spline")

\dontrun{
plot(x, y); grid()
lines(xi, yl, col="blue", lwd = 2)
lines(xi, yn, col="black", lty = 2)
lines(xi, ys, col="red")
}
}
\keyword{ math }
