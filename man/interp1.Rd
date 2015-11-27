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
        method = c("linear", "constant", "nearest", "spline", "cubic"))
}
\arguments{
  \item{x}{Numeric vector; points on the x-axis; at least two points require;
           will be sorted if necessary.}
  \item{y}{Numeric vector; values of the assumed underlying function;
           \code{x} and \code{y} must be of the same length.}
  \item{xi}{Numeric vector; points at which to compute the interpolation;
            all points must lie between \code{min(x)} and \code{max(x)}.}
  \item{method}{One of ``constant", ``linear", ``nearest", ``spline", or ``cubic"; default is ``linear"}
}
\details{
  Interpolation to find \code{yi}, the values of the underlying function
  at the points in the vector \code{xi}.

  Methods can be:
  \tabular{ll}{
  \code{linear} \tab linear interpolation (default) \cr
  \code{constant} \tab constant between points \cr
  \code{nearest} \tab nearest neighbor interpolation \cr
  \code{spline} \tab cubic spline interpolation \cr
  \code{cubic} \tab cubic Hermite interpolation \cr
  }
}
\value{
  Numeric vector representing values at points \code{xi}.
}
\note{
  Method `spline' uses the spline approach by Moler et al., and is identical 
  with the Matlab option of the same name, but slightly different from R's 
  spline function.

  The Matlab option ``cubic'' seems to have no direct correspondence in R.
  Therefore, we simply use \code{pchip} here.
}
\seealso{
\code{\link{approx}}, \code{\link{spline}}
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

## Difference between spline (Matlab) and spline (R).
x <- 1:6
y <- c(16, 18, 21, 17, 15, 12)
xs <- linspace(1, 6, 51)
ys <- interp1(x, y, xs, method = "spline")
sp <- spline(x, y, n = 51, method = "fmm")

\dontrun{
plot(x, y, main = "Matlab and R splines")
grid()
lines(xs, ys, col = "red")
lines(sp$x, sp$y, col = "blue")
legend(4, 20, c("Matlab spline", "R spline"), 
              col = c("red", "blue"), lty = 1)
  }
}
\keyword{ math }
