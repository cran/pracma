\name{cubicspline}
\alias{cubicspline}
\title{
  Interpolating Cubic Spline
}
\description{
  Computes the natural interpolation cubic spline.
}
\usage{
cubicspline(x, y, xi = NULL, endp2nd = FALSE, der = c(0, 0))
}
\arguments{
  \item{x, y}{x- and y-coordinates of points to be interpolated.}
  \item{xi}{x-coordinates of points at which the interpolation is to be
            performed.}
  \item{endp2nd}{logical; if true, the derivatives at the endpoints are
                 prescribed by \code{der}.}
  \item{der}{a two-components vector prescribing derivatives at endpoints.}
}
\details{
  \code{cubicspline} computes the values at \code{xi} of the natural 
  interpolating cubic spline that interpolate the values \code{y} at the
  nodes \code{x}. The derivatives at the endpoints can be prescribed.
}
\value{
  Returns either the interpolated values at the points \code{xi} or, if
  \code{is.null(xi)}, the piecewise polynomial that represents the spline.
}
\note{
  From the piecewise polynomial returned one can easily generate the spline
  function, see the examples.
}
\references{
  Quarteroni, Q., and F. Saleri (2006). Scientific Computing with Matlab
  and Octave. Springer-Verlag Berlin Heidelberg.
}
\seealso{
  \code{\link{spline}}
}
\examples{
##  Example: Average temperatures at different latitudes
x <- seq(-55, 65, by = 10)
y <- c(-3.25, -3.37, -3.35, -3.20, -3.12, -3.02, -3.02,
       -3.07, -3.17, -3.32, -3.30, -3.22, -3.10)
xs <- seq(-60, 70, by = 1)

# Generate a function for this
pp <- cubicspline(x, y)
ppfun <- function(xs) ppval(pp, xs)

\dontrun{
# Plot with and without endpoint correction
plot(x, y, col = "darkblue",
           xlim = c(-60, 70), ylim = c(-3.5, -2.8),
           xlab = "Latitude", ylab = "Temp. Difference",
           main = "Earth Temperatures per Latitude")
lines(spline(x, y), col = "darkgray")
grid()

ys <- cubicspline(x, y, xs, endp2nd = TRUE)     # der = 0 at endpoints
lines(xs, ys, col = "red")
ys <- cubicspline(x, y, xs)                     # no endpoint condition
lines(xs, ys, col = "darkred")
}
}
\keyword{ fitting }
