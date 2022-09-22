\encoding{utf8}
\name{circlefit}
\alias{circlefit}
\title{Fitting a Circle}
\description{
  Fitting a circle from points in the plane
}
\usage{
circlefit(xp, yp)
}
\arguments{
  \item{xp, yp}{Vectors representing the x and y coordinates of plane points}
}
\details{
  This routine finds an `algebraic' solution based on a linear fit.
  The value to be minimized is the distance of the given points to
  the nearest point on the circle along a radius.
}
\value{
  Returns x- and y-coordinates of the center and the radius as a vector
  of length 3.

  Writes the RMS error of the (radial) distance of the original points
  to the circle directly onto the console.
}
\references{
  Gander, W., G. H. Golub, and R. Strebel (1994). Fitting of Circles and
  Ellipses --- Least Squares Solutions. ETH Zürich, Technical Report 217,
  Institut für Wissenschaftliches Rechnen.
}
\examples{
# set.seed(8421)
n  <- 20
w  <- 2*pi*runif(n)
xp <- cos(w) + 1 + 0.25 * (runif(n) - 0.5)
yp <- sin(w) + 1 + 0.25 * (runif(n) - 0.5)

circe <- circlefit(xp, yp)  #=> 0.9899628 1.0044920 1.0256633
                            # RMS error: 0.07631986 
\dontrun{
x0 <- circe[1]; y0 <- circe[2]; r0 <- circe[3]
plot(c(-0.2, 2.2), c(-0.2, 2.2), type="n", asp=1)
grid()
abline(h=0, col="gray"); abline(v=0, col="gray")
points(xp, yp, col="darkred")

w  <- seq(0, 2*pi, len=100)
xx <- r0 * cos(w) + x0
yy <- r0 * sin(w) + y0
lines(xx, yy, col="blue")}
}
\keyword{ fitting }
