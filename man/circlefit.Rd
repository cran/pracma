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
  This routine first finds an `algebraic' solution based on a linear fit
  and then calls \code{optim} with this solution as starting point.

  The value to be minimized is the distance of the given points to the
  nearest point on the circle.
}
\value{
  Returns x- and y-coordinates of the center and the radius as a vector
  of length 3.

  Writes the RMS error of the distance of the original points to the circle
  directly onto the console.
}
\references{
  Gander, W., G. H. Golub, and R. Strebel (1994). Fitting of Circles and
  Ellipses --- Least Squares Solutions. ETH Z\"urich, Technical Report 217,
  Institut f\"ur Wissenschaftliches Rechnen.
}
\note{
  May be worth to apply \code{nls} instead of \code{optim}.
}
\examples{
# set.seed(8421)
n  <- 20
w  <- 2*pi*runif(n)
xp <- cos(w) + 1 + 0.25 * (runif(n) - 0.5)
yp <- sin(w) + 1 + 0.25 * (runif(n) - 0.5)

rslt <- circlefit(xp, yp)  #=> 0.9965782 1.0009066 1.0240452
x0 <- rslt[1]; y0 <- rslt[2]; r0 <- rslt[1]

\dontrun{
plot(c(-0.2, 2.2), c(-0.2, 2.2), type="n", asp=1)
grid()
abline(h=0, col="gray"); abline(v=0, col="gray")
points(xp, yp, col="darkred")

w  <- seq(0, 2*pi, len=100)
xx <- r0 * cos(w) + x0
yy <- r0 * sin(w) + y0
lines(xx, yy, col="blue")}
}
\keyword{ optimize }
