\name{trigApprox}
\alias{trigApprox}
\title{
  Trigonometric Approximation
}
\description{
  Computes the trigonometric series.
}
\usage{
trigApprox(t, x, m)
}
\arguments{
  \item{t}{vector of points at which to compute the values of the
           trigonometric approximation.}
  \item{x}{data from \code{t=0} to \code{t=2*(n-1)*pi/n}.}
  \item{m}{degree of trigonometric regression.}
}
\details{
  Calls \code{trigPoly} to get the trigonometric coefficients and then
  sums the finite series.
}
\value{
  Vector of values the same length as \code{t}.
}
\note{
  TODO: Return an approximating function instead.
}
\seealso{
\code{\link{trigPoly}}
}
\examples{
\dontrun{
##  Example: Gauss' Pallas data (1801)
asc <- seq(0, 330, by = 30)
dec <- c(408, 89, -66, 10, 338, 807, 1238, 1511, 1583, 1462, 1183, 804)
plot(2*pi*asc/360, dec, pch = "+", col = "red", xlim = c(0, 2*pi), ylim = c(-500, 2000),
     xlab = "Ascension [radians]", ylab = "Declination [minutes]",
     main = "Gauss' Pallas Data")
grid()
points(2*pi*asc/360, dec, pch = "o", col = "red")
ts <- seq(0, 2*pi, len = 100)
xs <- trigApprox(ts ,dec, 1)
lines(ts, xs, col = "black")
xs <- trigApprox(ts ,dec, 2)
lines(ts, xs, col = "blue")
legend(3, 0, c("Trig. Regression of degree 1", "Trig. Regression of degree 2",
                  "Astronomical position"), col = c("black", "blue", "red"),
                lty = c(1,1,0), pch = c("", "", "+"))}
}
\keyword{ math }
