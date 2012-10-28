\name{spinterp}
\alias{spinterp}
\title{
  Monotone (Shape-Preserving) Interpolation
}
\description{
  Monotone interpolation preserves the monotonicity of the data being
  interpolated, and when the data points are also monotonic, the slopes 
  of the interpolant should also be monotonic.
}
\usage{
spinterp(x, y, xp)
}
\arguments{
  \item{x, y}{x- and y-coordinates of the points that shall be interpolated.}
  \item{xp}{points that should be interpolated.}
}
\details{
  This implementation follows a cubic version of the method of Delbourgo and
  Gregory. It yields `shaplier' curves than the Stineman method.

  The calculation of the slopes is according to recommended practice:

  - monotonic and convex --> harmonic\cr
  - monotonic and nonconvex --> geometric\cr
  - nonmonotonic and convex --> arithmetic\cr
  - nonmonotonic and nonconvex --> circles (Stineman) [not implemented]

  The choice of supplementary coefficients \code{r[i]} depends on whether
  the data are montonic or convex or both:

  - monotonic, but not convex\cr
  - otherwise

  and that can be detected from the data. The choice \code{r[i]=3} for all
  \code{i} results in the standard cubic Hermitean rational interpolation.
}
\value{
  The interpolated values at all the points of \code{xp}.
}
\references{
  Stan Wagon (2010). Mathematica in Action. Third Edition, Springer-Verlag.
}
\note{
  At the moment, the data need to be monotonic and the case of convexity
  is not considered.
}
\seealso{
  \code{stinepack::stinterp}, \code{demography::cm.interp}
}
\examples{
data1 <- list(x = c(1,2,3,5,6,8,9,11,12,14,15),
              y = c(rep(10,6), 10.5,15,50,60,95))
data2 <- list(x = c(0,1,4,6.5,9,10),
              y = c(10,4,2,1,3,10))
data3 <- list(x = c(7.99,8.09,8.19,8.7,9.2,10,12,15,20),
              y = c(0,0.000027629,0.00437498,0.169183,0.469428,
                    0.94374,0.998636,0.999919,0.999994))
data4 <- list(x = c(22,22.5,22.6,22.7,22.8,22.9,
                    23,23.1,23.2,23.3,23.4,23.5,24),
              y = c(523,543,550,557,565,575,
                    590,620,860,915,944,958,986))
data5 <- list(x = c(0,1.1,1.31,2.5,3.9,4.4,5.5,6,8,10.1),
              y = c(10.1,8,4.7,4.0,3.48,3.3,5.8,7,7.7,8.6))

data6 <- list(x = c(-0.8, -0.75, -0.3, 0.2, 0.5),
              y = c(-0.9,  0.3,   0.4, 0.5, 0.6))
data7 <- list(x = c(-1, -0.96, -0.88, -0.62, 0.13, 1),
              y = c(-1, -0.4,   0.3,   0.78, 0.91, 1))

data8 <- list(x = c(-1, -2/3, -1/3, 0.0, 1/3, 2/3, 1),
              y = c(-1, -(2/3)^3, -(1/3)^3, -(1/3)^3, (1/3)^3, (1/3)^3, 1))

\dontrun{
opr <- par(mfrow=c(2,2))

# These are well-known test cases:
D <- data1
plot(D, ylim=c(0, 100)); grid()
xp <- seq(1, 15, len=51); yp <- spinterp(D$x, D$y, xp)
lines(spline(D), col="blue")
lines(xp, yp, col="red")

D <- data3
plot(D, ylim=c(0, 1.2)); grid()
xp <- seq(8, 20, len=51); yp <- spinterp(D$x, D$y, xp)
lines(spline(D), col="blue")
lines(xp, yp, col="red")

D <- data4
plot(D); grid()
xp <- seq(22, 24, len=51); yp <- spinterp(D$x, D$y, xp)
lines(spline(D), col="blue")
lines(xp, yp, col="red")

# Fix a horizontal slope at the end points
D <- data8
x <- c(-1.05, D$x, 1.05); y <- c(-1, D$y, 1)
plot(D); grid()
xp <- seq(-1, 1, len=101); yp <- spinterp(x, y, xp)
lines(spline(D, n=101), col="blue")
lines(xp, yp, col="red")

par(opr)}
}
\keyword{ math }
