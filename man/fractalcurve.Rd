\name{fractalcurve}
\alias{fractalcurve}
\title{
  Fractal Curves
}
\description{
  Generates the following fractal curves: Dragon Curve, Gosper Flowsnake 
  Curve, Hexagon Molecule Curve, Hilbert Curve, Koch Snowflake Curve, 
  Sierpinski Arrowhead Curve, Sierpinski (Cross) Curve, Sierpinski Triangle
  Curve.
}
\usage{
fractalcurve(n, which = c("hilbert", "sierpinski", "snowflake",
    "dragon", "triangle", "arrowhead", "flowsnake", "molecule"))
}
\arguments{
  \item{n}{integer, the `order' of the curve}
  \item{which}{character string, which curve to cumpute.}
}
\details{
  The Hilbert curve is a continuous curve in the plane with 4^N points.

  The Sierpinski (cross) curve is a closed curve in the plane with 4^(N+1)+1
  points.

  His arrowhead curve is a continuous curve in the plane with 3^N+1 points,
  and his triangle curve is a closed curve in the plane with 2*3^N+2 points.

  The Koch snowflake curve is a closed curve in the plane with 3*2^N+1
  points.

  The dragon curve is a continuous curve in the plane with 2^(N+1) points.

  The flowsnake curve is a continuous curve in the plane with 7^N+1 points.

  The hexagon molecule curve is a closed curve in the plane with 6*3^N+1
  points.
}
\value{
  Returns a list with \code{x, y} the x- resp. y-coordinates of the
  generated points describing the fractal curve.
}
\references{
  Peitgen, H.O., H. Juergens, and D. Saupe (1993). Fractals for the
  Classroom. Springer-Verlag Berlin Heidelberg.
}
\author{
  Copyright (c) 2011 Jonas Lundgren for the Matlab toolbox \code{fractal 
  curves} available on MatlabCentral under BSD license;
  here re-implemented in R with explicit allowance from the author.
}
\examples{
## The Hilbert curve transforms a 2-dim. function into a time series.
z <- fractalcurve(4, which = "hilbert")

\dontrun{
f1 <- function(x, y) x^2 + y^2
plot(f1(z$x, z$y), type = 'l', col = "darkblue", lwd = 2,
     ylim = c(-1, 2), main = "Functions transformed by Hilbert curves")

f2 <- function(x, y) x^2 - y^2
lines(f2(z$x, z$y), col = "darkgreen", lwd = 2)

f3 <- function(x, y) x^2 * y^2
lines(f3(z$x, z$y), col = "darkred", lwd = 2)
grid()}

\dontrun{
## Show some more fractal surves
n <- 8
opar <- par(mfrow=c(2,2), mar=c(2,2,1,1))

z <- fractalcurve(n, which="dragon")
x <- z$x; y <- z$y
plot(x, y, type='l', col="darkgrey", lwd=2)
title("Dragon Curve")

z <- fractalcurve(n, which="molecule")
x <- z$x; y <- z$y
plot(x, y, type='l', col="darkblue")
title("Molecule Curve")

z <- fractalcurve(n, which="arrowhead")
x <- z$x; y <- z$y
plot(x, y, type='l', col="darkgreen")
title("Arrowhead Curve")

z <- fractalcurve(n, which="snowflake")
x <- z$x; y <- z$y
plot(x, y, type='l', col="darkred", lwd=2)
title("Snowflake Curve")

par(opar)}
}
\keyword{ math }
