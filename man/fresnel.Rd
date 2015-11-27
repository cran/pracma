\name{fresnelS/C}
\alias{fresnelS}
\alias{fresnelC}
\title{
  Fresnel Integrals
}
\description{
  (Normalized) Fresnel integrals S(x) and C(x)
}
\usage{
fresnelS(x)
fresnelC(x)
}
\arguments{
  \item{x}{numeric vector.}
}
\details{
  The \emph{normalized} Fresnel integrals are defined as
  \deqn{S(x) = \int_0^x \sin(\pi/2 \, t^2) dt}
  \deqn{C(x) = \int_0^x \cos(\pi/2 \, t^2) dt}

  This program computes the Fresnel integrals S(x) and C(x) using Fortran
  code by Zhang and Jin. The accuracy is almost up to Machine precision.

  The functions are not (yet) truly vectorized, but use a call to `apply'.
  The underlying function \code{.fresnel} (not exported) computes single
  values of \code{S(x)} and \code{C(x)} at the same time.
}
\value{
  Numeric vector of function values.
}
\references{
  Zhang, S., and J. Jin (1996). Computation of Special Functions.
  Wiley-Interscience.
}
\note{
  Copyright (c) 1996 Zhang and Jin for the Fortran routines, converted to
  Matlab using the open source project `f2matlab' by Ben Barrowes, posted to
  MatlabCentral in 2004, and then translated to R by Hans W. Borchers.
}
\seealso{
  \code{\link{gaussLegendre}}
}
\examples{
##  Compute Fresnel integrals through Gauss-Legendre quadrature
f1 <- function(t) sin(0.5 * pi * t^2)
f2 <- function(t) cos(0.5 * pi * t^2)
for (x in seq(0.5, 2.5, by = 0.5)) {
    cgl <- gaussLegendre(51, 0, x)
    fs <- sum(cgl$w * f1(cgl$x))
    fc <- sum(cgl$w * f2(cgl$x))
    cat(formatC(c(x, fresnelS(x), fs, fresnelC(x), fc),
        digits = 8, width = 12, flag = " ----"), "\n")
}

\dontrun{
xs <- seq(0, 7.5, by = 0.025)
ys <- fresnelS(xs)
yc <- fresnelC(xs)

##  Function plot of the Fresnel integrals
plot(xs, ys, type = "l", col = "darkgreen",
    xlim = c(0, 8), ylim = c(0, 1),
    xlab = "", ylab = "", main = "Fresnel Integrals")
lines(xs, yc, col = "blue")
legend(6.25, 0.95, c("S(x)", "C(x)"), col = c("darkgreen", "blue"), lty = 1)
grid()

##  The Cornu (or Euler) spiral
plot(c(-1, 1), c(-1, 1), type = "n",
    xlab = "", ylab = "", main = "Cornu Spiral")
lines(ys, yc, col = "red")
lines(-ys, -yc, col = "red")
grid()}
}
\keyword{ specfun }
