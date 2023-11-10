\name{zeta}
\alias{zeta}
\title{
  Riemann Zeta Function
}
\description{
  Riemann's zeta function valid in the entire complex plane.
}
\usage{
zeta(z)
}
\arguments{
  \item{z}{Real or complex number or a numeric or complex vector.}
}
\details{
  Computes the zeta function for complex arguments using a series expansion
  for Dirichlet's eta function.

  Accuracy is about 7 significant digits for \code{abs(z)<50},
  drops off with higher absolute values.
}
\value{
  Returns a complex vector of function values.
}
\references{
  Zhang, Sh., and J. Jin (1996). Computation of Special Functions.
  Wiley-Interscience, New York.
}
\note{
  Copyright (c) 2001 Paul Godfrey for a Matlab version available on
  Mathwork's Matlab Central under BSD license.
}
\seealso{
  \code{\link{gammaz}}, \code{\link{eta}}
}
\examples{
##  First zero on the critical line s = 0.5 + i t
\dontrun{
x <- seq(0, 20, len=1001)
z <- 0.5 + x*1i
fr <- Re(zeta(z))
fi <- Im(zeta(z))
fa <- abs(zeta(z))
plot(x, fa, type="n", xlim = c(0, 20), ylim = c(-1.5, 2.5),
     xlab = "Imaginary part (on critical line)", ylab = "Function value",
     main = "Riemann's Zeta Function along the critical line")
lines(x, fr, col="blue")
lines(x, fi, col="darkgreen")
lines(x, fa, col = "red", lwd = 2)
points(14.1347, 0, col = "darkred")
legend(0, 2.4, c("real part", "imaginary part", "absolute value"),
       lty = 1, lwd = c(1, 1, 2), col = c("blue", "darkgreen", "red"))
grid()}
}
\keyword{ specfun }
