\name{taylor}
\alias{taylor}
\title{
  Taylor Series Approximation
}
\description{
  Local polynomial approximation through Taylor series.
}
\usage{
taylor(f, x0, n = 4, ...)
}
\arguments{
  \item{f}{differentiable function.}
  \item{x0}{point where the series expansion will take place.}
  \item{n}{Taylor series order to be used; should be \code{n <= 8}.}
  \item{...}{more variables to be passed to function \code{f}.}
}
\details{
  Calculates the first four coefficients of the Taylor series through
  numerical differentiation and uses some polynomial `yoga'.
}
\value{
  Vector of length \code{n+1} representing a polynomial of degree \code{n}.
}
\note{
  TODO: Pade approximation.
}
\seealso{
  \code{\link{fderiv}}
}
\examples{
taylor(sin, 0, 4)  #=> -0.1666666  0.0000000  1.0000000  0.0000000
taylor(exp, 1, 4)  #=>  0.04166657 0.16666673 0.50000000 1.00000000 1.00000000

f <- function(x) log(1+x)
p <- taylor(f, 0, 4)
p                     # log(1+x) = 0 + x - 1/2 x^2 + 1/3 x^3 - 1/4 x^4 +- ...
                      # [1] -0.250004  0.333334 -0.500000  1.000000  0.000000

\dontrun{
x <- seq(-1.0, 1.0, length.out=100)
yf <- f(x)
yp <- polyval(p, x)
plot(x, yf, type = "l", col = "gray", lwd = 3)
lines(x, yp, col = "red")
grid()}
}
\keyword{ math }
