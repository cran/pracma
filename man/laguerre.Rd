\name{laguerre}
\alias{laguerre}
\title{
  Laguerre's Method
}
\description{
  Laguerre's method for finding roots of complex polynomials.
}
\usage{
laguerre(p, x0, nmax = 25, tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{p}{real or complex vector representing a polynomial.}
  \item{x0}{real or complex point near the root.}
  \item{nmax}{maximum number of iterations.}
  \item{tol}{absolute tolerance.}
}
\details{
  Uses values of the polynomial and its first and second derivative.
}
\value{
  The root found, or a warning about the number of iterations.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\note{
  Computations are caried out in complex arithmetic, and it is possible
  to obtain a complex root even if the starting estimate is real.
}
\seealso{
  \code{\link{roots}}
}
\examples{
# 1 x^5 - 5.4 x^4 + 14.45 x^3 - 32.292 x^2 + 47.25 x - 26.46
p <- c(1.0, -5.4, 14.45, -32.292, 47.25, -26.46)
laguerre(p, 1)   #=> 1.2
laguerre(p, 2)   #=> 2.099987     (should be 2.1)
laguerre(p, 2i)  #=> 0+2.236068i  (+- 2.2361i, i.e sqrt(-5))
}
\keyword{ math }
