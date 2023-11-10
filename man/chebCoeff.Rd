\name{chebCoeff}
\alias{chebCoeff}
\title{Chebyshev Polynomials}
\description{
  Chebyshev Coefficients for Chebyshev polynomials of the first kind.
}
\usage{
chebCoeff(fun, a, b, n)
}
\arguments{
  \item{fun}{function to be approximated.}
  \item{a, b}{endpoints of the interval.}
  \item{n}{an integer \code{>= 0}.}
}
\details{
  For a function \code{fun} on on the interval \code{[a, b]} determines the
  coefficients of the Chebyshev polynomials up to degree \code{n} that will
  approximate the function (in L2 norm).
}
\value{
  Vector of coefficients for the Chebyshev polynomials, from low to high
  degrees (see the example).
}
\references{
  Weisstein, Eric W. ``Chebyshev Polynomial of the First Kind."
  From MathWorld --- A Wolfram Web Resource.
  \url{https://mathworld.wolfram.com/ChebyshevPolynomialoftheFirstKind.html}
}
\note{
  See the ``Chebfun Project'' <https://www.chebfun.org/> by
  Nick Trefethen.
}
\seealso{
\code{\link{chebPoly}}, \code{\link{chebApprox}}
}
\examples{
##  Chebyshev coefficients for x^2 + 1
n <- 4
f2 <- function(x) x^2 + 1
cC <- chebCoeff(f2, -1, 1, n)  #  3.0   0  0.5   0   0
cC[1] <- cC[1]/2               # correcting the absolute Chebyshev term
                               # i.e.  1.5*T_0 + 0.5*T_2
cP <- chebPoly(n)              # summing up the polynomial coefficients
p <- cC \%*\% cP                 #  0 0 1 0 1
}
\keyword{ math }
