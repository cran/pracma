\name{poly}
\alias{poly}
\title{Define Polynomial by Roots}
\description{
  Define a polynomial by its roots.
}
\usage{
  poly(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector or square matrix}
}
\details{
  Computes the characteristic polynomial of an (n x n)-Matrix.

  If \code{x} is a vector, \code{poly(x)} is the vector of coefficients
  of the polynomial whose roots are the elements of \code{x}.
}
\value{
  Vector representing a polynomial.
}
\note{
In Matlab/Octave this function is also called \code{poly()}.
}
\seealso{
  \code{\link{polyval}}, \code{\link{roots}}
}
\examples{
  poly(c(1, -1, 1i, -1i))  # Solves x^4 -1 = 0
  # Wilkinson's example:
  roots(poly(1:20))
}
\keyword{ math }
