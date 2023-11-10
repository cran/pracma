\name{Poly}
\alias{Poly}
\title{Define Polynomial by Roots}
\description{
  Define a polynomial by its roots.
}
\usage{
  Poly(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector or square matrix, real or complex}
}
\details{
  Computes the characteristic polynomial of an (n x n)-Matrix.

  If \code{x} is a vector, \code{Poly(x)} is the vector of coefficients
  of the polynomial whose roots are the elements of \code{x}.
}
\value{
  Vector representing a polynomial.
}
\note{
In Matlab/Octave this function is called \code{poly()}.
}
\seealso{
  \code{\link{polyval}}, \code{\link{roots}}
}
\examples{
  Poly(c(1, -1, 1i, -1i))  # Solves x^4 -1 = 0
  # Wilkinson's example:
  roots(Poly(1:20))
}
\keyword{ math }
