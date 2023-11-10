\name{polytrans, polygcf}
\alias{polytrans}
\alias{polygcf}
\title{Polynomial Transformations}
\description{
  Transform a polynomial, find a greatest common factor, or determine the
  multiplicity of a root.
}
\usage{
polytrans(p, q)

polygcf(p, q, tol = 1e-12)
}
\arguments{
  \item{p, q}{vectors representing two polynomials.}
  \item{tol}{tolerance for coefficients to tolerate.}
}
\details{
  Transforms polynomial \code{p} replacing occurences of \code{x} with
  another polynomial \code{q} in \code{x}.

  Finds a greatest common divisor (or factor) of two polynomials.
  Determines the multiplicity of a possible root; returns 0 if not a root.
  This is in general only true to a certain tolerance.
}
\value{
  \code{polytrans} and \code{polygcf} return vectors representing polynomials.
  \code{rootsmult} returns a natural number (or 0).
}
\note{
  There are no such functions in Matlab or Octave.
}
\seealso{
  \code{\link{polyval}}
}
\examples{
# (x+1)^2 + (x+1) + 1
polytrans(c(1, 1, 1), c(1, 1))    #=> 1 3 3
polytrans(c(1, 1, 1), c(-1, -1))  #=> 1 1 1

p <- c(1,-1,1,-1,1)         #=>  x^4 - x^3 + x^2 - x + 1
q <- c(1,1,1)               #=>  x^2 + x + 1
polygcf(polymul(p, q), q)   #=>  [1] 1 1 1

p = polypow(c(1, -1), 6)    #=>  [1] 1  -6  15 -20  15  -6   1
rootsmult(p, 1)             #=>  [1] 6
}
\keyword{ math }
