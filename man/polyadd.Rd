\name{polyadd}
\alias{polyadd}
\title{Adding Polynomials}
\description{
  Add two polynomials given as vectors.
}
\usage{
  polyadd(p, q)
}
\arguments{
  \item{p, q}{Vectors representing two polynomials.}
}
\details{
  Polynomial addition realized simply by multiplying and summing up
  all the coefficients after extending vectors to the same length.
}
\value{
  Vector representing a polynomial.
}
\note{
  There is no such function in Matlab or Octave.
}
\seealso{
  \code{\link{conv}}
}
\examples{
polyadd(c(1, 1, 1), 1)
polyadd(c(1, 1, 1), c(0, 0, 1))
polyadd(c(-0.5, 1, -1), c(0.5, 0, 1))
}
\keyword{ math }
