\name{polymul, polydiv}
\alias{polymul}
\alias{polydiv}
\title{Multiplying and Dividing Polynomials}
\description{
  Multiply or divide two polynomials given as vectors.
}
\usage{
  polymul(p, q)

  polydiv(p, q)
}
\arguments{
  \item{p, q}{Vectors representing two polynomials.}
}
\details{
  Polynomial multiplication realized simply by multiplying and summing up
  all the coefficients. Division is an alias for \code{deconv}.
  Polynomials are defined from highest to lowest coefficient.
}
\value{
  Vector representing a polynomial. For division, it returns a list with
  'd' the result of the division and 'r' the rest.
}
\note{
  \code{conv} also realizes polynomial multiplication, through Fast Fourier
  Transformation, with the drawback that small imaginary parts may evolve.
  \code{deconv} can also be used for polynomial division.
}
\seealso{
  \code{conv}, \code{deconv}
}
\examples{
# Multiply x^2 + x + 1 with itself
polymul(c(1, 1, 1), c(0, 1, 1, 1))  #=> 1 2 3 2 1

polydiv(c(1, 2, 3, 2, 1), c(1, 1, 1))
#=> d = c(1,1,1); #=> r = c(0.000000e+00 -1.110223e-16)
}
\keyword{ math }
