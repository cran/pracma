\name{polycnv}
\alias{polycnv}
\title{Multiplying Polynomials}
\description{
  Multiply two polynomials given as vectors.
}
\usage{
  polycnv(p, q)
}
\arguments{
  \item{p, q}{vectors representing two polynomials}
}
\details{
  Polynomial multiplication realized simply by multiplying and summing up
  all the coefficients.
}
\value{
  Vector representing a polynomial.
}
\note{
  \code{conv} also realizes polynomial multiplication, through Fast Fourier
  Transformation, with the drawback that small imaginary parts may evolve.
}
\seealso{
  \code{conv}, \code{deconv}
}
\examples{
# Multiply x^2 + x + 1 with itself
polycnv(c(1, 1, 1), c(0, 1, 1, 1))  #=> 1 2 3 2 1
}
\keyword{ math }
