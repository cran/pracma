\name{polyder}
\alias{polyder}
\title{Derivative of Polynomial}
\description{
  Differentiate polynomials.
}
\usage{
  polyder(p, q)
}
\arguments{
  \item{p}{polynomial \code{p} given as a vector}
  \item{q}{polynomial \code{p} given as a vector}
}
\details{
  Calculates the derivative of polynomials and polynomial products.

  \code{polyder(p)} returns the derivative of \code{p} while
  \code{polyder(p, q)} returns the derivative of the product of the
  polynomials \code{p} and \code{q}.
}
\value{
  a vector representing a polynomial
}
\seealso{
    \code{\link{polyval}}, \code{\link{polyint}}
}
\examples{
  polyder(c(3, 6, 9), c(1, 2, 0))  # 12 36 42 18
}
\keyword{ math }
