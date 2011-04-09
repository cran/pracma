\name{roots}
\alias{roots}
\title{Polynomial Roots}
\description{
  Computes the roots of the polynomial \code{p}.
}
\usage{
  roots(p)
}
\arguments{
  \item{p}{vector of real numbers representing the polynomial}
}
\details{
  For solution the function computes the eigenvalues of the companion matrix.

  At the moment, only real coefficients are allowed.
}
\value{
  a vector holding the roots of the polynomial
}
\seealso{
  \code{\link{polydef}}, \code{\link{compan}}
}
\examples{
  roots(c(1, 0, 1, 0, 0))       # 0 0 1i -1i
  p <- polydef(c(-2, -1, 0, 1, 2))
  roots(p)
}
\keyword{ math }
