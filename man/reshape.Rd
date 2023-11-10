\name{Reshape}
\alias{Reshape}
\title{Reshape Matrix}
\description{
  Reshape matrix or vector.
}
\usage{
Reshape(a, n, m)
}
\arguments{
  \item{a}{matrix or vector}
  \item{n, m}{size of the result}
}
\details{
  \code{Reshape(a, n, m)} returns the n-by-m matrix whose elements are taken
  column-wise from \code{a}.

  An error results if \code{a} does not have \code{n*m} elements.
  If \code{m} is missing, it will be calculated from \code{n} and the
  size of \code{a}.
}   
\value{
  Returns matrix (or array) of the requested size containing the elements
  of \code{a}.
}
\examples{
a <- matrix(1:12, nrow=4, ncol=3)
Reshape(a, 6, 2)
Reshape(a, 6)     # the same
Reshape(a, 3, 4)
}
\keyword{array}

