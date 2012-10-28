\name{bsxfun}
\alias{bsxfun}
\title{
  Elementwise Function Application (Matlab Style)
}
\description{
  Apply a binary function elementwise.
}
\usage{
  bsxfun(f, x, y, ...)
}
\arguments{
  \item{f}{function with two input parameters.}
  \item{x, y}{two vectors, matrices, or arrays of the same size.}
  \item{...}{optional parameters to be passed to the function.}
}
\details{
  Apply element-by-element a binary function to two vectors or matrices of
  the same size.
}
\value{
  The result will be a vector or matrix of the same size as \code{x, y}.
}
\note{
  The underlying function \code{mapply} can be applied in a more general
  setting with many function parameters:

  \code{mapply(f, x, y, z, ...)}

  but the array structure will not be preserved in this case.
}
\seealso{
  \code{\link{Vectorize}}
}
\examples{
X <- matrix(rep(1:10, each = 10), 10, 10)
Y <- t(X)
bsxfun("*", X, Y)  # multiplication table
}
\keyword{ math }
