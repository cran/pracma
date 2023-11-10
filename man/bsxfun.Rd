\name{bsxfun}
\alias{bsxfun}
\alias{arrayfun}
\title{
  Elementwise Function Application (Matlab Style)
}
\description{
  Apply a binary function elementwise.
}
\usage{
  bsxfun(func, x, y)

  arrayfun(func, ...)
}
\arguments{
  \item{func}{function with two or more input parameters.}
  \item{x, y}{two vectors, matrices, or arrays of the same size.}
  \item{...}{list of arrays of the same size.}
}
\details{
  \code{bsxfun} applies element-by-element a binary function to two vectors,
  matrices, or arrays of the same size. For matrices, \code{sweep} is used for
  reasons of speed, otherwise \code{mapply}. (For arrays of more than two
  dimensions this may become very slow.)

  \code{arrayfun} applies \code{func} to each element of the arrays and
  returns an array of the same size.
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

f <- function(x, y) x[1] * y[1]     # function not vectorized
A <- matrix(c(2, 3, 5, 7), 2, 2)
B <- matrix(c(11, 13, 17, 19), 2, 2)
arrayfun(f, A, B)
}
\keyword{ math }
