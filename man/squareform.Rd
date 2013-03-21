\name{squareform}
\alias{squareform}
\title{
  Format Distance Matrix (Matlab Style)
}
\description{
  Format or generate a distance matrix.
}
\usage{
squareform(x)
}
\arguments{
  \item{x}{numeric vector or matrix.}
}
\details{
  If \code{x} is a vector as created by the \code{dist} function, it converts
  it into a fulll square, symmetric matrix.
  And if \code{x} is a distance matrix, i.e. square, symmetric amd with zero
  diagonal elements, it returns the flattened lower triangular submatrix.
}
\value{
  Returns a matrix if \code{x} is a vector,
  and a vextor if \code{x} is a matrix.
}
\seealso{
  \code{\link{dist}}
}
\examples{
x <- 1:6
y <- squareform(x)
#  0  1  2  3
#  1  0  4  5
#  2  4  0  6
#  3  5  6  0
all(squareform(y) == x)
# TRUE
}
\keyword{ array }
