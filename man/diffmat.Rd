\name{diffmat}
\alias{diffmat}
\title{
  Difference Matrix
}
\description{
  Difference matrix of a numeric vector.
}
\usage{
diffmat(x)
}
\arguments{
  \item{x}{Numeric vector.}
}
\details{
  The vector \code{x} sits in the diagonal, and the i-th difference vector
  in both i-th secondary diagonals.
}
\value{
  Numeric n-by-n matrix, where \code{n=length(x)}.
}
\examples{
x <- c(1,2,3,5,7,11,13,17,19)
(A <- diffmat(x))
}
\keyword{ array }
