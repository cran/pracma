\name{eye}
\alias{eye}
\alias{ones}
\alias{zeros}
\title{Some Basic Matrices}
\description{
  Create basic matrices.
}
\usage{
eye(n, m = n)
ones(n, m = n)
zeros(n, m = n)
}
\arguments{
  \item{m, n}{numeric scalars specifying size of the matrix}
}
\value{
  Matrix of size \code{n x m}.
  Defaults to a square matrix if \code{m} is missing.

  No dropping of dimensions; if \code{n = 1}, still returns a matrix and
  not a vector.
}
\seealso{ 
  \code{\link{Diag}},
}   
\examples{
eye(3)
ones(3, 1)
zeros(1, 3)
}
\keyword{ array }

