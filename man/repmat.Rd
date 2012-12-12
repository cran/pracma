\name{repmat}
\alias{repmat}
\title{Replicate Matrix}
\description{
  Replicate and tile matrix.
}
\usage{
repmat(a, n, m = n)
}
\arguments{
  \item{a}{vector or matrix to be replicated.}
  \item{n, m}{number of times to replicate in each dimension.}
}
\details{
  \code{repmat(a,m,n)} creates a large matrix consisting of an m-by-n tiling
  of copies of \code{a}. 
}
\value{
  Returns matrix with value \code{a} replicated to the number of times
  in each dimension specified.
  Defaults to square if dimension argument resolves to a single value.
}
\seealso{ 
  \code{\link{Reshape}}
}   
\examples{
repmat(1, 3)                  # same as ones(3)
repmat(1, 3, 3)
repmat(matrix(1:4, 2, 2), 3)
}
\keyword{array}
