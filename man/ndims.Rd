\name{ndims}
\alias{ndims}
\title{Number of Dimensions}
\description{
  Number of matrix or array dimensions.
}
\usage{
ndims(x)
}
\arguments{
  \item{x}{a vector, matrix, array, or list}
}
\details{
  Returns the number of dimensions as \code{length(x)}.

  For an empty object its dimension is 0, for vectors it is 1
  (deviating from MATLAB), for matrices it is 2, and for arrays
  it is the number of dimensions, as usual.
  Lists are considered to be (one-dimensional) vectors.
}
\value{
  the number of dimensions in a vector, matrix, or array \code{x}.
}
\seealso{
  \code{\link{size}}
}
\note{
  The result will differ from Matlab when \code{x} is a vector.
}
\examples{
ndims(c())                      # 0
ndims(as.numeric(1:8))          # 1
ndims(list(a=1, b=2, c=3))      # 1
ndims(matrix(1:12, 3, 4))       # 2
ndims(array(1:8, c(2,2,2)))     # 3
}
\keyword{array}

