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

  A vector is seen as a matrix with one row,
  i.e. for a vector the number of dimensions is 2.
}
\value{
  the number of dimensions in the vector or array \code{x}.
}
\seealso{
  \code{\link{size}}
}
\note{
  The result will differ from Matlab when \code{x} is a character vector.
}
\examples{
ndims(1:8)
ndims(array(1:8, c(2,2,2)))
}
\keyword{array}

