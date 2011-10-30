\name{size}
\alias{size}
\title{Size of Matrix}
\description{
  Provides the dimensions of \code{x}.
}
\usage{
size(x, k)
}
\arguments{
  \item{x}{vector, matrix, or array}
  \item{k}{integer specifying a particular dimension}
}
\details{
  Returns the number of dimensions as \code{length(x)}.

  Vector will be treated as a single row matrix.
}
\value{
  vector containing the dimensions of \code{x}, or the \code{k}-th dimension
  if \code{k} is not missing.
}
\note{
  The result will differ from Matlab when \code{x} is a character vector.
}
\seealso{
\code{\link[base]{dim}}
}
\examples{
size(1:8)
size(matrix(1:8, 2, 4))		# 2 4
size(matrix(1:8, 2, 4), 2)	# 4
size(matrix(1:8, 2, 4), 3)	# 1
}
\keyword{array}

