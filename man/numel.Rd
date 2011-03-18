\name{numel}
\alias{numel}
\title{Number of Elements}
\description{
  Number of elements in a vector, matrix, or array.
}
\usage{
numel(x)
}
\arguments{
  \item{x}{a vector, matrix, array or list}
}
\value{
  the number of elements of \code{a}.
}
\seealso{
  \code{\link{size}}
}
\examples{
numel(c(1:12))
numel(matrix(1:12, 3, 4))
}
\keyword{array}

