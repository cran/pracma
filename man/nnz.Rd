\name{nnz}
\alias{nnz}
\title{Nonzero Elements}
\description{
  Number of non-zero elements.
}
\usage{
nnz(x)
}
\arguments{
  \item{x}{a numeric or complex vector or matrix.}
}
\value{
  the number of non-zero elements of \code{x}.
}
\seealso{
  \code{\link{find}}
}
\examples{
nnz(diag(10))
}
\keyword{array}
