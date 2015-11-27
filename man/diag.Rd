\name{Diag}
\alias{Diag}
\title{Matrix Diagonal}
\description{
Generate diagonal matrices or return diagonal of a matrix
}
\usage{
Diag(x, k = 0)
}
\arguments{
  \item{x}{vector or matrix}
  \item{k}{integer indicating a secondary diagonal}
}
\details{
If \code{x} is a vector, \code{Diag(x, k)} generates a matrix with \code{x}
as the (k-th secondary) diagonal.

If \code{x} is a matrix, \code{Diag(x, k)} returns the (\code{k}-th secondary) diagonal of \code{x}.

The \code{k}-th secondary diagonal is above the main diagonal for \code{k > 0}
and below the main diagonal for \code{k < 0}.
}
\value{
matrix or vector
}
\note{
In Matlab/Octave this function is called \code{diag()} and has a different
signature than the corresponding function in \R.
}
\seealso{
\code{\link{diag}}, \code{\link{Trace}}
}
\examples{
Diag(matrix(1:12,3,4),  1)
Diag(matrix(1:12,3,4), -1)

Diag(c(1,5,9), 1)
Diag(c(1,5,9), -1)
}
\keyword{ array }
