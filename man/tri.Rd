\name{tri}
\alias{tril}
\alias{triu}
\title{
  Triangular Matrices (Matlab Style)
}
\description{
  Extract lower or upper triangular part of a matrix.
}
\usage{
tril(M, k = 0)
triu(M, k = 0)
}
\arguments{
  \item{M}{numeric matrix.}
  \item{k}{integer, indicating a secondary diagonal.}
}
\details{
  \code{tril}\cr
  Returns the elements on and below the kth diagonal of X, where k = 0 is
  the main diagonal, k > 0 is above the main diagonal, and k < 0 is below
  the main diagonal.

  \code{triu}\cr
  Returns the elements on and above the kth diagonal of X, where k = 0 is
  the main diagonal, k > 0 is above the main diagonal, and k < 0 is below
  the main diagonal.
}
\value{
  Matrix the same size as the input matrix.
}
\note{
  For \code{k==0} it is simply an application of the R functions
  \code{lower.tri} resp. \code{upper.tri}.
}
\seealso{
  \code{\link{Diag}}
}
\examples{
tril(ones(4,4), +1)
#    1  1  0  0
#    1  1  1  0
#    1  1  1  1
#    1  1  1  1

triu(ones(4,4), -1)
#    1  1  1  1
#    1  1  1  1
#    0  1  1  1
#    0  0  1  1
}
\keyword{ array }
