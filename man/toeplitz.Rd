\name{Toeplitz}
\alias{Toeplitz}
\title{Toeplitz Matrix}
\description{
  Generate Toeplitz matrix from column and row vector.
}
\usage{
Toeplitz(a, b)
}
\arguments{
  \item{a}{vector that will be the first column}
  \item{b}{vector that if present will form the first row.}
}
\details{
  \code{Toeplitz(a, b)} returns a (non-symmetric) Toeplitz matrix whose
  first column is \code{a} and whose first row is \code{b}. The following
  rows are shifted to the left.

  If the first element of \code{b} differs from the last element of \code{a}
  it is overwritten by this one (and a warning sent).
}
\value{
  Matrix of size \code{(length(a), length(b))}.
}
\note{
  \code{stats::Toeplitz} does not allow to specify the row vector, that is
  returns only the \emph{symmetric} Toeplitz matrix.
}
\seealso{
  \code{\link{hankel}}
}
\examples{
Toeplitz(c(1, 2, 3, 4, 5))
Toeplitz(c(1, 2, 3, 4, 5), c(1.5, 2.5, 3.5, 4.5, 5.5))
}
\keyword{ array }
