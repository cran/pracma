\name{hankel}
\alias{hankel}
\title{Hankel Matrix}
\description{
Generate Hankel matrix from column and row vector
}
\usage{
  hankel(a, b)
}
\arguments{
  \item{a}{vector that will be the first column}
  \item{b}{vector that if present will form the last row.}
}
\details{
\code{hankel(a)} returns the square Hankel matrix whose first column is
\code{a} and whose elements are zero below the secondary diagonal. (I.e.:
\code{b} may be missing.)

\code{hankel(a, b)} returns a Hankel matrix whose first column is \code{a}
and whose last row is \code{b}. If the first element of \code{b} differs
from the last element of \code{a} it is overwritten by this one.
}
\value{
  matrix of size \code{(length(a), length(b))}
}
\seealso{
  \code{\link{Toeplitz}}, \code{\link{hadamard}}
}
\examples{
hankel(1:5, 5:1)
}
\keyword{ specmat }
