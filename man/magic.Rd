\name{magic}
\alias{magic}
\title{Magic Square}
\description{
  Create a magic square.
}
\usage{
magic(n)
}
\arguments{
  \item{n}{numeric scalar specifying dimensions for the result;
           \code{n} must be a scalar greater than or equal to 3.}
}
\details{
  A magic square is a square matrix where all row and column sums and also
  the diagonal sums all have the same value.

  This value or the characteristic sum for a magic square of order \eqn{n}
  is \eqn{sum(1:n^2)/n}.
}
\value{
  Returns an \code{n}-by-\code{n} matrix constructed from
  the integers \code{1} through \code{N^2} with equal row and column sums.
}
\note{
  A magic square, scaled by its magic sum, is doubly stochastic.
}
\examples{
magic(3)
}
\author{
  P. Roebuck \email{roebuck@mdanderson.org} for the first R version in the
  package `matlab'. The version here is more R-like.
}
\keyword{array}
