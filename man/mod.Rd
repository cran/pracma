\name{mod, rem}
\alias{mod}
\alias{rem}
\alias{idiv}
\title{Integer Division}
\description{
  Integer division functions and remainders
}
\usage{
idiv(n, m)
mod(n, m)
rem(n, m)
}
\arguments{
  \item{n}{numeric vector (preferably of integers)}
  \item{m}{must be a scalar integer (positive, zero, or negative)}
}
\details{
  \code{idiv(n, m)} is integer division, with the same effect as
  \code{n \%/\% m}.

  \code{mod(n, m)} is the modulo operator and returns \eqn{n mod m}.
  \code{mod(n, 0)} is \code{n}, and the result always has the same sign
  as \code{m}.

  \code{rem(n, m)} is the same modulo operator and returns \eqn{n mod m}.
  \code{mod(n, 0)} is \code{NaN}, and the result always has the same sign
  as \code{n}.
}
\value{
  a numeric (integer) value or vector/matrix
}
\note{
  The following relation is fulfilled (for \code{m != 0}):

  \code{mod(n, m) = n - m * floor(n/m)}
}
\seealso{
Binary R operators \code{\%/\%} and \code{\%\%}.
}
\examples{
idiv(20, 6)
mod(c(-5:5), 5)
rem(c(-5:5), 5)
}
\keyword{ arith }
