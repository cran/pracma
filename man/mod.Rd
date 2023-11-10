\name{mod, rem}
\alias{mod}
\alias{rem}
\alias{idivide}
\title{Integer Division}
\description{
  Integer division functions and remainders
}
\usage{
mod(n, m)
rem(n, m)

idivide(n, m, rounding = c("fix", "floor", "ceil", "round"))
}
\arguments{
  \item{n}{numeric vector (preferably of integers)}
  \item{m}{must be a scalar integer (positive, zero, or negative)}
  \item{rounding}{rounding mode.}
}
\details{
  \code{mod(n, m)} is the modulo operator and returns \eqn{n\,mod\,m}.
  \code{mod(n, 0)} is \code{n}, and the result always has the same sign
  as \code{m}.

  \code{rem(n, m)} is the same modulo operator and returns \eqn{n\,mod\,m}.
  \code{mod(n, 0)} is \code{NaN}, and the result always has the same sign
  as \code{n}.

  \code{idivide(n, m)} is integer division, with the same effect as
  \code{n \%/\% m} or using an optional rounding mode.
}
\value{
  a numeric (integer) value or vector/matrix.
}
\note{
  The following relation is fulfilled (for \code{m != 0}):

  \code{mod(n, m) = n - m * floor(n/m)}
}
\seealso{
Binary R operators \code{\%/\%} and \code{\%\%}.
}
\examples{
mod(c(-5:5), 5)
rem(c(-5:5), 5)

idivide(c(-2, 2), 3, "fix")     #  0 0
idivide(c(-2, 2), 3, "floor")   # -1 0
idivide(c(-2, 2), 3, "ceil")    #  0 1
idivide(c(-2, 2), 3, "round")   # -1 1
}
\keyword{ arith }
