\name{pow2}
\alias{pow2}
\title{Base 2 Power}
\description{
  Power with base 2.
}
\usage{
  pow2(f, e)
}
\arguments{
  \item{f}{numeric vector of factors}
  \item{e}{numeric vector of exponents for base 2}
}
\details{
  Computes the expression \code{f * 2^e}, setting \code{e} to \code{f}
  and \code{f} to 1 in case \code{e} is missing.
  Complex values are only processed if \code{e} is missing.
}
\value{
  Returns a numeric vector computing \eqn{f\,2^e}.
}
\seealso{
  \code{\link{nextpow2}}
}
\examples{
  pow2(c(0, 1, 2, 3))                   #=> 1 2 4 8
  pow2(c(0, -1, 2, 3), c(0,1,-2,3))     #=> 0.0 -2.0  0.5 24.0
  pow2(1i)                              #=> 0.7692389+0.6389613i
}
\keyword{ arith }
