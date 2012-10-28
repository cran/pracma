\name{nextpow2}
\alias{nextpow2}
\title{Next Power of 2}
\description{
  Smallest power of 2 greater than the argument.
}
\usage{
  nextpow2(x)
}
\arguments{
  \item{x}{numeric scalar, vector, or matrix}

}
\details{
  Computes the smalest integer \code{n} such that \eqn{abs(x) \le 2^n}.
  IF \code{x} is a vector or matrix, returns the result component-wise.
  For negative or complex values, the absolute value will be taken.
}
\value{
  an integer \code{n} such that \eqn{x \le 2^n}.
}
\seealso{
  \code{\link{pow2}}
}
\examples{
  nextpow2(10)                   #=> 4
  nextpow2(1:10)                 #=> 0 1 2 2 3 3 3 3 4 4
  nextpow2(-2^10)                #=> 10
  nextpow2(.Machine$double.eps)  #=> -52
}
\keyword{ arith }
