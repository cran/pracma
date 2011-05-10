\name{fact}
\alias{fact}
\title{
  Factorial Function
}
\description{
  Factorial for non-negative integers \code{n <= 170}.
}
\usage{
fact(n)
}
\arguments{
  \item{n}{Vector of integers.}
}
\details{
  The factorial is computed by brute force; factorials for \code{n >= 171}
  are not representable as `double' anymore.
}
\value{
  Returns the factorial of each element in \code{n}. If \code{n < 0} the
  value is \code{NaN}, and for \code{n > 170} it is \code{Inf}.
  Non-integers will be reduced to integers through \code{floor(n)}.
}
\note{
  The R core function \code{factorial} uses the \code{gamma} function,
  whose implementation is not accurate enough for larger input values.
}
\seealso{
  \code{\link{factorial}}
}
\examples{
fact(c(-1, 0, 1, NA, 171))  #=> NaN   1   1  NA Inf
fact(100)                   #=> 9.332621544394410e+157
factorial(100)              #=> 9.332621544394225e+157
# correct value:                9.332621544394415e+157
# Stirling's approximation:     9.324847625269420e+157
# n! ~ sqrt(2*pi*n) * (n/e)^n
}
\keyword{ math }
