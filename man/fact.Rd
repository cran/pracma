\name{fact}
\alias{fact}
\alias{factorial2}
\title{
  Factorial Function
}
\description{
  Factorial for non-negative integers \code{n <= 170}.
}
\usage{
fact(n)

factorial2(n)
}
\arguments{
  \item{n}{Vector of integers, for \code{fact}, resp. a single integer for
           \code{factorial2}.}
}
\details{
  The factorial is computed by brute force; factorials for \code{n >= 171}
  are not representable as `double' anymore.
}
\value{
  \code{fact} returns the factorial of each element in \code{n}.
  If \code{n < 0} the value is \code{NaN}, and for \code{n > 170}
  it is \code{Inf}.
  Non-integers will be reduced to integers through \code{floor(n)}.

  \code{factorial2} returns the product of all even resp. odd integers,
  depending on whether \code{n} is even or odd.
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

factorial2(8);  factorial2(9);  factorial2(10)  # 384   945  3840
factorial(10) / factorial2(10)                  # => factorial2(9)
}
\keyword{ math }
