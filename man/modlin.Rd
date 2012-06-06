\name{modlin}
\alias{modlin}
\title{Modular Linear Equation Solver}
\description{
  Solves the modular equation \code{a x = b mod n}.
}
\usage{
modlin(a, b, n)
}
\arguments{
  \item{a, b, n}{integer scalars}
}
\details{
  Solves the modular equation \code{a x = b mod n}. This eqation is solvable
  if and only if \code{gcd(a,n)|b}. The function uses the extended greatest
  common divisor approach.
}
\value{
  Returns a vector of integer solutions.
}
\seealso{
\code{\link{extGCD}}
}
\examples{
modlin(14, 30, 100)             # 95 45
modlin(3, 4, 5)                 # 3
modlin(3, 5, 6)                 # []
modlin(3, 6, 9)                 # 2 5 8
}
\keyword{ arith }
