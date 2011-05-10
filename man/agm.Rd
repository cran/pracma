\name{agm}
\alias{agm}
\title{
  Arithmetic-geometric Mean
}
\description{
  The arithmetic-geometric mean of positive numbers.
}
\usage{
agm(a, b, maxiter = 25, tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{a, b}{Positive numbers.}
  \item{maxiter}{Maximum number of iterations.}
  \item{tol}{tolerance; stops when \code{abs(a-b) < tol}.}
}
\details{
  The arithmetic-geometric mean is defined as the common limit of the two
  sequences \eqn{a_{n+1} = (a_n + b_n)/2} and \eqn{b_{n+1} = \sqrt(a_n b_n)}.
}
\value{
  Returnes one value, the mean of the last two values \code{a}, \code{b}.
}
\references{
  \url{http://mathworld.wolfram.com/Arithmetic-GeometricMean.html}
}
\note{
  The AGM is also defined for negative values a and/or b, but then the
  complex square root has to be taken.
}
\seealso{
  Arithmetic, geometric, and harmonic mean.
}
\examples{
##  Gauss constant
1 / agm(1, sqrt(2), tol = 1e-15)$agm  # 0.834626841674073
}
\keyword{ math }
