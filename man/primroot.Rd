\name{primroot}
\alias{primroot}
\title{
  Primitive Root
}
\description{
  Find the smallest primitive root modulo m.
}
\usage{
primroot(m)
}
\arguments{
  \item{m}{A prime integer.}
}
\details{
  For every prime number \eqn{m} there exists a natural number \eqn{n} that
  generates the field \eqn{F_m}, i.e. \eqn{n, n^2, ..., n^{m-1} mod m} are
  all different.

  The computation here is all brute force. As most primitive roots are
  relatively small, it is still reasonable fast.

  One trick is to factorize \eqn{m-1} and test only for those prime factors.
  In R this is not more efficient as factorization also takes some time.
}
\value{
  A natural number if \code{m} is prime, else \code{NA}.
}
\references{
  Arndt, J. (2010). Matters Computational: Ideas, Algorithms, Source Code.
  Springer-Verlag, Berlin Heidelberg Dordrecht.
}
\author{
  HwB  <hwborchers@googlemail.com>
}
\note{
  This function is \emph{not} vectorized.
}
\seealso{
  \code{\link{modpower}}, \code{\link{modorder}}
}
\examples{
P <- primes(100)
R <- c()
for (p in P) {
    R <- c(R, primroot(p))
}
cbind(P, R)  # 7 is the biggest prime root here (for p=71)
}
\keyword{ math }
