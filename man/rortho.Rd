\name{rortho}
\alias{rortho}
\title{
  Generate Random Orthogogonal Matrix
}
\description{
  Generates random orthogonal matrix of size \code{n}.

  Will be needed in applications that explore high-dimensional data spaces,
  for example optimization procedures or Monte Carlo methods.
}
\usage{
rortho(n)
}
\arguments{
  \item{n}{positive integer.}
}
\details{
  Uses the idea of the Stewart 1980, ``The Efficient Generation of Random 
  Orthogonal Matrices with an Application to Condition Estimators'', to apply 
  the Householder transformation to a uniformly distributed random vector.
}
\value{
  Orthogonal matrix \code{Q} of size \code{n}, that is \code{Q \%*\% t(Q)}
  is the unit matrix of size \code{n}.
}
\examples{
Q <- rortho(5)
zapsmall(Q \%*\% t(Q))
zapsmall(t(Q) \%*\% Q)
}
\keyword{ math }
