\name{randortho}
\alias{randortho}
\title{
  Generate Random Orthonormal or Unitary Matrix
}
\description{
  Generates random orthonormal or unitary matrix of size \code{n}.

  Will be needed in applications that explore high-dimensional data spaces,
  for example optimization procedures or Monte Carlo methods.
}
\usage{
randortho(n, type = c("orthonormal", "unitary"))
}
\arguments{
  \item{n}{positive integer.}
  \item{type}{orthonormal (i.e., real) or unitary (i.e., complex) matrix.}
}
\details{
  Generates orthonormal or unitary matrices \code{Q}, that is
  \code{t(Q)} resp \code{t(Conj(Q))} is inverse to \code{Q}. The randomness
  is meant with respect to the (additively invariant) Haar measure on
  \eqn{O(n)} resp. \eqn{U(n)}.

  Stewart (1980) describes a way to generate such matrices by applying
  Householder transformation. Here a simpler approach is taken based on the
  QR decomposition, see Mezzadri (2006),
}
\value{
  Orthogonal (or unitary) matrix \code{Q} of size \code{n}, that is
  \code{Q \%*\% t(Q)} resp. \code{Q \%*\% t(Conj(Q))} is the unit matrix
  of size \code{n}.
}
\note{
  \code{rortho} was deprecated and eventually removed in version 2.1.7.
}
\references{
G. W. Stewart (1980). ``The Efficient Generation of Random Orthogonal Matrices
with an Application to Condition Estimators''.
SIAM Journal on Numerical Analysis, Vol. 17, No. 3, pp. 403-409.

F. Mezzadri (2006). ``How to generate random matrices from the classical
compact groups''. NOTICES of the AMS, Vol. 54 (2007), 592-604.
(arxiv.org/abs/math-ph/0609050v2)
}
\examples{
Q <- randortho(5)
zapsmall(Q \%*\% t(Q))
zapsmall(t(Q) \%*\% Q)
}
\keyword{ math }
