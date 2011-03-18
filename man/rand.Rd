\name{rand}
\alias{rand}
\alias{randn}
\alias{randi}
\title{Create Random Matrices}
\description{
  Create random matrices.
}
\usage{
rand(n = 1, m = n)
randn(n = 1, m = n)
randi(imax, n = 1, m = n)
}
\arguments{
  \item{n, m}{integers specifying the size of the matrix}
  \item{imax}{integer or pair of integers}
}
\details{
  \code{rand()}, \code{randn()}, \code{randi()} create random matrices of
  size \code{n x m}, where the default is square matrices if \code{m} is
  missing.

  \code{rand()} uses the uniform distribution on \code{]0, 1[}, while 
  \code{randn()} uses the normal distribution with mean 0 and standard
  deviation 1.

  \code{randi()} generates integers between \code{imax[1]} and \code{imax[2]}
  resp. 1 and \code{imax}, if \code{imax} is a scalar.
}
\value{
  matrices of size \code{n x m} resp. a vector of length \code{n}
}
\note{
  The Matlab style of setting a seed is not available; use R style
  \code{set.seed(...)}.
}
\seealso{
  \code{\link[base]{set.seed}}
}
\examples{
rand(3)
randn(1, 5)
randi(c(1,6), 1, 10)
}
\keyword{ array }
