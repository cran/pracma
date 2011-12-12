\name{rand}
\alias{rand}
\alias{randn}
\alias{randi}
\alias{randp}
\title{Create Random Matrices}
\description{
  Create random matrices or random points in a unit circle (Matlab style).
}
\usage{
rand(n = 1, m = n)
randn(n = 1, m = n)
randi(imax, n = 1, m = n)
randp(n = 1, r = 1)
}
\arguments{
  \item{n, m}{integers specifying the size of the matrix}
  \item{imax}{integer or pair of integers}
  \item{r}{radius of circle, default 1.}
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

  \code{randp()} generate uniformly random points in the unit circle (or in
  a circle of radius r). It does that by generating tuples of points and
  rejects those outside the circle. \code{n} can only be  a vector.
}
\value{
  Matrices of size \code{nxm} resp. a vector of length \code{n}.

  \code{randp()} returns a pair of values representing a point in the circle,
  or a matrix of size \code{(n,2)}.
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

U <- randp(1000, 2)
\dontrun{
plot(U[, 1], U[, 2], pch="+")}

#-- v is 2 independent normally distributed elements
# u <- randp(1); r <- t(u) %*% u
# v <- sqrt(-2 * log(r)/r) * u

n <- 5000; U <- randp(n)
R <- apply(U*U, 1, sum)
P <- sqrt(-2 * log(R)/R) * U  # rnorm(2*n)
\dontrun{
hist(c(P))}
}
\keyword{ stat }
