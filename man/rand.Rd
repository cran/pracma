\name{rand}
\alias{rand}
\alias{randn}
\alias{randi}
\alias{randsample}
\alias{rands}
\alias{randp}
\title{Create Random Matrices}
\description{
  Create random matrices or random points in a unit circle (Matlab style).
}
\usage{
rand(n = 1, m = n)
randn(n = 1, m = n)
randi(imax, n = 1, m = n)
randsample(n, k, w = NULL, replacement = FALSE)

rands(n = 1, N = 1, r = 1)
randp(n = 1, r = 1)
}
\arguments{
  \item{n, m}{integers specifying the size of the matrix}
  \item{imax}{integer or pair of integers}
  \item{k}{number of elements to return.}
  \item{w}{weight vector, used for discrete probabilities.}
  \item{replacement}{logical; sampling with or without replacement.}
  \item{N}{dimension of a shere, N=1 for the unit circle}
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

  \code{randsample()} samples \code{k} elements from \code{1:n}, with or 
  without replacement, or returns a weighted sample (with replacement),
  using the weight vector \code{w} for probabilities.

  \code{rands()} generates uniformly random points on an \code{N}-sphere in 
  the \code{N+1}-dimensional space. To generate uniformly random points in the 
  \code{N}-dim. unit cube, take points in \code{S^{N-1}} und multiply with
  \code{unif(n)^(1/(N-1))}.

  \code{randp()} generates uniformly random points in the unit circle (or in
  a circle of radius r).
}
\value{
  Matrices of size \code{nxm} resp. a vector of length \code{n}.

  \code{randp()} returns a pair of values representing a point in the circle,
  or a matrix of size \code{(n,2)}. \code{rands()} returns a matrix of size
  \code{(n, N+1)} with all rows being vectors of length \code{1}.
}
\note{
  The Matlab style of setting a seed is not available; use R style
  \code{set.seed(...)}.
}
\references{
  Knuth, D. (1981). The Art of Computer programming; Vol. 2: Seminumerical
  Algorithms; Chapt. 3: Random Numbers. Addison-Wesley, Reading.
}
\seealso{
  \code{\link{set.seed}}
}
\examples{
rand(3)
randn(1, 5)
randi(c(1,6), 1, 10)
randsample(10, 5, replacement = TRUE, w = c(0,0,0, 1, 1, 1, 1, 0,0,0))

P <- rands(1000, N = 1, r = 2)
U <- randp(1000, 2)
\dontrun{
plot(U[, 1], U[, 2], pch = "+", asp = 1)
points(P, pch = ".")}

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
