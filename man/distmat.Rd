\name{distmat}
\alias{distmat}
\title{Distance Matrix}
\description{
  Computes the Euclidean distance between rows of two matrices.
}
\usage{
distmat(X, Y)
}
\arguments{
  \item{X}{matrix of some size \code{m x k}; vector will be taken as row matrix.}
  \item{Y}{matrix of some size \code{n x k}; vector will be taken as row matrix.}
}
\details{
  Computes Euclidean distance between two vectors A and B as:

    \code{||A-B|| = sqrt ( ||A||^2 + ||B||^2 - 2*A.B )}

  and vectorizes to rows of two matrices (or vectors).
}
\value{
  matrix of size \code{m x n} if \code{x} is of size \code{m x k} and
  \code{y} is of size \code{n x k}.
}
\references{
  Matlab Central
}
\note{
  If \code{a} is \code{m x r} and \code{b} is \code{n x r} then 

    \code{apply(outer(a,t(b),"-"),c(1,4),function(x)sqrt(sum(diag(x*x))))}

  is the \code{m x n} matrix of distances between the \code{m} rows
  of \code{a} and \code{n} rows of \code{b}.

  This can be modified as necessary, if one wants to apply distances other
  than the euclidean.

  BUT: The code shown here is 10-100 times faster, utilizing the similarity
  between Euclidean distance and matrix operations.
}
\seealso{
\code{\link{dist}}
}
\examples{
A <- c(0.0, 0.0)
B <- matrix(c(
        0,0, 1,0, 0,1, 1,1), nrow=4, ncol = 2, byrow = TRUE)
distmat(A, B)  #=> 0 1 1 sqrt(2)

X <- matrix(rep(0.5, 5), nrow=1, ncol=5)
Y <- matrix(runif(50), nrow=10, ncol=5)
distmat(X, Y)
}
\keyword{ array }