\name{perms}
\alias{perms}
\title{
Generate Permutations
}
\description{
Generates all permutations of a vector \code{a}.
}
\usage{
perms(a)
}
\arguments{
  \item{a}{numeric vector of some length \code{n}}
}
\details{
  If \code{a} is a vector of length \code{n}, generate all permutations
  of the elements in \code{a} as a matrix of size \code{n! x n} where
  each row represents one permutation.

  A matrix will be expanded as vector.
}
\value{
  matrix of permutations of the elements of \code{a}
}
\note{
Not feasible for \code{length(a) > 10}.
}
\seealso{
\code{\link{randperm}}
}
\examples{
perms(6)
perms(1:6)
perms(c(1, exp(1), pi))
}
\keyword{ arith }
