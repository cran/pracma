\name{randperm}
\alias{randperm}
\title{
Random Permutation
}
\description{
Generates a random permutation.
}
\usage{
randperm(a, k)
}
\arguments{
  \item{a}{integer or numeric vector of some length \code{n}.}
  \item{k}{integer, smaller as \code{a} or \code{length(a)}.}
}
\details{
  Generates one random permutation of \code{k} of the elements \code{a}, if
  \code{a} is a vector, or of \code{1:a} if \code{a} is a single integer.
}
\value{
  Vector of permuted elements of \code{a} or \code{1:a}.
}
\note{
  This behavior is different from Matlab/Octave, but does better correspond
  with the behavior of the perms() function.
}
\seealso{
\code{\link{perms}}
}
\examples{
randperm(1:6, 3)
randperm(6, 6)
randperm(11:20, 5)
randperm(seq(2, 10, by=2))
}
\keyword{ arith }
