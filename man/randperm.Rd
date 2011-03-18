\name{randperm}
\alias{randperm}
\title{
Random Permutation
}
\description{
Generates a random permutation.
}
\usage{
randperm(a)
}
\arguments{
  \item{a}{numeric vector of some length \code{n}}
}
\details{
  Generates one random permutation of the elements \code{a}.
}
\value{
  vector of permuted elements of \code{a}
}
\note{
  This behavior is different from Matlab/Octave, but does better correspond
  with the behavior of the perms() function.
}
\seealso{
\code{\link{perms}}
}
\examples{
randperm(6)
randperm(1:6)
randperm(seq(2, 10, by=2))
}
\keyword{ arith }
