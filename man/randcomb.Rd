\name{randcomb}
\alias{randcomb}
\title{
Random Combination
}
\description{
Generates a random combination.
}
\usage{
randcomb(a, m)
}
\arguments{
  \item{a}{numeric vector of some length \code{n}}
  \item{m}{integer with \code{0 <= m <= n}}
}
\details{
  Generates one random combination of the elements \code{a} of length
  \code{m}.
}
\value{
  vector of combined elements of \code{a}
}
\note{
  This behavior is different from Matlab/Octave, but does better correspond
  with the behavior of the perms() function.
}
\seealso{
\code{\link{combs}}, \code{\link{randperm}}
}
\examples{
randcomb(seq(2, 10, by=2), m = 3)
}
\keyword{ arith }
