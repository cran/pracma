\name{combs, randcomb}
\alias{combs}
\alias{randcomb}
\title{
Generate Combinations
}
\description{
Generates all combinations of length \code{m} of a vector \code{a}.
}
\usage{
combs(a, m)
randcomb(a, m)
}
\arguments{
  \item{a}{numeric vector of some length \code{n}}
  \item{m}{integer with \code{0 <= m <= n}}
}
\details{
  \code{combs} generates combinations of length \code{n} of the elements
  of the vector \code{a}.

  \code{randcomb} generates just one random combination.
}
\value{
  matrix representing combunations of the elements of \code{a}
}
\seealso{
\code{\link{perms}}, \code{\link{randperm}}
}
\examples{
combs(seq(2, 10, by=2), m = 3)
randcomb(seq(2, 10, by=2), m = 3)
}
\keyword{ arith }
