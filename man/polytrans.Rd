\name{polytrans}
\alias{polytrans}
\title{Polynomial Transformation}
\description{
  Transform a polynomial.
}
\usage{
polytrans(p, q)
}
\arguments{
  \item{p, q}{vectors representing two polynomials.}
}
\details{
  Transforms polynomial \code{p} replacing occurences of \code{x} with
  another polynomial \code{q} in \code{x}.
}
\value{
  Vector representing a polynomial.
}
\note{
  There is no such function in Matlab or Octave.
}
\seealso{
  \code{\link{polypow}}
}
\examples{
# (x+1)^2 + (x+1) + 1
polytrans(c(1, 1, 1), c(1, 1))    #=> 1 3 3
polytrans(c(1, 1, 1), c(-1, -1))  #=> 1 1 1
}
\keyword{ math }
