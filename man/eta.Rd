\name{eta}
\alias{eta}
\title{
  Dirichlet Eta Function
}
\description{
  Dirichlet's eta function valid in the entire complex plane.
}
\usage{
eta(z)
}
\arguments{
  \item{z}{Real or complex number or a numeric or complex vector.}
}
\details{
  Computes the eta function for complex arguments using a series expansion.

  Accuracy is about 13 significant digits for \code{abs(z)<100},
  drops off with higher absolute values.
}
\value{
  Returns a complex vector of function values.
}
\references{
  Zhang, Sh., and J. Jin (1996). Computation of Special Functions.
  Wiley-Interscience, New York.
}
\note{
  Copyright (c) 2001 Paul Godfrey for a Matlab version available on
  Mathwork's Matlab Central under BSD license.
}
\seealso{
  \code{\link{gammaz}}, \code{\link{zeta}}
}
\examples{
z <- 0.5 + (1:5)*1i
eta(z)
z <- c(0, 0.5+1i, 1, 1i, 2+2i, -1, -2, -1-1i)
eta(z)
}
\keyword{ specfun }
