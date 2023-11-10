\name{polyint}
\alias{polyint}
\title{Anti-derivative of Polynomial}
\description{
  Integrate polynomials.
}
\usage{
  polyint(p, k)
}
\arguments{
  \item{p}{polynomial \code{p} given as a vector}
  \item{k}{an integration constant}
}
\details{
  Calculates the integral, i.e. the antiderivative, of a polynomial
  and adds a constant of integration \code{k} if given, else 0.
}
\value{
  a vector representing a polynomial
}
\seealso{
  \code{\link{polyval}}, \code{\link{polyder}}
}
\examples{
  polyint(c(1, 1, 1, 1, 1), 1)
}
\keyword{ math }
