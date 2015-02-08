\name{vander}
\alias{vander}
\title{Vandermonde matrix}
\description{
  Generate Vandermonde matrix from a numeric vector.
}
\usage{
  vander(x)
}
\arguments{
  \item{x}{Numeric vector}
}
\details{
  Generates the usual Vandermonde matrix from a numeric vector, e.g.
  applied when fitting a polynomial to given points.
  Complex values are allowed.
}
\value{
  Vandermonde matrix of dimension n where \code{n = length(x)}.
}
\examples{
  vander(c(1:10))
}
\keyword{ specmat }
