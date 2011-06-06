\name{conv}
\alias{conv}
\title{Polynomial Convolution}
\description{
  Convolution and polynomial multiplication.
}
\usage{
conv(x, y)
}
\arguments{
  \item{x, y}{real or complex vectors.}
}
\details{
  \code{r = conv(p,q)} convolves vectors \code{p} and \code{q}.
  Algebraically, convolution is the same operation as multiplying the
  polynomials whose coefficients are the elements of \code{p} and \code{q}.
}
\value{
  Another vector.
}
\note{
  \code{conv} utilizes fast Fourier transformation.
}
\seealso{
  \code{\link{deconv}}, \code{\link{polyadd}}
}
\examples{
conv(c(1, 1, 1), 1)
conv(c(1, 1, 1), c(0, 0, 1))
conv(c(-0.5, 1, -1), c(0.5, 0, 1))
}
\keyword{ timeseries }
