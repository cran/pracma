\name{deconv}
\alias{deconv}
\title{Deconvolution}
\description{
  Deconvolution and polynomial division.
}
\usage{
deconv(b, a)
}
\arguments{
  \item{b, a}{real or complex vectors.}
}
\details{
  \code{deconv(b,a)} deconvolves vector \code{a} out of vector \code{b}.
  The quotient is returned in vector \code{q} and the remainder in vector
  \code{r} such that \code{b = conv(a,q)+r}.

  If \code{b} and \code{a} are vectors of polynomial coefficients,
  convolving them is equivalent to multiplying the two polynomials,
  and deconvolution is polynomial division.
}
\value{
  List with elements named \code{q} and \code{r}.
}
\note{
  TODO: Base \code{deconv} on some \code{filter1d} function.
}
\seealso{
  \code{\link{conv}}, \code{\link{polymul}}
}
\examples{
b <- c(10, 40, 100, 160, 170, 120)
a <- c(1, 2, 3, 4)

p <- deconv(b, a)
p$q                #=> 10 20 30
p$r                #=>  0  0  0
}
\keyword{ timeseries }
