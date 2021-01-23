\name{brown72}
\alias{brown72}
\docType{data}
\title{
  Brownian Motion
}
\description{
  The Brown72 data set represents a fractal Brownian motion with
  a prescribed Hurst exponent 0f 0.72 .
}
\usage{
data(brown72)
}
\format{
  The format is: one column.
}
\details{
  Estimating the Hurst exponent for a data set provides a measure of whether
  the data is a pure random walk or has underlying trends. Brownian walks can
  be generated from a defined Hurst exponent.
}
\examples{
\dontrun{
data(brown72)
plot(brown72, type = "l", col = "blue")
grid()}
}
\keyword{ datasets }
