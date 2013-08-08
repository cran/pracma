\name{eps}
\alias{eps}
\title{
  Floating Point Relative Accuracy
}
\description{
  Distance from 1.0 to the next largest double-precision number.
}
\usage{
eps(x = 1.0)
}
\arguments{
  \item{x}{scalar or numerical vector or matrix.}
}
\details{
  \code{d=eps(x)} is the positive distance from \code{abs(x)} to the next 
  larger floating point number in double precision.

  If \code{x} is an array, \code{eps(x)} will return \code{eps(max(abs(x)))}.
}
\value{
  Returns a scalar.
}
\examples{
for (i in -5:5) cat(eps(10^i), "\n")
# 1.694066e-21 
# 1.355253e-20 
# 2.168404e-19 
# 1.734723e-18 
# 1.387779e-17 
# 2.220446e-16 
# 1.776357e-15 
# 1.421085e-14 
# 1.136868e-13 
# 1.818989e-12 
# 1.455192e-11 
}
\keyword{ arith }
