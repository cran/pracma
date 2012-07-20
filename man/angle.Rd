\name{angle}
\alias{Real}
\alias{Imag}
\alias{angle}
\title{Basic Complex Functions}
\description{
Basic complex functions (Matlab style)
}
\usage{
Real(z)
Imag(z)
angle(z)
}
\arguments{
  \item{z}{Vector or matrix of real or complex numbers}
}
\details{
These are just Matlab names for the corresponding functions in R. The
\code{angle} function is simply defined as \code{atan2(Im(z), Re(z))}.
}
\value{
returning real or complex values; \code{angle} returns in radians.
}
\note{
The true Matlab names are \code{real}, \code{imag}, and \code{conj}, but as
\code{real} was taken in R, all these beginnings are changed to capitals.

The function \code{Mod} has no special name in Matlab;
use \code{abs()} instead.
}
\seealso{
\code{\link{Mod}}, \code{\link{abs}}
}
\examples{
z <- c(0, 1, 1+1i, 1i)
Real(z)   # Re(z)
Imag(z)   # Im(z)
Conj(z)   # Conj(z)
abs(z)    # Mod(z)
angle(z)
}
\keyword{ arith }
