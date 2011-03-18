\name{angle}
\alias{real}
\alias{imag}
\alias{conj}
\alias{angle}
\title{Basic Complex Functions}
\description{
Basic complex functions (Matlab style)
}
\usage{
real(z)
imag(z)
conj(z)
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
The function \code{Mod} has no special name in Matlab;
use \code{abs()} instead.
}
\seealso{
\code{\link{Mod}}, \code{\link{abs}}
}
\examples{
z <- c(0, 1, 1+1i, 1i)
real(z)   # Re(z)
imag(z)   # Im(z)
conj(z)   # Conj(z)
abs(z)    # Mod(z)
angle(z)
}
\keyword{ arith }
