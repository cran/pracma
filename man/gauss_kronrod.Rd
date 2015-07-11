\name{gauss_kronrod}
\alias{gauss_kronrod}
\title{
  Gauss-Kronrod Quadrature
}
\description{
  Simple Gaussian-Kronrod quadrature formula.
}
\usage{
gauss_kronrod(f, a, b, ...)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{a, b}{end points of the interval.}
  \item{...}{variables to be passed to the function.}
}
\details{
  Gaussian quadrature of degree 7 with Gauss-Kronrod of degree 15 for error
  estimation, the \code{quadQK15} procedure in the QUADPACK library.
}
\value{
  List of value and relative error.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\note{
  The function needs to be vectorized (though this could easily be changed),
  but the function does not need to be defined at the end points.
}
\seealso{
  \code{\link{quadgk}}, \code{\link{romberg}}
}
\examples{
gauss_kronrod(sin, 0, pi)  #  2.000000000000000 , rel.error: 1.14e-12
gauss_kronrod(exp, 0, 1)   #  1.718281828459045 , rel.error: 0
                           #  1.718281828459045 , i.e. exp(1) - 1
}
\keyword{ math }
