\name{quadQK15}
\alias{quadQK15}
\title{
  Gaussian Quadrature
}
\description{
  Gaussian quadrature.
}
\usage{
quadQK15(f, a, b, ...)
}
\arguments{
  \item{f}{function to be integrated.}
  \item{a, b}{end points of the interval.}
  \item{...}{variables to be passed to the function.}
}
\details{
  Gaussian quadrature of degree 15 with Gauss-Kronrod for error estimation.
}
\value{
  List of value and absolute error.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\note{
  The function needs to be vectorized (though this could easily be changed).
}
\seealso{
  \code{\link{romberg}}
}
\examples{
quadQK15(sin, 0, pi)  #  2.000000000001791 , rel.error: 1.14e-12
quadQK15(exp, 0, 1)   #  1.718281828459045 , rel.error: 0
                      #  1.718281828459045 , i.e. exp(1) - 1
}
\keyword{ math }
