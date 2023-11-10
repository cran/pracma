\name{hypot}
\alias{hypot}
\title{Hypotenuse Function}
\description{
Square root of sum of squares
}
\usage{
hypot(x, y)
}
\arguments{
  \item{x, y}{Vectors of real or complex numbers of the same size}
}
\details{
Element-by-element computation of the square root of the sum of squares
of vectors resp. matrices \code{x} and \code{y}.
}
\value{
Returns a vector or matrix of the same size.
}
\note{
Returns \code{c()} if \code{x} or \code{y} is empty and the other one has
length 1. If one input is scalar, the other a vector, the scalar will be
extended to a vector of appropriate length. In all other cases, \code{x}
and \code{y} have to be of the same size.
}
\examples{
hypot(3,4)
hypot(1, c(3, 4, 5))
hypot(c(0, 0), c(3, 4))
}
\keyword{ array }
