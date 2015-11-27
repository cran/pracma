\name{charpoly}
\alias{charpoly}
\title{
  Characteristic Polynomial
}
\description{
  Computes the characteristic polynomial (and the inverse of the matrix, if
  requested) using the Faddeew-Leverrier method.
}
\usage{
charpoly(a, info = FALSE)
}
\arguments{
  \item{a}{quadratic matrix; size should not be much larger than 100.}
  \item{info}{logical; if true, the inverse matrix will also be reported.}
}
\details{
  Computes the characteristic polynomial recursively. In the last step
  the determinant and the inverse matrix can be determined without any
  extra cost (if the matrix is not singular).
}
\value{
  Either the characteristic polynomial as numeric vector, or a list with
  components \code{cp}, the characteristic polynomial, \code{det}, the
  determinant, and \code{inv}, the inverse matrix, will be returned.
}
\references{
  Hou, S.-H. (1998). Classroom Note: A Simple Proof of the Leverrier--Faddeev
  Characteristic Polynomial Algorithm, SIAM Review, 40(3), pp. 706--709.
}
\examples{
a <- magic(5)
A <- charpoly(a, info = TRUE)
A$cp
roots(A$cp)
A$det
zapsmall(A$inv \%*\% a)
}
\keyword{ array }
