\name{polyval}
\alias{polyval}
\title{Evaluating a Polynomial}
\description{
  Evaluate polynomial given as a numeric vector.
}
\usage{
  polyval(p, x)
}
\arguments{
  \item{p}{vector representing a polynomial}
  \item{x}{vector of values where to evaluate the polynomial}
}
\details{
  Evaluate the polynomial given by \code{p} at the values specified
  by the elements of \code{x}. If \code{x} is a matrix, the polynomial
  will be evaluated at each element and a matrix returned.
}
\value{
  vector of values
}
\seealso{
  \code{\link{poly}}, \code{\link{roots}}
}
\examples{
  # Evaluate 3 x^2 + 2 x + 1 at x = 5, 7, and 9
  p = c(3, 2, 1);
  polyval(p, c(5, 7, 9))  # 86  162  262
}
\keyword{ math }
