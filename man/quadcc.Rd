\name{quadcc}
\alias{quadcc}
\title{
  Adaptive Clenshaw-Curtis Quadrature
}
\description{
  Adaptive Clenshaw-Curtis Quadrature.
}
\usage{
quadcc(f, a, b, tol = .Machine$double.eps^0.5, ...)
}
\arguments{
  \item{f}{integrand as function, may have singularities at the endpoints.}
  \item{a, b}{endpoints of the integration interval.}
  \item{tol}{relative tolerence.}
  \item{\ldots}{Additional parameters to be passed to the function \code{f}.}
}
\details{
  Adaptive version of the Clenshaw-Curtis quadrature formula with an
  (4, 8)-point erroe term.
}
\value{
  List with two components, \code{value} the value of the integral and
  the relative error \code{error}.
}
\seealso{
  \code{clenshaw_curtis}
}
\examples{
\dontrun{
##  Dilogarithm function
flog <- function(t) log(1-t)/t
quadcc(flog, 1, 0, tol = 1e-12)
# 1.644934066848128 - pi^2/6 < 1e-13
}
}
\keyword{ math }
