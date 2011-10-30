\name{fzsolve}
\alias{fzsolve}
\title{
  Complex Root Finding
}
\description{
  Find the root of a complex function
}
\usage{
fzsolve(fz, z0)
}
\arguments{
  \item{fz}{complex(-analytic) function.}
  \item{z0}{complex point near the assumed root.}
}
\details{
  \code{fzsolve} tries to find the root of the complex and relatively
  smooth (i.e., analytic) function near a starting point.

  The function is considered as real function \code{R^2 --> R^2} and the
  \code{newtonsys} function is applied.
}
\value{
  Complex point with sufficiently small function value.
}
\seealso{
  \code{\link{newtonsys}}
}
\examples{
fz <- function(z) sin(z)^2 + sqrt(z) - log(z)
fzsolve(fz, 1+1i)
# 0.2555197+0.8948303i
}
\keyword{ math }
