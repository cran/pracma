\name{fzero}
\alias{fzero}
\alias{zeroin}
\title{
  Root Finding
}
\description{
  Find root of continuous function of one variable.
}
\usage{
fzero(f, x0, ..., maxiter = 100, tol = .Machine$double.eps^(1/2))

zeroin(f, interval, ..., tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{f}{function whose root is to be found.}
  \item{x0}{a point near the root or an interval giving end points.}
  \item{interval}{vector of length 2, defining a finite interval on the real line.}
  \item{maxiter}{maximum number of iterations.}
  \item{tol}{absolute tolerance.}
  \item{...}{additional variables to be passed to the function.}
}
\details{
  \code{fzero} tries to find a zero of \code{f} near \code{x0}, if \code{x0}
  is a scalar. Expands the interval until different signs are found at the
  endpoints or the maximum number of iterations is exceeded.

  If \code{x0} is a vector of length two, \code{fzero} assumes \code{x0} is
  an interval where the sign of \code{x0[1]} differs from the sign of
  \code{x0[1]}. An error occurs if this is not true.

  \code{zeroin} implements a version of the Brent-Dekker algorithm.

  This approach will not find zeroes of quadratic order.
}
\value{
  List with
  \item{x}{location of the root.}
  \item{fval}{function value at the root.}
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  \code{fzero} mimics the Matlab function of the same name 
}
\seealso{
  \code{\link{brentDekker}}, \code{\link{newtonRaphson}}
}
\examples{
fzero(sin, 3)                    # 3.141593 +- 1.25e-13
fzero(cos,c(1, 2))               # 1.570796 +- 1.02e-9
fzero(function(x) x^3-2*x-5, 2)  # 2.094551 +- 1.58e-11
}
\keyword{ math }
