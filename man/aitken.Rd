\name{aitken}
\alias{aitken}
\title{
  Aitken' Method
}
\description{
  Aitken's acceleration method.
}
\usage{
aitken(f, x0, nmax = 12, tol = 1e-8, ...)
}
\arguments{
  \item{f}{Function with a fixpoint.}
  \item{x0}{Starting value.}
  \item{nmax}{Maximum number of iterations.}
  \item{tol}{Relative tolerance.}
  \item{...}{Additional variables passed to f.}
}
\details{
  Aitken's acceleration method, or delta-squared process, is used for
  accelerating the rate of convergence of a sequence (from linear to
  quadratic), here applied to the fixed point iteration scheme of a
  function.
}
\value{
  The fixpoint (as found so far).
}
\references{
  Quarteroni, A., and F. Saleri (2006). Scientific Computing with Matlab
  and Octave. Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  Sometimes used to accerate Newton-Raphson (Steffensen's method).
}
\seealso{
  \code{\link{lambertWp}}
}
\examples{
# Find a zero of    f(x) = cos(x) - x*exp(x)
# as fixpoint of  phi(x) = x + (cos(x) - x*exp(x))/2
phi <- function(x) x + (cos(x) - x*exp(x))/2
aitken(phi, 0)  #=> 0.5177574
}
\keyword{ math }
