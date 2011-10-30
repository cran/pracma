\name{fminbnd}
\alias{fminbnd}
\title{
  Minimum Finding
}
\description{
  Find minimum of single-variable function on fixed interval.
}
\usage{
fminbnd(f, x1, x2, ..., minimize = TRUE, tol = .Machine$double.eps^(2/3))
}
\arguments{
  \item{f}{function whose minimum or maximum is to be found.}
  \item{x1, x2}{endpoints of the interval to be searched.}
  \item{minimize}{logical; shall a minimum or a maximum be found.}
  \item{tol}{relative tolerance.}
  \item{...}{additional variables to be passed to the function..}
}
\details{
  fminbnd finds the minimum of a function of one variable within a fixed
  interval. The Matlab version is based on golden section search and
  parabolic interpolation.

  \code{fminbnd} may only give local solutions.
  \code{fminbnd} never evaluates \code{f} at the endpoints.
}
\value{
  List with
  \item{x}{location of the location of minimum resp. maximum.}
  \item{fval}{function value at the optimum.}
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  \code{fminbnd} mimics the Matlab function of the same name and uses the
  R function \code{optimize}.
  Could easily be replaced by \code{pracma::fibsearch}.
}
\seealso{
  \code{\link{fibsearch}}, \code{\link{golden_ratio}}
}
\examples{
fminbnd(cos, 3, 4)          # x = 3.141593  , fval = -1
f <- function(x) x^3-2*x-5
fminbnd(f, 0, 2)            # x = 0.8164966 , fval = -6.088662
}
\keyword{ optimize }
