\name{pracma-package}
\alias{pracma-package}
\alias{pracma}
\docType{package}
\title{
Practical Numerical Math Routines
}
\description{
  This package provides R implementations of more advanced
  math functions from Matlab and Octave (and the Euler Math Toolbox)
  with a special view on optimization and time series routines.

  Some of these realizations are the result of courses on Scientific
  Computing (``Wissenschaftliches Rechnen'') and are mostly intended to
  demonstrate how to implement certain algorithms in R/S.
}
\details{
\tabular{ll}{
Package: \tab pracma\cr
Type: \tab Package\cr
Version: \tab 0.2-2\cr
Date: \tab 2011-03-18\cr
License: \tab GPLv3\cr
LazyLoad: \tab yes\cr
}
  Several implementations of root finding routines are provided as examples.
}
\author{
Hans W Borchers

Maintainer: HwB <hwborchers@googlemail.com>
}
\references{
  Press, Teukolsky, Vetterling, and Flannery (1992). Numerical Recipes in C -
  The Art of Numerical Computing. Second Edition, Cambridge University Press.

  Quarteroni, A., and F. Saleri (2006). Scientific Computing with Matlab
  and Octave. Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\keyword{ package }
\seealso{
  The R package `matlab' contains some of the basic routines from Matlab,
  but unfortunatela not any of the higher math routines.
}
\examples{
\dontrun{
tail(primes(1e7))  # the last 5 prime numbers below 10,000,000 .}
}
