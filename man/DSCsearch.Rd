\name{DSCsearch}
\alias{DSCsearch}
\title{
  Davies-Swann-Campey Search
}
\description{
  Implements the Davies-Swann-Campey search in one dimension.
}
\usage{
DSCsearch(f, p, inc = 0.05, scl = 0.1, tol = 1e-6)
}
\arguments{
  \item{f}{function to be minimized.}
  \item{p}{start value.}
  \item{inc}{initial increment.}
  \item{scl}{scaling constant.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
}
\details{
  This is a refinement of Rosenbrock's algorithm. It first takes steps of 
  increasing multiples of some fixed value along a direction until a bracket
  for the one-dimensional minimizer is found.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{niter}{number of iterations performed.}
    \item{feval}{number of function evaluations.}
}
\references{
  A. Antoniou and W.-S. Lu (2007). Practical Optimization. Algorithms and
  Engineering Applications. Springer, New York.

  R. M. Lewis, V. Torczon, and M. W. Trosset (2000). Direct Search Methods:
  Then and Now. Langley Research Center, NASA/CR-2000-210125.
}
\author{
  HwB  <hwborchers@googlemail.com>
}
\note{
  This is a multivariate minimization and search procedure which is here
  reduced to the one-dimensional case.

  Used some formulas and Matlab code as described in the book ``Practical
  Optimization'', see above.
}
\seealso{
  \code{\link{optimize}}
}
\examples{
DSCsearch(sin, 0.8*pi, 0.05, 0.1, 1e-6)
}
\keyword{ optimize }
