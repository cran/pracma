\name{brent_dekker}
\alias{brent_dekker}
\title{
  Brent-Dekker Root Finding Method
}
\description{
Finding roots of univariate functions in bounded intervals.
}
\usage{
brent_dekker(f, a, b, ..., maxiter = 100, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{f}{Function or its name as a string.}
  \item{a}{Left end point of an interval.}
  \item{b}{Right end point of an interval.}
  \item{maxiter}{Maximum number of iterations.}
  \item{tol}{Absolute tolerance.}
  \item{...}{Additional arguments to be passed to f.}
}
\details{
  Well known root finding algorithms for real, univariate, continuous
  functions. The Brent-Dekker approach is a clever combination of secant
  and bisection with quadratic interpolation.
}
\value{
  Brent-Dekker (at the moment) that just returns the root found
  or \code{NA} if the maximum number of iterations has been exceeded..
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\author{
  Hans W Borchers  <hwborchers@googlemail.com>
}
\note{
  \code{fzero} in Octave uses a more sophisticated approach with cubic
  interpolation.
}
\seealso{
\code{\link{uniroot}}
}
\examples{
# Legendre polynomial of degree 5
lp5 <- c(63, 0, -70, 0, 15, 0)/8
f <- function(x) polyval(lp5, x)
brent_dekker(f, 0.6, 1)  # 0.9061798459 correct to 10 places
}
\keyword{ math }
