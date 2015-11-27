\name{cotes}
\alias{cotes}
\title{
  Newton-Cotes Formulas
}
\description{
  Closed composite Newton-Cotes formulas of degree 2 to 8.
}
\usage{
cotes(f, a, b, n, nodes, ...)
}
\arguments{
  \item{f}{the integrand as function of two variables.}
  \item{a, b}{lower and upper limit of the integral.}
  \item{n}{number of subintervals (grid points).}
  \item{nodes}{number of nodes in the Newton-Cotes formula.}
  \item{\ldots}{additional parameters to be passed to the function.}
}
\details{
  2 to 8 point closed and summed Newton-Cotes numerical integration formulas.

  These formulas are called `closed' as they include the endpoints.
  They are called `composite' insofar as they are combined with a
  Lagrange interpolation over subintervals.
}
\value{
  The integral as a scalar.
}
\note{
  It is generally recommended not to apply Newton-Cotes formula of degrees
  higher than 6, instead increase the number \code{n} of subintervals used.
}
\author{
  Standard Newton-Cotes formulas can be found in every textbook.
  Copyright (c) 2005 Greg von Winckel of nicely vectorized Matlab code,
  available from MatlabCentral, for 2 to 11 grid points.
  R version by Hans W Borchers, with permission.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\seealso{
  \code{\link{simpadpt}}, \code{\link{trapz}}
}
\examples{
cotes(sin, 0, pi/2, 20, 2)      # 0.999485905248533
cotes(sin, 0, pi/2, 20, 3)      # 1.000000211546591
cotes(sin, 0, pi/2, 20, 4)      # 1.000000391824184
cotes(sin, 0, pi/2, 20, 5)      # 0.999999999501637
cotes(sin, 0, pi/2, 20, 6)      # 0.999999998927507
cotes(sin, 0, pi/2, 20, 7)      # 1.000000000000363  odd degree is better
cotes(sin, 0, pi/2, 20, 8)      # 1.000000000002231
}
\keyword{ math }
