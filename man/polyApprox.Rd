\name{polyApprox}
\alias{polyApprox}
\title{
  Polynomial Approximation
}
\description{
  Generate a polynomial approximation.
}
\usage{
polyApprox(f, a, b, n, ...)
}
\arguments{
  \item{f}{function to be approximated.}
  \item{a, b}{end points of the interval.}
  \item{n}{degree of the polynomial.}
  \item{...}{further variables for function \code{f}.}
}
\details{
  Uses the Chebyshev coefficients to derive polynomial coefficients.
}
\value{
  List with four components:
  \item{p}{the approximating polynomial.}
  \item{f}{a function evaluating this polynomial.}
  \item{cheb.coeff}{the Chebyshev coefficients.} 
  \item{estim.prec}{the estimated precision over the given interval.}
}
\references{
  Carothers, N. L. (1998). A Short Course on Approximation Theory.
  Bowling Green State University.
}
\note{
  The Chebyshev approximation is optimal in the sense of the \eqn{L^1} norm,
  but not as a solution of the \emph{minimax} problem; for this, an
  application of the Remez algorithm is needed.
}
\seealso{
\code{\link{chebApprox}}, \code{\link{polyfit}}
}
\examples{
##  Example
#   Polynomial approximation for sin
polyApprox(sin, -pi, pi, 9)
# $p
#  [1]  2.197296e-06  0.000000e+00 -1.937495e-04  0.000000e+00  8.317144e-03
#  [6]  0.000000e+00 -1.666468e-01  0.000000e+00  9.999961e-01  0.000000e+00
#
# $f
# function (x) 
# polyval(p, x)
#
# $cheb.coeff
#  [1]  0.06549943  0.00000000 -0.58518036  0.00000000  2.54520983  0.00000000
#  [7] -5.16709776  0.00000000  3.14158037  0.00000000
#
# $estim.prec
# [1] 1.151207e-05

\dontrun{
f <- polyApprox(sin, -pi, pi, 9)$f
x <- seq(-pi, pi, length.out = 100)
y <- sin(x) - f(x)
plot(x, y, type = "l", col = "blue")
grid()}
}
\keyword{ math }
