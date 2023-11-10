\name{trigPoly}
\alias{trigPoly}
\title{
  Trigonometric Polynomial
}
\description{
  Computes the trigonometric coefficients.
}
\usage{
trigPoly(x, m)
}
\arguments{
  \item{x}{data from \code{t=0} to \code{t=2*(n-1)*pi/n}.}
  \item{m}{degree of trigonometric regression.}
}
\details{
  Compute the coefficients of the trigonometric series of degree \code{m},
  \deqn{a_0 + \sum_k(a_k \cos(k t) + b_k \sin(k t))}
  by applying orthogonality relations.
}
\value{
  Coefficients as a list with components \code{a0}, \code{a}, and \code{b}. 
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\note{
  For irregular spaced data or data not covering the whole period, use
  standard regression techniques, see examples.
}
\seealso{
\code{\link{trigApprox}}
}
\examples{
# Data available only from 0 to pi/2
t <- seq(0, pi, len=7)
x <- 0.5 + 0.25*sin(t) + 1/3*cos(t) - 1/3*sin(2*t) - 0.25*cos(2*t)

# use standard regression techniques
A <- cbind(1, cos(t), sin(t), cos(2*t), sin(2*t))
ab <- qr.solve(A, x)
ab
# [1]  0.5000000  0.3333333  0.2500000 -0.2500000 -0.3333333
ts <- seq(0, 2*pi, length.out = 100)
xs <- ab[1] + ab[2]*cos(ts) +
      ab[3]*sin(ts) + ab[4]*cos(2*ts) +ab[5]*sin(2*ts)

\dontrun{
# plot to make sure
plot(t, x, col = "red", xlim=c(0, 2*pi), ylim=c(-2,2),
           main = "Trigonometric Regression")
lines(ts, xs, col="blue")
grid()}
}
\keyword{ math }
