\name{chebPoly}
\alias{chebPoly}
\title{Chebyshev Polynomials}
\description{
  Chebyshev polynomials and their values.
}
\usage{
chebPoly(n, x = NULL)
}
\arguments{
  \item{n}{an integer \code{>= 0}.}
  \item{x}{a numeric vector, possibly empty; default \code{NULL}.}
}
\details{
  Determines an (n+1)-ny-(n+1)-Matrix of Chebyshev polynomials up to degree n.

  The coefficients of the first \code{n} Chebyshev polynomials are computed
  using the recursion formula. For computing any values at points the well
  known Horner schema is applied.
}
\value{
  If \code{x} is \code{NULL}, returns an \code{(n+1)}-by-\code{(n+1)} matrix
  with the coefficients of the first Chebyshev polynomials from \code{0} to
  \code{n}, one polynomial per row with coefficients from highest to lowest
  order.

  If \code{x} is a numeric vector, returns the values of the \code{n}-th
  Chebyshev polynomial at the points of \code{x}.
}
\references{
  Carothers, N. L. (1998). A Short Course on Approximation Theory.
  Bowling Green State University.
}
\note{
  See the ``Chebfun Project'' <https://www.chebfun.org/> by
  Nick Trefethen.
}
\seealso{
\code{\link{chebCoeff}}, \code{\link{chebApprox}}
}
\examples{
chebPoly(6)

\dontrun{
##  Plot 6 Chebyshev Polynomials
plot(0, 0, type="n", xlim=c(-1, 1), ylim=c(-1.2, 1.2),
    main="Chebyshev Polynomials for n=1..6", xlab="x", ylab="y")
grid()
x <- seq(-1, 1, length.out = 101)
for (i in 1:6) {
    y <- chebPoly(i, x)
    lines(x, y, col=i)
}
legend(x = 0.55, y = 1.2, c("n=1", "n=2", "n=3", "n=4", "n=5", "n=6"),
    col = 1:6, lty = 1, bg="whitesmoke", cex = 0.75)
}
}
\keyword{ math }
