\name{contfrac}
\alias{contfrac}
\title{
  Continous Fractions
}
\description{
  Evaluate a continuous fraction or generate one.
}
\usage{
contfrac(x, tol = 1e-06)
}
\arguments{
  \item{x}{a numeric scalar or vector.}
  \item{tol}{tolerance; default \code{1e-6} to make a nicer appearance for
             \code{pi}.}
}
\details{
  If \code{x} is a scalar its continuous fraction will be generated up to
  the accuracy prescribed in \code{tol}. If it is of length greater 1, the
  function assumes this is a continuous fraction and computes its value.

  For implementation \code{contfrac} uses the representation of continuous
  fractions through 2-by-2 matrices, i.e. the recursion formula.
}
\value{
  Either a numeric value, or a list with components \code{cf}, numeric vector
  representing the continuous fraction \eqn{[b_0; b_1, \ldots, b_{n-1}]};
  \code{rat}, the rational number as a vector with (numerator, denumerator);
  and \code{prec}, the difference between \code{x} and the value of the
  contimuous fraction.
}
\references{
  Hardy, G. H., and E. M. Wright (1979). An Introduction to the Theory of
  Numbers. Fifth Edition, Oxford University Press, New York.
}
\author{
  HwB  <hwborchers@googlemail.com>
}
\note{
  This function is \emph{not} vectorized.
}
\seealso{
\code{\link{rat}}, \code{\link{rats}}
}
\examples{
contfrac(pi)
contfrac(c(3, 7, 15, 1))
}
\keyword{ math }
