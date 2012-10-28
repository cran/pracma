\name{horner}
\alias{horner}
\alias{hornerdefl}
\title{
Horner's Rule
}
\description{
  Compute the value of a polynomial via Horner's Rule.
}
\usage{
horner(p, x)
hornerdefl(p, x)
}
\arguments{
  \item{p}{Numeric vector representing a polynomial.}
  \item{x}{Numeric scalar, vector or matrix at which to evaluate the polynomial.}
}
\details{
  \code{horner} utilizes the Horner scheme to evaluate the polynomial and its
  first derivative at the same time.

  The polynomial \code{p = p_1*x^n + p_2*x^{n-1} + ... + p_n*x + p_{n+1}}
  is hereby represented by the vector \code{p_1, p_2, ..., p_n, p_{n+1}},
  i.e. from highest to lowest coefficient.

  \code{hornerdefl} uses a similar approach to return the value of \code{p}
  at \code{x} and a polynomial \code{q} that satisfies

  \code{p(t) = q(t) * (t - x) + r, r constant}

  which implies \code{r=0} if \code{x} is a root of \code{p}. This will allow
  for a repeated root finding of polynomials.
}
\value{
  \code{horner} returns a list with two elements, \code{list(y=..., dy=...)}
  where the first list elements returns the values of the polynomial, the
  second the values of its derivative at the point(s) \code{x}.

  \code{hornerdefl} returns a list \code{list(y=..., dy=...)} where \code{q}
  represents a polynomial, see above.
}
\note{
  For fast evaluation, there is no error checking for \code{p} and \code{x},
  which both must be numerical vectors
  (\code{x} can be a matrix in \code{horner}).
}
\references{
  Quarteroni, A., and Saleri, F. (2006) Scientific Computing with Matlab
  and Octave. Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\seealso{
\code{\link{polyval}}
}
\examples{
x <- c(-2, -1, 0, 1, 2)
p <- c(1, 0, 1)  # polynomial x^2 + x, derivative 2*x
horner(p, x)$y   #=>  5  2  1  2  5
horner(p, x)$dy  #=> -4 -2  0  2  4

p <- Poly(c(1, 2, 3))  # roots 1, 2, 3
hornerdefl(p, 3)          # q = x^2- 3 x + 2  with roots 1, 2
}
\keyword{ math }
