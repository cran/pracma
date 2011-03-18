\name{horner}
\alias{horner}
\title{
Horner's Method
}
\description{
  Compute the value of a polynomial via the Horner scheme.
}
\usage{
horner(p, x)
}
\arguments{
  \item{p}{Numeric vector representing a polynomial.}
  \item{x}{Numeric scalar or vector at which to evaluate the polynomial.}
}
\details{
  Utilizes the Horner scheme to evaluate the polynomial and its first
  derivative at the same time.

  The polynomial \code{p = p_1*x^n + p_2*x^{n-1} + ... + p_n*x + p_{n+1}}
  is hereby represented by the vector \code{p_1, p_2, ..., p_n, p_{n+1}},
  i.e. from highest to lowest coefficient.
}
\value{
  Returns a list with two elements, \code{list(y=..., dy=...)} where the
  first list elements returns the values of the polynomial, the second the
  values of its derivative at the point(s) \code{x}.
}
\references{
  Quarteroni, A., and Saleri, F. (2006) Scientific Computing with Matlab
  and Octave. Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\author{
  HwB  <hwborchers@googlemail.com>
}
\seealso{
\code{\link{polyval}}
}
\examples{
x <- c(-2, -1, 0, 1, 2)
p <- c(1, 0, 1)  # polynomial x^2 + x, derivative 2*x
horner(p, x)$y   #=>  5  2  1  2  5
horner(p, x)$dy  #=> -4 -2  0  2  4
}
\keyword{ math }
