\name{bernstein}
\alias{bernstein}
\alias{bernsteinb}
\title{
  Bernstein Polynomials
}
\description{
  Bernstein base polynomials and approximations.
}
\usage{
bernstein(f, n, x)

bernsteinb(k, n, x)
}
\arguments{
  \item{f}{function to be approximated by Bernstein polynomials.}
  \item{k}{integer between 0 and n, the k-th Bernstein polynomial
           of order n.}
  \item{n}{order of the Bernstein polynomial(s).}
  \item{x}{numeric scalar or vector where the Bernstein polynomials
           will be calculated.}
}
\details{
  The Bernstein basis polynomials \eqn{B_{k,n}(x)} are defined as
  \deqn{ B_{k,n}(x) = {{n}\choose{k}} x^k (1-x)^{n-k} }
  and form a basis for the vector space of polynomials of degree 
  \eqn{n} over the interval \eqn{[0,1]}.

  \code{bernstein(f, n, x)} computes the approximation of function
  \code{f} through Bernstein polynomials of degree \code{n}, resp.
  computes the value of this approximation at \code{x}. The function
  is vectorized and applies a brute force calculation.

  But if \code{x} is a scalar, the value will be calculated using
  De Casteljau's algorithm for higher accuracy. For bigger \code{n}
  the binomial coefficients may be in for problems.
}
\value{
  Returns a scalar or vector of function values.
}
\references{
  See https://en.wikipedia.org/wiki/Bernstein_polynomial
}
\examples{
## Example
f <- function(x) sin(2*pi*x)
xs <- linspace(0, 1)
ys <- f(xs)
\dontrun{
plot(xs, ys, type='l', col="blue",
     main="Bernstein Polynomials")
grid()
b10  <- bernstein(f,  10, xs)
b100 <- bernstein(f, 100, xs)
lines(xs, b10,  col="magenta")
lines(xs, b100, col="red") }

# Bernstein basis polynomials
\dontrun{
xs <- linspace(0, 1)
plot(c(0,1), c(0,1), type='n',
     main="Bernstein Basis Polynomials")
grid()
n = 10
for (i in 0:n) {
    bs <- bernsteinb(i, n, xs)
    lines(xs, bs, col=i+1)
} }
}
\keyword{ math }
