\name{expint}
\alias{expint}
\alias{expint_E1}
\alias{expint_Ei}
\alias{li}
\title{
  Exponential and Logarithmic Integral
}
\description{
  The exponential integral functions E1 and Ei and
  the logarithmic integral Li.

  The exponential integral is defined for \eqn{x > 0} as
  \deqn{\int_x^\infty \frac{e^{-t}}{t} dt}
  and by analytic continuation in the complex plane. It can also be defined
  as the Cauchy principal value of the integral
  \deqn{\int_{-\infty}^x \frac{e^t}{t} dt}
  This is denoted as \eqn{Ei(x)} and the relationship between \code{Ei} and
  \code{expint(x)} for x real, x > 0 is as follows:
  \deqn{Ei(x) = - E1(-x) -i \pi}

  The logarithmic integral \eqn{li(x)} for real \eqn{x, x > 0}, is defined as
  \deqn{li(x) = \int_0^x \frac{dt}{log(t)}}
  and the Eulerian logarithmic integral as \eqn{Li(x) = li(x) - li(2)}.

  The integral \eqn{Li} approximates the prime number function \eqn{\pi(n)},
  i.e., the number of primes below or equal to n (see the examples).
}
\usage{
expint(x)
expint_E1(x)

expint_Ei(x)
li(x)
}
\arguments{
  \item{x}{vector of real or complex numbers.}
}
\details{
  For \code{x} in \code{[-38, 2]} we use a series expansion,
  otherwise a continued fraction, see the references below, chapter 5.
}
\value{
  Returns a vector of real or complex numbers, the vectorized exponential
  integral, resp. the logarithmic integral.
}
\note{
  The logarithmic integral \code{li(10^i)-li(2)} is an approximation of the
  number of primes below \code{10^i}, i.e., \code{Pi(10^i)}, see ``?primes''.
}
\references{
  Abramowitz, M., and I.A. Stegun (1965). Handbook of Mathematical Functions.
  Dover Publications, New York.
}
\seealso{
  \code{gsl::expint_E1,expint_Ei}, \code{\link{primes}}
}
\examples{
expint_E1(1:10)
#   0.2193839  0.0489005  0.0130484  0.0037794  0.0011483
#   0.0003601  0.0001155  0.0000377  0.0000124  0.0000042
expint_Ei(1:10)

\dontrun{
estimPi <- function(n) round(Re(li(n) - li(2))) # estimated number of primes
primesPi <- function(n) length(primes(n))       # true number of primes <= n
N <- 1e6
(estimPi(N) - primesPi(N)) / estimPi(N)         # deviation is 0.16 percent!}
}
\keyword{ specfun }
