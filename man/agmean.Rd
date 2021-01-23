\name{agmean}
\alias{agmean}
\title{
  Arithmetic-geometric Mean
}
\description{
  The arithmetic-geometric mean of real or complex numbers.
}
\usage{
agmean(a, b)
}
\arguments{
  \item{a, b}{vectors of real or complex numbers of the same length (or scalars).}
}
\details{
  The arithmetic-geometric mean is defined as the common limit of the two
  sequences \eqn{a_{n+1} = (a_n + b_n)/2} and \eqn{b_{n+1} = \sqrt(a_n b_n)}.

  When used for negative or complex numbers, the complex square root function
  is applied.
}
\value{
  Returns a list with compoinents: \code{agm} a vector of arithmetic-geometric 
  means, component-wise, \code{niter} the number of iterations, and \code{prec}
  the overall estimated precision.
}
\note{
  Gauss discovered that elliptic integrals can be effectively computed via
  the arithmetic-geometric mean (see example below), for example:
  \deqn{\int_0^{\pi/2} \frac{dt}{\sqrt{1 - m^2 sin^2(t)}}  = \frac{(a+b) \pi}{4 \cdot agm(a,b)}}
  where \eqn{m = (a-b)/(a+b)}
}
\references{
  \url{https://mathworld.wolfram.com/Arithmetic-GeometricMean.html}
}
\seealso{
  Arithmetic, geometric, and harmonic mean.
}
\examples{
##  Accuracy test: Gauss constant
1/agmean(1, sqrt(2))$agm - 0.834626841674073186  # 1.11e-16 < eps = 2.22e-16

## Gauss' AGM-based computation of \pi
a <- 1.0
b <- 1.0/sqrt(2)
s <- 0.5
d <- 1L
while (abs(a-b) > eps()) {
    t <- a
    a <- (a + b)*0.5
    b <- sqrt(t*b)
    c <- (a-t)*(a-t)
    d <- 2L * d
    s <- s - d*c
}
approx_pi <- (a+b)^2 / s / 2.0
abs(approx_pi - pi)             # 8.881784e-16 in 4 iterations

##  Example: Approximate elliptic integral
N <- 20
m <- seq(0, 1, len = N+1)[1:N]
E <- numeric(N)
for (i in 1:N) {
    f <- function(t) 1/sqrt(1 - m[i]^2 * sin(t)^2)
    E[i] <- quad(f, 0, pi/2)
}
A <- numeric(2*N-1)
a <- 1
b <- a * (1-m) / (m+1)

\dontrun{
plot(m, E, main = "Elliptic Integrals vs. arith.-geom. Mean")
lines(m, (a+b)*pi / 4 / agmean(a, b)$agm, col="blue")
grid()}
}
\keyword{ arith }
