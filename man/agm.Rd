\name{agm}
\alias{agm}
\title{
  Arithmetic-geometric Mean
}
\description{
  The arithmetic-geometric mean of positive numbers.
}
\usage{
agm(a, b, maxiter = 25, tol = .Machine$double.eps^(1/2))
}
\arguments{
  \item{a, b}{Positive numbers.}
  \item{maxiter}{Maximum number of iterations.}
  \item{tol}{tolerance; stops when \code{abs(a-b) < tol}.}
}
\details{
  The arithmetic-geometric mean is defined as the common limit of the two
  sequences \eqn{a_{n+1} = (a_n + b_n)/2} and \eqn{b_{n+1} = \sqrt(a_n b_n)}.
}
\value{
  Returnes one value, the mean of the last two values \code{a}, \code{b}.
}
\references{
  Borwein, J. M., and P. B. Borwein (1998). Pi and the AGM: A Study in
  Analytic Number Theory and Computational Complexity. Second, reprinted
  Edition, A Wiley-interscience publication.
}
\note{
  The AGM is also defined for negative values a and/or b, but then the
  complex square root has to be taken.
}
\seealso{
  Arithmetic, geometric, and harmonic mean.
}
\examples{
##  Gauss constant
1 / agm(1, sqrt(2), tol = 1e-15)$agm  # 0.834626841674073

##  Calculate the (elliptic) integral 2/pi \int_0^1 dt / sqrt(1 - t^4)
f <- function(t) 1 / sqrt(1-t^4)
2 / pi * integrate(f, 0, 1)$value
1 / agm(1, sqrt(2))$agm

##  Calculate pi with quadratic convergence (modified AGM)
#   See algorithm 2.1 in Borwein and Borwein
y <- sqrt(sqrt(2))
x <- (y+1/y)/2
p <- 2+sqrt(2)
for (i in 1:6){
  cat(format(p, digits=16), "\n")
  p <- p * (1+x) / (1+y)
  s <- sqrt(x)
  y <- (y*s + 1/s) / (1+y)
  x <- (s+1/s)/2
  }

\dontrun{
##  Calculate pi with arbitrary precision using the Rmpfr package
require("Rmpfr")
vpa <- function(., d = 32) mpfr(., precBits = 4*d)
# Function to compute \pi to d decimal digits accuracy, based on the 
# algebraic-geometric mean, correct digits are doubled in each step.
agm_pi <- function(d) {
    a <- vpa(1, d)
    b <- 1/sqrt(vpa(2, d))
    s <- 1/vpa(4, d)
    p <- 1
    n <- ceiling(log2(d));
    for (k in 1:n) {
        c <- (a+b)/2
        b <- sqrt(a*b)
        s <- s - p * (c-a)^2
        p <- 2 * p
        a <- c
    }
    return(a^2/s)
}
d <- 64
pia <- agm_pi(d)
print(pia, digits = d)
# 3.141592653589793238462643383279502884197169399375105820974944592
# 3.1415926535897932384626433832795028841971693993751058209749445923 exact
}
}
\keyword{ math }
