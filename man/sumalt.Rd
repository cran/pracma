\name{sumalt}
\alias{sumalt}
\title{
  Alternating Series Acceleration
}
\description{
  Computes the value of an (infinite) alternating sum applying an
  acceleration method found by Cohen et al.
}
\usage{
sumalt(f_alt, n)
}
\arguments{
  \item{f_alt}{a funktion of \code{k=0..Inf} that returns element
               \code{a_k} of the infinite alternating series.}
  \item{n}{number of elements of the series used for calculating.}
}
\details{
  Computes the sum of an alternating series (whose entries are strictly
  decreasing), applying the acceleration method developped by H. Cohen,
  F. Rodriguez Villegas, and Don Zagier.

  For example, to compute the Leibniz series (see below) to 15 digits 
  exactly, \code{10^15} summands of the series will be needed. This
  accelleration approach here will only need about 20 of them!
}
\value{
  Returns an approximation of the series value.
}
\references{
  Henri Cohen, F. Rodriguez Villegas, and Don Zagier.
  Convergence Acceleration of Alternating Series.
  Experimental Mathematics, Vol. 9 (2000).
}
\author{
  Implemented by Hans W Borchers.
}
\seealso{
  \code{\link{aitken}}
}
\examples{
# Beispiel: Leibniz-Reihe 1 - 1/3 + 1/5 - 1/7 +- ...
a_pi4 <- function(k) (-1)^k / (2*k + 1)
sumalt(a_pi4, 20)  # 0.7853981633974484 = pi/4 + eps()

# Beispiel: Van Wijngaarden transform needs 60 terms
n <- 60; N <- 0:n
a <- cumsum((-1)^N / (2*N+1))
for (i in 1:n) {
    a <- (a[1:(n-i+1)] + a[2:(n-i+2)]) / 2
}
a - pi/4  # 0.7853981633974483

# Beispiel: 1 - 1/2^2 + 1/3^2 - 1/4^2 +- ...
b_alt <- function(k) (-1)^k / (k+1)^2
sumalt(b_alt, 20)  # 0.8224670334241133 = pi^2/12 + eps()

\dontrun{
# Dirichlet eta() function: eta(s) = 1/1^s - 1/2^s + 1/3^s -+ ...
  eta_ <- function(s) {
    eta_alt <- function(k) (-1)^k / (k+1)^s
    sumalt(eta_alt, 30)
  }
  eta_(1)                       # 0.6931471805599453 = log(2)
  abs(eta_(1+1i) - eta(1+1i))   # 1.24e-16
}
}
\keyword{ math }
