\name{integral}
\alias{integral}
\title{
  Adaptive Numerical Integration
}
\description{
  Combines several approaches to adaptive numerical integration of functions
  of one variable.
}
\usage{
integral(fun, xmin, xmax,
            method = c("Kronrod","Richardson","Clenshaw","Simpson","Romberg"),
            vectorized = TRUE, arrayValued = FALSE,
            reltol = 1e-8, abstol = 0, ...)
}
\arguments{
  \item{fun}{integrand, univariate (vectorized) function.}
  \item{xmin,xmax}{endpoints of the integration interval.}
  \item{method}{integration procedure, see below.}
  \item{vectorized}{logical; is the integrand a vectorized function; not used.}
  \item{arrayValued}{logical; is the integrand array-valued.}
  \item{reltol}{relative tolerance.}
  \item{abstol}{absolute tolerance; not used.}
  \item{...}{additional parameters to be passed to the function.}
}
\details{
  \code{integral} combines the following methods for adaptive numerical
  integration (also available as separate functions):
  \itemize{
    \item Kronrod (Gauss-Kronrod)
    \item Richardson (Gauss-Richardson)
    \item Clenshaw (Clenshaw-Curtis; not yet made adaptive)
    \item Simpson (adaptive Simpson)
    \item Romberg
  }
  Recommended default method is Gauss-Kronrod.
  Most methods require that function \code{f} is vectorized.

  If the interval is infinite, \code{quadinf} will be called that accepts
  the same methods as well. If the function is array-valued, \code{quadv}
  is called that applies an adaptive Simpson procedure -- other methods are
  ignored.
}
\value{
  Returns the integral, no error terms given.
}
\references{
  Davis, Ph. J., and Ph. Rabinowitz (1984). Methods of Numerical Integration.
  Dover Publications, New York.
}
\note{
  \code{integral} does not provide `new' functionality, everything is already
  contained in the functions called here.
}
\seealso{
  \code{\link{quadgk}}, \code{\link{quadgr}}, \code{quadcc},
  \code{\link{simpadpt}}, \code{\link{romberg}},
  \code{\link{quadv}}, \code{\link{quadinf}}
}
\examples{
##  Very smooth function
fun <- function(x) 1/(x^4+x^2+0.9)
val <- 1.582232963729353
for (m in c("Kron", "Rich", "Clen", "Simp", "Romb")) {
    Q <- integral(fun, -1, 1, reltol = 1e-12, method = m)
    cat(m, Q, abs(Q-val), "\n")}
# Kron 1.582233 3.197442e-13 
# Rich 1.582233 3.197442e-13 
# Clen 1.582233 3.199663e-13 
# Simp 1.582233 3.241851e-13 
# Romb 1.582233 2.555733e-13 

##  Highly oscillating function
fun <- function(x) sin(100*pi*x)/(pi*x)
val <- 0.4989868086930458
for (m in c("Kron", "Rich", "Clen", "Simp", "Romb")) {
    Q <- integral(fun, 0, 1, reltol = 1e-12, method = m)
    cat(m, Q, abs(Q-val), "\n")}
# Kron 0.4989868 2.775558e-16 
# Rich 0.4989868 4.440892e-16 
# Clen 0.4989868 2.231548e-14
# Simp 0.4989868 6.328271e-15 
# Romb 0.4989868 1.508793e-13

## Evaluate improper integral
fun <- function(x) log(x)^2 * exp(-x^2)
val <- 1.9475221803007815976
for (m in c("Kron", "Rich", "Clen", "Simp", "Romb")) {
    Q <- integral(fun, 0, Inf, reltol = 1e-12, method = m)
    cat(m, Q, abs(Q-val), "\n")}
# Kron 1.947522 1.101341e-13 
# Rich 1.947522 2.928655e-10 
# Clen 1.948016 1.960654e-13 
# Simp 1.947522 9.416912e-12 
# Romb 1.952683 0.00516102

## Array-valued function
log1 <- function(x) log(1+x)
fun <- function(x) c(exp(x), log1(x))
Q <- integral(fun, 0, 1, reltol = 1e-12, arrayValued = TRUE, method = "Simpson")
abs(Q - c(exp(1)-1, log(4)-1))          # 2.220446e-16 4.607426e-15
}
\keyword{ math }
