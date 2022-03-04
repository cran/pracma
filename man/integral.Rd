\name{integral}
\alias{integral}
\title{
  Adaptive Numerical Integration
}
\description{
  Combines several approaches to adaptive numerical integration of 
  functions of one variable.
}
\usage{
integral(fun, xmin, xmax,
         method = c("Kronrod", "Clenshaw","Simpson"),
         no_intervals = 8, random = FALSE,
         reltol = 1e-8, abstol = 0, ...)
}
\arguments{
  \item{fun}{integrand, univariate (vectorized) function.}
  \item{xmin,xmax}{endpoints of the integration interval.}
  \item{method}{integration procedure, see below.}
  \item{no_intervals}{number of subdivisions at at start.}
  \item{random}{logical; shall the length of subdivisions be random.}
  \item{reltol}{relative tolerance.}
  \item{abstol}{absolute tolerance; not used.}
  \item{...}{additional parameters to be passed to the function.}
}
\details{
  \code{integral} combines the following methods for adaptive 
  numerical integration (also available as separate functions):
  \itemize{
    \item Kronrod (Gauss-Kronrod)
    \item Clenshaw (Clenshaw-Curtis; not yet made adaptive)
    \item Simpson (adaptive Simpson)
  }
  Recommended default method is Gauss-Kronrod. Also try Clenshaw-Curtis
  that may be faster at times.

  Most methods require that function \code{f} is vectorized. This will
  be checked and the function vectorized if necessary.
  
  By default, the integration domain is subdivided into \code{no_intervals}
  subdomains to avoid 0 results if the support of the integrand function is
  small compared to the whole domain. If \code{random} is true, nodes will
  be picked randomly, otherwise forming a regular division.

  If the interval is infinite, \code{quadinf} will be called that 
  accepts the same methods as well. [If the function is array-valued, 
  \code{quadv} is called that applies an adaptive Simpson procedure,
  other methods are ignored -- not true anymore.]
}
\value{
  Returns the integral, no error terms given.
}
\references{
  Davis, Ph. J., and Ph. Rabinowitz (1984). Methods of Numerical 
  Integration. Dover Publications, New York.
}
\note{
  \code{integral} does not provide `new' functionality, everything is 
  already contained in the functions called here. Other interesting
  alternatives are Gauss-Richardson (\code{quadgr}) and Romberg 
  (\code{romberg}) integration.
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
for (m in c("Kron", "Clen", "Simp")) {
    Q <- integral(fun, -1, 1, reltol = 1e-12, method = m)
    cat(m, Q, abs(Q-val), "\n")}
# Kron 1.582233 3.197442e-13 
# Rich 1.582233 3.197442e-13  # use quadgr()
# Clen 1.582233 3.199663e-13 
# Simp 1.582233 3.241851e-13 
# Romb 1.582233 2.555733e-13  # use romberg()

##  Highly oscillating function
fun <- function(x) sin(100*pi*x)/(pi*x)
val <- 0.4989868086930458
for (m in c("Kron", "Clen", "Simp")) {
    Q <- integral(fun, 0, 1, reltol = 1e-12, method = m)
    cat(m, Q, abs(Q-val), "\n")}
# Kron 0.4989868 2.775558e-16 
# Rich 0.4989868 4.440892e-16  # use quadgr()
# Clen 0.4989868 2.231548e-14
# Simp 0.4989868 6.328271e-15 
# Romb 0.4989868 1.508793e-13  # use romberg()

## Evaluate improper integral
fun <- function(x) log(x)^2 * exp(-x^2)
val <- 1.9475221803007815976
Q <- integral(fun, 0, Inf, reltol = 1e-12)
# For infinite domains Gauss integration is applied!
cat(m, Q, abs(Q-val), "\n")
# Kron 1.94752218028062 2.01587635473288e-11 

## Example with small function support
fun <- function(x)
            ifelse (x <= 0 | x >= pi, 0, sin(x))
integral(fun, -100, 100, no_intervals = 1)      # 0
integral(fun, -100, 100, no_intervals = 10)     # 1.99999999723
integral(fun, -100, 100, random=FALSE)          # 2
integral(fun, -100, 100, random=TRUE)           # 2 (sometimes 0 !)
integral(fun, -1000, 10000, random=FALSE)       # 0
integral(fun, -1000, 10000, random=TRUE)        # 0 (sometimes 2 !)
}
\keyword{ math }
