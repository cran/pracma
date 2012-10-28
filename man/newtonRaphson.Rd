\name{newtonRaphson}
\alias{newtonRaphson}
\alias{secant}
\title{
  Rootfinding through Newton-Raphson or Secant.
}
\description{
  Finding roots of univariate functions.
}
\usage{
newtonRaphson(fun, x0, dfun = NULL, ...,
       maxiter = 100, tol = .Machine$double.eps^0.5)

secant(fun, a, b, ..., maxiter = 100, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{fun}{Function or its name as a string.}
  \item{x0}{starting value for newtonRaphson().}
  \item{dfun}{A function to compute the derivative of \code{f}.
              If \code{NULL}, a numeric derivative will be computed.}
  \item{a}{For \code{secant} one of the two starting values.}
  \item{b}{Another starting value.}
  \item{maxiter}{maximum number of iterations; default 100.}
  \item{tol}{absolute tolerance; default \code{eps^(1/2)}}
  \item{...}{Additional arguments to be passed to f.}
}
\details{
  Well known root finding algorithms for real, univariate, continuous
  functions. 
}
\value{
  Return a list with components \code{root}, \code{f.root}, 
  the function value at the found root, \code{iter}, the number of iterations
  done, and \code{root}, and the estimated precision \code{estim.prec}

  For both methods the estimated precision is given as the difference to
  the last solution before stop; this may be misleading.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\seealso{
\code{\link{newtonHorner}}
}
\examples{
# Legendre polynomial of degree 5
lp5 <- c(63, 0, -70, 0, 15, 0)/8
f <- function(x) polyval(lp5, x)
newtonRaphson(f, 1.0)  # 0.9061798459 correct to 10 decimals in 5 iterations
secant(f, 0.9, 1)      # 0.9061798459 correct to 10 decimals in 5 iterations
}
\keyword{ math }
