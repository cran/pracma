\name{newtonHorner}
\alias{newtonHorner}
\title{
  Newton's Root Finding Method for Polynomials.
}
\description{
  Finding roots of univariate polynomials.
}
\usage{
newtonHorner(p, x0, maxiter = 50, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{p}{Numeric vector representing a polynomial.}
  \item{x0}{starting value for newtonHorner().}
  \item{maxiter}{maximum number of iterations; default 100.}
  \item{tol}{absolute tolerance; default \code{eps^(1/2)}}
}
\details{
  Similar to \code{newtonRahson}, except that the computation of the
  derivative is done through the Horner scheme in parallel with computing
  the value of the polynomial. This makes the algorithm significantly
  faster. 
}
\value{
  Return a list with components \code{root}, \code{f.root}, 
  the function value at the found root, \code{iter}, the number of iterations
  done, and \code{root}, and the estimated precision \code{estim.prec}

  The estimated precision is given as the difference to the last solution
  before stop.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\seealso{
\code{\link{newtonRaphson}}
}
\examples{
##  Example: x^3 - 6 x^2 + 11 x - 6  with roots 1, 2, 3
p <- c(1, -6, 11, -6)
x0 <- 0
while (length(p) > 1) {
    N <- newtonHorner(p, x0)
    if (!is.null(N$root)) {
        cat("x0 =", N$root, "\n")
        p <- N$deflate
    } else {
        break
    }
}
##  Try: p <- Poly(c(1:20))
}
\keyword{ math }
