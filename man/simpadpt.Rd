\name{simpadpt}
\alias{simpadpt}
\title{
  Adaptive Simpson Quadrature
}
\description{
  Numerically evaluate an integral using adaptive Simpson's rule.
}
\usage{
simpadpt(f, a, b, tol = 1e-6, ...)
}
\arguments{
  \item{f}{univariate function, the integrand.}
  \item{a, b}{lower limits of integration; must be finite.}
  \item{tol}{relative tolerance}
  \item{\ldots}{additional arguments to be passed to \code{f}.}
}
\details{
   Approximates the integral of the function \code{f} from a to b to within
   an error of \code{tol} using recursive adaptive Simpson quadrature.
}
\value{
  A numerical value or vector, the computed integral.
}
\note{
  Based on code from the book by Quarteroni et al.,
  with some tricks borrowed from Matlab and Octave.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\seealso{
  \code{\link{quad}}, \code{\link{simpson2d}}
}
\examples{
myf <- function(x, n) 1/(x+n)  # 0.0953101798043249 , log((n+1)/n) for n=10
simpadpt(myf, 0, 1, n = 10)    # 0.095310179804535

##  Dilogarithm function
flog  <- function(t) log(1-t) / t  # singularity at t=1, almost at t=0
dilog <- function(x) simpadpt(flog, x, 0, tol = 1e-12)
dilog(1)  # 1.64493406685615
          # 1.64493406684823 = pi^2/6

\dontrun{
N <- 51
xs <- seq(-5, 1, length.out = N)
ys <- numeric(N)
for (i in 1:N) ys[i] <- dilog(xs[i])
plot(xs, ys, type = "l", col = "blue",
             main = "Dilogarithm function")
grid()}
}
\keyword{ math }
