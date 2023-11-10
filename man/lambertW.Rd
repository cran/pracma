\name{lambertWp}
\alias{lambertWp}
\alias{lambertWn}
\title{
  Lambert's W Function
}
\description{
  Principal real branch of the Lambert W function.
}
\usage{
lambertWp(x)
lambertWn(x) 
}
\arguments{
  \item{x}{Numeric vector of real numbers \code{>= -1/e}.}
}
\details{
  The Lambert W function is the inverse of \code{x --> x e^x}, with two
  real branches, W0 for \code{x >= -1/e} and W-1 for \code{-1/e <= x < 0}.
  Here the principal branch is called \code{lambertWp}, tho other one
  \code{lambertWp}, computed for real \code{x}.

  The value is calculated using an iteration that stems from applying
  Halley's method. This iteration is quite fast and accurate.

  The functions is not really vectorized, but at least returns a vector of
  values when presented with a numeric vector of length \code{>= 2}.
}
\value{
  Returns the solution \code{w} of \code{w*exp(w) = x} for real \code{x}
  with \code{NaN} if \code{x < 1/exp(1)} (resp. \code{x >= 0} for the
  second branch).
}
\references{
  Corless, R. M., G. H.Gonnet, D. E. G Hare, D. J. Jeffrey, and D. E. Knuth
  (1996). On the Lambert W Function. Advances in Computational Mathematics,
  Vol. 5, pp. 329-359.
}
\note{
  See the examples how values for the second branch or the complex
  Lambert W function could be calculated by Newton's method.
}
\seealso{
  \code{\link{halley}}
}
\examples{
##  Examples
lambertWp(0)          #=> 0
lambertWp(1)          #=> 0.5671432904097838...  Omega constant
lambertWp(exp(1))     #=> 1
lambertWp(-log(2)/2)  #=> -log(2)

# The solution of  x * a^x = z  is  W(log(a)*z)/log(a)
# x * 123^(x-1) = 3
lambertWp(3*123*log(123))/log(123)  #=> 1.19183018...

x <- seq(-0.35, 0.0, by=0.05)
w <- lambertWn(x)
w * exp(w)            # max. error < 3e-16
# [1] -0.35 -0.30 -0.25 -0.20 -0.15 -0.10 -0.05   NaN

\dontrun{
xs <- c(-1/exp(1), seq(-0.35, 6, by=0.05))
ys <- lambertWp(xs)
plot(xs, ys, type="l", col="darkred", lwd=2, ylim=c(-2,2),
     main="Lambert W0 Function", xlab="", ylab="")
grid()
points(c(-1/exp(1), 0, 1, exp(1)), c(-1, 0, lambertWp(1), 1))
text(1.8, 0.5, "Omega constant")
  }

## Analytic derivative of lambertWp (similar for lambertWn)
D_lambertWp <- function(x) {
    xw <- lambertWp(x)
    1 / (1+xw) / exp(xw)
}
D_lambertWp(c(-1/exp(1), 0, 1, exp(1)))
# [1] Inf 1.0000000 0.3618963 0.1839397

## Second branch resp. the complex function lambertWm()
F <- function(xy, z0) {
    z <- xy[1] + xy[2]*1i
    fz <- z * exp(z) - z0
    return(c(Re(fz), Im(fz)))
}
newtonsys(F, c(-1, -1), z0 = -0.1)   #=> -3.5771520639573
newtonsys(F, c(-1, -1), z0 = -pi/2)  #=> -1.5707963267949i = -pi/2 * 1i
}
\keyword{ specfun }
