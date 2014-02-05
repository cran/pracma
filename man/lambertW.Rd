\name{lambertWp}
\alias{lambertWp}
\alias{D_lambertWp}
\title{
  Lambert's W Function
}
\description{
  Principal branch of the Lambert W function
}
\usage{
lambertWp(z)

D_lambertWp(z) 
}
\arguments{
  \item{z}{Numeric vector of real numbers \code{>= -1/e}.}
}
\details{
  The Lambert W function is the inverse of \code{x --> x e^x}, which
  is unique for \code{x >= -1/e}. Here only the principal branch is
  computed for real \code{z}.

  The value is calculated using an iteration that stems from applying
  Newton's method. This iteration is quite fast.

  \code{D_lambertWp} returns the exact derivative of \code{lambertWp},
  based on the fact that \code{lambertWp} is the inverse of \code{x*exp(x)}.

  The functions is not really vectorized, but at least return a vector of
  values when presented with a numeric vector of length \code{>= 2}.
}
\value{
  Returns the solution \code{w} of \code{w*exp(w) = z} for real \code{z}
  with \code{NA} if \code{z < 1/exp(1)}.
}
\references{
  Corless, R. M., G. H.Gonnet, D. E. G Hare, D. J. Jeffrey, and D. E. Knuth
  (1996). On the Lambert W Function. Advances in Computational Mathematics,
  Vol. 5, pp. 329-359.
  \url{http://www.apmaths.uwo.ca/~djeffrey/Offprints/W-adv-cm.pdf}.
}
\note{
  See the examples how values for the second branch or the complex
  Lambert W function could be calculated by Newton's method.
}
\seealso{
  \code{\link{newtonRaphson}}
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

\dontrun{
xs <- c(-1/exp(1), seq(-0.35, 6, by=0.05))
ys <- lambertWp(xs)
plot(xs, ys, type="l", col="darkred", lwd=2, ylim=c(-2,2),
     main="Lambert W0 Function", xlab="", ylab="")
grid()
points(c(-1/exp(1), 0, 1, exp(1)), c(-1, 0, lambertWp(1), 1))
text(1.8, 0.5, "Omega constant")}

# Second branch resp. the complex function lambertWm()
F <- function(xy, z0) {
    z <- xy[1] + xy[2]*1i
    fz <- z * exp(z) - z0
    return(c(Re(fz), Im(fz)))
}
newtonsys(F, c(-1, -1), z0 = -0.1)   #=> -3.5771520639573
newtonsys(F, c(-1, -1), z0 = -pi/2)  #=> -1.5707963267949i = -pi/2 * 1i
}
\keyword{ math }
