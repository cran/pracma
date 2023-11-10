\name{Si, Ci}
\alias{Si}
\alias{Ci}
\title{
  Sine and Cosine Integral Functions
}
\description{
  Computes the sine and cosine integrals through approximations.
}
\usage{
Si(x)
Ci(x)
}
\arguments{
  \item{x}{Scalar or vector of real numbers.}
}
\details{
  The sine and cosine integrals are defined as
  \deqn{Si(x) = \int_0^x \frac{\sin(t)}{t} dt}
  \deqn{Ci(x) = - \int_x^\infty \frac{\cos(t)}{t} dt = \gamma + \log(x) + \int_0^x \frac{\cos(t)-1}{t} dt}
  where \eqn{\gamma} is the Euler-Mascheroni constant.
}
\value{
  Returns a scalar of sine resp. cosine integrals applied to each
  element of the scalar/vector. The value \code{Ci(x)} is not correct, 
  it should be \code{Ci(x)+pi*i}, only the real part is returned!
  
  The function is not truely vectorized, for vectors the values are
  calculated in a for-loop. The accuracy is about \code{10^-13} and better
  in a reasonable range of input values.
}
\references{
  Zhang, S., and J. Jin (1996). Computation of Special Functions. Wiley-Interscience.
}
\seealso{
  \code{\link{sinc}}, \code{\link{expint}}
}

\examples{
x <- c(-3:3) * pi
Si(x); Ci(x)

\dontrun{
xs <- linspace(0, 10*pi, 200)
ysi <- Si(xs); yci <- Ci(xs)
plot(c(0, 35), c(-1.5, 2.0), type = 'n', xlab = '', ylab = '',
     main = "Sine and cosine integral functions")
lines(xs, ysi, col = "darkred",  lwd = 2)
lines(xs, yci, col = "darkblue", lwd = 2)
lines(c(0, 10*pi), c(pi/2, pi/2), col = "gray")
lines(xs, cos(xs), col = "gray")
grid()}
}

\keyword{ specfun }

