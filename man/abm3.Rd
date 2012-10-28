\name{abm3pc}
\alias{abm3pc}
\title{
  Adams-Bashford-Moulton
}
\description{
  Third-order Adams-Bashford-Moulton predictor-corrector method.
}
\usage{
abm3pc(f, a, b, y0, n = 50, ...)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)}.}
  \item{a, b}{endpoints of the interval.}
  \item{y0}{starting values at point \code{a}.}
  \item{n}{the number of steps from \code{a} to \code{b}.}
  \item{\ldots}{additional parameters to be passed to the function.}
}
\details{
  Combined Adams-Bashford and Adams-Moulton (or: multi-step) method of
  third order with corrections according to the predictor-corrector approach.
}
\value{
  List with components \code{x} for grid points between \code{a} and \code{b}
  and \code{y} a vector \code{y} the same length as \code{x}; additionally
  an error estimation \code{est.error} that should be looked at with caution.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\note{
  This function serves demonstration purposes only.
}
\seealso{
  \code{\link{rk4}}, \code{\link{ode23}}
}
\examples{
##  Attempt on a non-stiff equation
#   y' = y^2 - y^3, y(0) = d, 0 <= t <= 2/d, d = 0.01
f <- function(t, y) y^2 - y^3
d <- 1/250
abm1 <- abm3pc(f, 0, 2/d, d, n = 1/d)
abm2 <- abm3pc(f, 0, 2/d, d, n = 2/d)
\dontrun{
plot(abm1$x, abm1$y, type = "l", col = "blue")
lines(abm2$x, abm2$y, type = "l", col = "red")
grid()}
}
\keyword{ ode }
