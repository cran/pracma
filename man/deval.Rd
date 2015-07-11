\name{deval}
\alias{deval}
\title{
  Evaluate ODE Solution
}
\description{
  Evaluate solution of a differential equation solver.
}
\usage{
deval(x, y, xp, idx = NULL)
}
\arguments{
  \item{x}{vector of (time) points at which the differential equation
           has been solved.}
  \item{y}{values of the function(s) that have been computed for the
           given (time) points.}
  \item{xp}{point or numeric vector at which the solution is wanted;
            must be sorted.}
  \item{idx}{index of functions whose vales shall be returned.}
}
\details{
  Determines where the points \code{xp} lie within the vector \code{x}
  and interpolates linearly.
}
\value{
  An \code{length(xp)}-by-\code{length(idx)} matrix of values at points
  \code{xp}.
}
\note{
  The interpolation is linear only for the moment.
}
\seealso{
  \code{\link{deeve}}
}
\examples{
##  Free fall:  v' = -g - cw abs(v)^1.1,  cw = 1.6 drag coefficien
f <- function(t, y) -9.81 + 1.6*abs(y)^1.1
sol <- rk4(f, 0, 10, 0, 100)
# speed after 0.5, 1, 1.5, 2 seconds
cbind(c(0.5,1,1.5,2), -deval(sol$x, sol$y, c(0.5, 1, 1.5, 2)))
#  0.5  3.272267  m/s
#  1.0  4.507677
#  1.5  4.953259
#  2.0  5.112068
# plot(sol$x, -sol$y, type="l", col="blue"); grid()
}
\keyword{ ode }
