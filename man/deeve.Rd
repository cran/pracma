\name{deeve}
\alias{deeve}
\title{
  Event Detection in ODE solution
}
\description{
  Detect events in solutions of a differential equation.
}
\usage{
deeve(x, y, yv = 0, idx = NULL)
}
\arguments{
  \item{x}{vector of (time) points at which the differential equation
           has been solved.}
  \item{y}{values of the function(s) that have been computed for the
           given (time) points.}
  \item{yv}{point or numeric vector at which the solution is wanted.}
  \item{idx}{index of functions whose vales shall be returned.}
}
\details{
  Determines when (in \code{x} coordinates) the \code{idx}-th solution
  function will take on the value \code{yv}.

  The interpolation is linear for the moment. For points outside the
  \code{x} interval \code{NA} is returned.
}
\value{
  A (time) point \code{x0} at which the event happens.
}
\note{
  The interpolation is linear only for the moment.
}
\seealso{
  \code{\link{deval}}
}
\examples{
##  Damped pendulum:  y'' = -0.3 y' - sin(y)
#   y1 = y, y2 = y':  y1' = y2,  y2' = -0.3*y2 - sin(y1)
f <- function(t, y) {
	dy1 <- y[2]
	dy2 <- -0.3*y[2] - sin(y[1])
	return(c(dy1, dy2))
}
sol <- rk4sys(f, 0, 10, c(pi/2, 0), 100)
deeve(sol$x, sol$y[,1])                   # y1 = 0 : elongation in [sec]
# [1] 2.073507 5.414753 8.650250
# matplot(sol$x, sol$y); grid()
}
\keyword{ ode }
