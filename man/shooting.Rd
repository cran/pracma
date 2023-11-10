\name{shooting}
\alias{shooting}
\title{Shooting Method}
\description{
  The shooting method solves the boundary value problem for 
  second-order differential equations.
}
\usage{
shooting(f, t0, tfinal, y0, h, a, b,
         itermax = 20, tol = 1e-6, hmax = 0)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y'' = f(x, y, y')}.}
  \item{t0, tfinal}{start and end points of the interval.}
  \item{y0}{starting value of the solution.}
  \item{h}{function defining the boundary condition as a function at the
           end point of the interval.}
  \item{a, b}{two guesses of the derivative at the start point.}
  \item{itermax}{maximum number of iterations for the secant method.}
  \item{tol}{tolerance to be used for stopping and in the \code{ode45}
             solver.}
  \item{hmax}{maximal step size, to be passed to the solver.}
}
\details{
  A second-order differential equation is solved with boundary conditions
  \code{y(t0) = y0} at the start point of the interval, and
  \code{h(y(tfinal), dy/dt(tfinal)) = 0} at the end. The zero of
  \code{h} is found by a simple secant approach.
}
\value{
  Returns a list with two components, \code{t} for grid (or `time')
  points between \code{t0} and \code{tfinal}, and \code{y} the solution
  of the differential equation evaluated at these points.
}
\references{
  L. V. Fausett (2008). Applied Numerical Analysis Using MATLAB.
  Second Edition, Pearson Education Inc.
}
\note{
  Replacing secant with Newton's method would be an easy exercise.
  The same for replacing \code{ode45} with some other solver.
}
\seealso{
  \code{\link{bvp}}
}
\examples{
#-- Example 1
f <- function(t, y1, y2) -2*y1*y2
h <- function(u, v) u + v - 0.25

t0 <- 0; tfinal <- 1
y0 <- 1
sol <- shooting(f, t0, tfinal, y0, h, 0, 1)
\dontrun{
plot(sol$t, sol$y[, 1], type='l', ylim=c(-1, 1))
xs <- linspace(0, 1); ys <- 1/(xs+1)
lines(xs, ys, col="red")
lines(sol$t, sol$y[, 2], col="gray")
grid()}

#-- Example 2
f <- function(t, y1, y2) -y2^2 / y1
h <- function(u, v) u - 2
t0 <- 0; tfinal <- 1
y0 <- 1
sol <- shooting(f, t0, tfinal, y0, h, 0, 1)
}
\keyword{ode}
