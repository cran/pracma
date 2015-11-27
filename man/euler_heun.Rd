\name{euler_heun}
\alias{euler_heun}
\title{
  Euler-Heun ODE Solver
}
\description{
  Euler and Euler-Heun ODE solver.
}
\usage{
euler_heun(f, a, b, y0, n = 100, improved = TRUE, ...)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)}.}
  \item{a, b}{start and end points of the interval.}
  \item{y0}{starting value at a.}
  \item{n}{number of grid points.}
  \item{improved}{logical; shall the Heun method be used; default TRUE.}
  \item{...}{additional parameters to be passed to the function.}
}
\details{
  \code{euler_heun} is an integration method for ordinary differential
  equations using the simple Euler resp. the improved Euler-Heun Method.
}
\value{
  List with components \code{t} for grid (or `time') points, and \code{y} 
  the vector of predicted values at those grid points.
}
\references{
  Quarteroni, A., and F. Saleri (). Scientific Computing with MATLAB
  and Octave. Second Edition, Springer-Verlag, Berlin Heidelberg, 2006.
}
\seealso{
  \code{\link{cranknic}}
}
\examples{
##  Flame-up process
f <- function(x, y) y^2 - y^3
s1 <- cranknic(f, 0, 200, 0.01)
s2 <- euler_heun(f, 0, 200, 0.01)
\dontrun{
plot(s1$t, s1$y, type="l", col="blue")
lines(s2$t, s2$y, col="red")
grid()}
}
\keyword{ ode }
