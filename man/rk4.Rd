\name{rk4, rk4sys}
\alias{rk4}
\alias{rk4sys}
\title{
  Classical Runge-Kutta
}
\description{
  Classical Runge-Kutta of order 4.
}
\usage{
rk4(f, a, b, y0, n)

rk4sys(f, a, b, y0, n)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)};\cr
           defined as a function \eqn{R \times R^m \rightarrow R^m}, where \eqn{m}
           is the number of equations.}
  \item{a, b}{endpoints of the interval.}
  \item{y0}{starting values; for \eqn{m} equations \code{y0} needs to be
                a vector of length \code{m}.}
  \item{n}{the number of steps from \code{a} to \code{b}.}
}
\details{
  Classical Runge-Kutta of order 4 for (systems of) ordinary differential
  equations with fixed step size.
}
\value{
  List with components \code{x} for grid points between \code{a} and \code{b}
  and \code{y} an n-by-m matrix with solutions for variables in columns, i.e.
  each row contains one time stamp.
}
\references{
  Fausett, L. V. (2007). Applied Numerical Analysis Using Matlab.
  Second edition, Prentice Hall.
}
\note{
  This function serves demonstration purposes only.
}
\seealso{
  \code{\link{ode23}}, \code{\link{deval}}
}
\examples{
##  Example1: ODE
# y' = y*(-2*x + 1/x) for x != 0, 1 if x = 0
# solution is x*exp(-x^2)
f <- function(x, y) {
	if (x != 0) dy <- y * (- 2*x + 1/x)
	else        dy <- rep(1, length(y))
	return(dy)
}
sol <- rk4(f, 0, 2, 0, 50)
\dontrun{
x <- seq(0, 2, length.out = 51)
plot(x, x*exp(-x^2), type = "l", col = "red")
points(sol$x, sol$y, pch = "*")
grid()}

##  Example2: Chemical process
  f <- function(t, u) {
    u1 <- u[3] - 0.1 * (t+1) * u[1]
    u2 <- 0.1 * (t+1) * u[1] - 2 * u[2]
    u3 <- 2 * u[2] - u[3]
    return(c(u1, u2, u3))
  }
u0 <- c(0.8696, 0.0435, 0.0870)
a <- 0; b <- 40
n <- 40
sol <- rk4sys(f, a, b, u0, n)
\dontrun{
matplot(sol$x, sol$y, type = "l", lty = 1, lwd = c(2, 1, 1),
        col = c("darkred", "darkblue", "darkgreen"),
        xlab = "Time [min]", ylab= "Concentration [Prozent]",
        main = "Chemical composition")
grid()}
}
\keyword{ ode }
