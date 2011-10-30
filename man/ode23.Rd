\name{ode23}
\alias{ode23}
\title{
  Runge-Kutta
}
\description{
  Runge-Kutta (2, 3)-method with variable step size.
}
\usage{
ode23(f, t0, tfinal, y0, ..., rtol = 1e-3, atol = 1e-6)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)};\cr
           defined as a function \eqn{R \times R^m \rightarrow R^m}, where
           \eqn{m} is the number of equations.}
  \item{t0, tfinal}{start and end points of the interval.}
  \item{y0}{starting values as column vector;
    for \eqn{m} equations \code{u0} needs to be a vector of length \code{m}.}
  \item{rtol, atol}{relative and absolute tolerance.}
  \item{...}{Additional parameters to be passed to the function.}
}
\details{
  \code{ode23} is an integration method for systems of ordinary differential
  equations using second and third order Runge-Kutta-Fehlberg formulas with
  automatic step-size.
}
\value{
  List with components \code{t} for grid (or `time') points between \code{t0}
  and \code{tfinal}, and \code{y} an n-by-m matrix with solution variables in
  columns, i.e. each row contains one time stamp.
}
\references{
  Moler, C. (2004). Numerical Computing with Matlab. Revised Reprint, SIAM.
  \url{http://www.mathworks.com/moler/chapters.html}.
}
\note{
  Copyright (c) 2004 C. Moler for the Matlab textbook version \code{ode23tx}.
}
\seealso{
  \code{\link{rk4sys}}, \code{\link{deval}}
}
\examples{
##  Example1: Three-body problem
f <- function(t, y)
		as.matrix(c(y[2]*y[3], -y[1]*y[3], 0.51*y[1]*y[2]))
y0 <- as.matrix(c(0, 1, 1))
t0 <- 0; tf <- 20
sol <- ode23(f, t0, tf, y0, rtol=1e-5, atol=1e-10)
\dontrun{
matplot(sol$t, sol$y, type = "l", lty = 1, lwd = c(2, 1, 1),
        col = c("darkred", "darkblue", "darkgreen"),
        xlab = "Time [min]", ylab= "",
        main = "Three-body Problem")
grid()}

##  Example2: Van der Pol Equation
#   x'' + (x^2 - 1) x' + x = 0
f <- function(t, x)
        as.matrix(c(x[1] * (1 - x[2]^2) -x[2], x[1]))
t0 <- 0; tf <- 20
x0 <- as.matrix(c(0, 0.25))
sol <- ode23(f, t0, tf, x0)
\dontrun{
plot(c(0, 20), c(-3, 3), type = "n",
     xlab = "Time", ylab = "", main = "Van der Pol Equation")
lines(sol$t, sol$y[, 1], col = "blue")
lines(sol$t, sol$y[, 2], col = "darkgreen")
grid()}
}
\keyword{ math }
