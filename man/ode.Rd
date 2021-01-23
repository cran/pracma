\name{ode23}
\alias{ode23}
\alias{ode23s}
\alias{ode45}
\alias{ode78}
\title{
  Non-stiff (and stiff) ODE solvers
}
\description{
  Runge-Kutta (2, 3)-method with variable step size, resp. (4,5)-method
  with Dormand-Price coefficients, or (7,8)-pairs with Fehlberg coefficients.
  The function \code{f(t, y)} has to return the derivative as a column vector.
}
\usage{
ode23(f, t0, tfinal, y0, ..., rtol = 1e-3, atol = 1e-6)

ode23s(f, t0, tfinal, y0, jac = NULL, ...,
            rtol = 1e-03, atol = 1e-06, hmax = 0.0)

ode45(f, t0, tfinal, y0, ..., atol = 1e-6, hmax = 0.0)
ode78(f, t0, tfinal, y0, ..., atol = 1e-6, hmax = 0.0)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)};\cr
           defined as a function \eqn{R \times R^m \rightarrow R^m}, where
           \eqn{m} is the number of equations.}
  \item{t0, tfinal}{start and end points of the interval.}
  \item{y0}{starting values as column vector;
    for \eqn{m} equations \code{u0} needs to be a vector of length \code{m}.}
  \item{jac}{jacobian of \code{f} as a function of \code{x} alone;
    if not specified, a finite difference approximation will be used.}
  \item{rtol, atol}{relative and absolute tolerance.}
  \item{hmax}{maximal step size, default is \code{(tfinal - t0)/10.}}
  \item{...}{Additional parameters to be passed to the function.}
}
\details{
  \code{ode23} is an integration method for systems of ordinary differential
  equations using second and third order Runge-Kutta-Fehlberg formulas with
  automatic step-size.

  \code{ode23s} can be used to solve a stiff system of ordinary differential
  equations, based on a modified Rosenbrock triple method of order (2,3);
  See section 4.1 in [Shampine and Reichelt].

  \code{ode45} implements Dormand-Prince (4,5) pair that minimizes the local
  truncation error in the 5th-order estimate which is what is used to step
  forward (local extrapolation). Generally it produces more accurate results
  and costs roughly the same computationally.

  \code{ode78} implements Fehlberg's (7,8) pair and is a 7th-order accurate
  integrator therefore the local error normally expected is O(h^8).  However,
  because this particular implementation uses the 8th-order estimate for xout
  (i.e. local extrapolation) moving forward with the 8th-order estimate will
  yield errors on the order of O(h^9). It requires 13 function evaluations per
  integration step.
}
\value{
  List with components \code{t} for grid (or `time') points between \code{t0}
  and \code{tfinal}, and \code{y} an n-by-m matrix with solution variables in
  columns, i.e. each row contains one time stamp.
}
\references{
  Ascher, U. M., and L. R. Petzold (1998). Computer Methods for Ordinary
  Differential Equations and Differential-Algebraic Equations. SIAM.

  L.F. Shampine and M.W. Reichelt (1997). The MATLAB ODE Suite.
  SIAM Journal on Scientific Computing, Vol. 18, pp. 1-22.

  Moler, C. (2004). Numerical Computing with Matlab. Revised Reprint, SIAM.
  \url{https://www.mathworks.com/moler/chapters.html}.
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

##  Example3: Van der Pol as stiff equation
vdP  <- function(t,y) as.matrix(c(y[2], 10*(1-y[1]^2)*y[2]-y[1]))
ajax <- function(t, y)
            matrix(c(0, 1, -20*y[1]*y[2]-1, 10*(1-y[1]^2)), 2,2, byrow = TRUE)
sol <- ode23s(vdP, t0, tf, c(2, 0), jac = ajax, hmax = 1.0)
\dontrun{
plot(sol$t, sol$y[, 1], col = "blue")
lines(sol$t, sol$y[, 1], col = "blue")
lines(sol$t, sol$y[, 2]/8, col = "red", lwd = 2)
grid()}

##  Example4: pendulum
m = 1.0;  l = 1.0   # [kg] resp. [m]
g = 9.81; b = 0.7   # [m/s^2] resp. [N s/m]
fp = function(t, x)
        c( x[2] , 1/(1/3*m*l^2)*(-b*x[2]-m*g*l/2*sin(x[1])) )
t0 <- 0.0; tf <- 5.0; hmax = 0.1
y0 = c(30*pi/180, 0.0)
sol = ode45(fp, t0, tf, y0, hmax = 0.1)
\dontrun{
matplot(sol$t, sol$y, type = "l", lty = 1)
grid()}

##  Example: enforced pendulum
g <- 9.81
L <- 1.0; Y <- 0.25; w <- 2.5
f <- function(t, y) {
    as.matrix(c(y[2], -g/L * sin(y[1]) + w^2/L * Y * cos(y[1]) * sin(w*t)))
}
y0 <- as.matrix(c(0, 0))
sol <- ode78(f, 0.0, 60.0, y0, hmax = 0.05)
\dontrun{
plot(sol$t, sol$y[, 1], type="l", col="blue")
grid()}
}
\keyword{ ode }
