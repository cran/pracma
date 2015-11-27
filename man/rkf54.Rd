\name{rkf54}
\alias{rkf54}
\title{
  Runge-Kutta-Fehlberg
}
\description{
  Runge-Kutta-Fehlberg with adaptive step size.
}
\usage{
rkf54(f, a, b, y0, tol = .Machine$double.eps^0.5,
                   control = list(), ...)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)}.}
  \item{a, b}{endpoints of the interval.}
  \item{y0}{starting values at \code{a}.}
  \item{tol}{relative tolerance, used for determining the step size.}
  \item{control}{list for influencing the step size with components\cr
                 \code{hmin, hmax}, the minimal, maximal step size\cr
                 \code{jmax}, the maximally allowed number of steps.}
  \item{\ldots}{additional parameters to be passed to the function.}
}
\details{
  Runge-Kutta-Fehlberg is a kind of Runge-Kutta method of solving ordinary
  differential equations of order (5, 4) with variable step size.

  ``At each step, two different approximations for the solution are made and
  compared.  If the two answers are in close agreement, the approximation is
  accepted. If the two answers do not agree to a specified accuracy, the step
  size is reduced.  If the answers agree to more significant digits than
  required, the step size is increased.''

  Some textbooks promote the idea to use the five-order formula as the
  accepted value instead of using it for error estimation. This approach
  is taken here, that is why the function is called \code{rkf54}. The idea
  is still debated as the accuracy determinations appears to be diminished.
}
\value{
  List with components \code{x} for grid points between \code{a} and \code{b}
  and \code{y} the function values of the numerical solution.
}
\references{
  Stoer, J., and R. Bulirsch (2002). Introduction to Numerical Analysis.
  Third Edition, Springer-Verlag, New York.

  Mathematica code associated with the book:\cr
  Mathews, J. H., and K. D. Fink (2004). Numerical Methods Using Matlab.
  Fourth Edition, Prentice Hall.
}
\note{
  This function serves demonstration purposes only.
}
\seealso{
  \code{\link{rk4}}, \code{\link{ode23}}
}
\examples{
# Example: y' = 1 + y^2
f1 <- function(x, y)  1 + y^2
sol11 <- rkf54(f1, 0, 1.1, 0.5, control = list(hmin = 0.01))
sol12 <- rkf54(f1, 0, 1.1, 0.5, control = list(jmax =  250))

# Riccati equation: y' = x^2 + y^2
f2 <- function(x, y)  x^2 + y^2
sol21 <- rkf54(f2, 0, 1.5, 0.5, control = list(hmin = 0.01))
sol22 <- rkf54(f2, 0, 1.5, 0.5, control = list(jmax =  250))

\dontrun{
plot(0, 0, type = "n", xlim = c(0, 1.5), ylim = c(0, 20),
     main = "Riccati", xlab = "", ylab = "")
points(sol11$x, sol11$y, pch = "*", col = "darkgreen")
lines(sol12$x, sol12$y)
points(sol21$x, sol21$y, pch = "*", col = "blue")
lines(sol22$x, sol22$y)
grid()}
}
\keyword{ ode }
