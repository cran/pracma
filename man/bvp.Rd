\name{bvp}
\alias{bvp}
\title{
  Boundary Value Problems
}
\description{
  Solves boundary value problems of linear second order differential equations.
}
\usage{
bvp(f, g, h, x, y, n = 50)
}
\arguments{
  \item{f, g, h}{functions on the right side of the differential equation.
        If \code{f, g} or \code{h} is a scalar instead of a function, it is
        assumed to be a constant coefficient in the differential equation.}
  \item{x}{\code{x[1], x[2]} are the interval borders where the solution
           shall be computed.}
  \item{y}{boundary conditions such that
           \code{y(x[1]) = y[1], y(x[2]) = y[2]}.}
  \item{n}{number of intermediate grid points; default 50.}
}
\details{
  Solves the two-point boundary value problem given as a linear differential 
  equation of second order in the form:
  \deqn{y'' = f(x) y' + g(x) y + h(x)}
  with the finite element method. The solution \eqn{y(x)} shall exist
  on the interval \eqn{[a, b]} with boundary conditions \eqn{y(a) = y_a} 
  and \eqn{y(b) = y_b}.
}
\value{
  Returns a list \code{list(xs, ys)} with the grid points \code{xs} and the
  values \code{ys} of the solution at these points, including the boundary
  points.
}
\references{
  Kutz, J. N. (2005). Practical Scientific Computing. Lecture Notes 
  98195-2420, University of Washington, Seattle.
}
\note{
  Uses a tridiagonal equation solver that may be faster then \code{qr.solve}
  for large values of \code{n}.
}
\seealso{
  \code{\link{shooting}}
}
\examples{
##  Solve y'' = 2*x/(1+x^2)*y' - 2/(1+x^2) * y + 1
##  with y(0) = 1.25 and y(4) = -0.95 on the interval [0, 4]:
f1 <- function(x) 2*x / (1 + x^2)
f2 <- function(x)  -2 / (1 + x^2)
f3 <- function(x) rep(1, length(x))     # vectorized constant function 1
x <- c(0.0,   4.0)
y <- c(1.25, -0.95)
sol <- bvp(f1, f2, f3, x, y)
\dontrun{
plot(sol$xs, sol$ys, ylim = c(-2, 2),
     xlab = "", ylab = "", main = "Boundary Value Problem")
# The analytic solution is
sfun <- function(x) 1.25 + 0.4860896526*x - 2.25*x^2 + 
                    2*x*atan(x) - 1/2 * log(1+x^2) + 1/2 * x^2 * log(1+x^2)
xx <- linspace(0, 4)
yy <- sfun(xx)
lines(xx, yy, col="red")
grid()}
}
\keyword{ ode }
