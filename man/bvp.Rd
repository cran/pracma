\name{bvp}
\alias{bvp}
\title{
  Solve Boundary Value Problem
}
\description{
  Solves the two-point boundary value problem given as a differential equation
  of the form:
  \deqn{-c_1 y'' + c_2 y' + c_3 y = f(x)}
  with the centered finite difference method. The solution \eqn{y(x)} shall
  exist on the interval \eqn{[a, b]} with boundary conditions \eqn{y(a) = y_a} 
  and \eqn{y(b) = y_b}.
}
\usage{
bvp(f, a, b, ya, yb, N, cc, ...)
}
\arguments{
  \item{f}{function on the right side of the differential equation.}
  \item{a, b}{interval borders where the solution shall be computed.}
  \item{ya, yb}{boundary conditions such that \code{y(a) = ya, y(b) = yb}.}
  \item{N}{number of intermediate grid points.}
  \item{cc}{contains the coefficients \code{cc[1], cc[2], cc[3]} of the equation.}
  \item{...}{additional arguments to be passed to the function \code{f}.}
}
\details{
  Computes the solution as a least-squares approximation.
}
\value{
  Returns a list \code{list(xh, yh)} with the grid points \code{xh} and the
  values \code{yh} of the solution at these points.
}
\references{
  Quarteroni, A., and F. Saleri (2006). Scientific Computing with MATLAB and
  Octave. Springer-Verlag, Berlin Heidelberg.
}
\note{
  Notice that the solution can be affected by spurious oscillations if
  \eqn{h \ge 2 c_1 / c_2}.
}
\seealso{
  \code{feuler}
}
\examples{
##  Solve -y'' + 0.1*y = 1 + sin(4*pi*x) on [0, 1] with y(0) = y(1) = 0.
fun <- function(x) 1 + sin(4*pi*x)
sol <- bvp(fun, 0, 1, 0, 0, 40, c(1, 0, 0.1))
\dontrun{
plot(sol$xh, sol$yh, type="l", col="blue")
grid()}
}
\keyword{ math }
