\name{bulirsch-stoer}
\alias{bulirsch_stoer}
\alias{midpoint}
\title{
  Bulirsch-Stoer Algorithm
}
\description{
  Bulirsch-Stoer algorithm for solving Ordinary Differential Equations (ODEs)
  very accurately.
}
\usage{
bulirsch_stoer(f, t, y0, ..., tol = 1e-07)

midpoint(f, t0, tfinal, y0, tol = 1e-07, kmax = 25)
}
\arguments{
  \item{f}{function describing the differential equation \eqn{y' = f(t, y)}.}
  \item{t}{vector of \code{x}-values where the values of the ODE function
           will be computed; needs to be increasingly sorted.}
  \item{y0}{starting values as column vector.}
  \item{...}{additional parameters to be passed to the function.}
  \item{tol}{relative tolerance in the Ricardson extrapolation.}
  \item{t0, tfinal}{start and end point of the interval.}
  \item{kmax}{maximal number of steps in the Richardson extrapolation.}
}
\details{
  The Bulirsch-Stoer algorithm is a well-known method to obtain high-accuracy
  solutions to ordinary differential equations with reasonable computational
  efforts. It exploits the midpoint method to get good accuracy in each step.
  
  The (modified) midpoint method computes the values of the dependent
  variable \code{y(t)} from \code{t0} to \code{tfinal} by a sequence of
  substeps, applying Richardson extrapolation in each step.

  Bulirsch-Stoer and midpoint shall not be used with non-smooth functions or
  singularities inside the interval. The best way to get intermediate points
  \code{t = (t[1], ..., t[n])} may be to call \code{ode23} or \code{ode23s}
  first and use the \code{x}-values returned to start \code{bulirsch_stoer}
  on.
}
\value{
  bulirsch_stoer returns a list with \code{x} the grid points input, and
  \code{y} a vector of function values at the se points.
}
\references{
  J. Stoer and R. Bulirsch (2002). Introduction to Numerical Analysis.
  Third Edition, Texts in Applied Mathematics 12, Springer Science +
  Business, LCC, New York.
}
\author{
  Copyright (c) 2014 Hans W Borchers
}
\note{
  Will be extended to become a full-blown Bulirsch-Stoer implementation.
}
\seealso{
  \code{\link{ode23}}, \code{\link{ode23s}}
}
\examples{
## Example: y'' = -y
f1 <- function(t, y) as.matrix(c(y[2], -y[1]))
y0 <- as.matrix(c(0.0, 1.0))
tt <- linspace(0, pi, 13)
yy <- bulirsch_stoer(f1, tt, c(0.0, 1.0))   # 13 equally-spaced grid points
yy[nrow(yy), 1]                             # 1.1e-11

\dontrun{
S  <- ode23(f1, 0, pi, c(0.0, 1.0))
yy <- bulirsch_stoer(f1, S$t, c(0.0, 1.0))  # S$x 13 irregular grid points
yy[nrow(yy), 1]                             #  2.5e-11
S$y[nrow(S$y), 1]                           # -7.1e-04

## Example: y' = -200 x y^2                 # y(x) = 1 / (1 + 100 x^2)
f2 <- function(t, y) -200 * t * y^2
y0 < 1
tic(); S <- ode23(f2, 0, 1, y0); toc()            # 0.002 sec
tic(); yy <- bulirsch_stoer(f2, S$t, y0); toc()   # 0.013 sec}
}
\keyword{ ode }
