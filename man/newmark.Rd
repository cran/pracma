\name{newmark}
\alias{newmark}
\title{
  Newmark Method
}
\description{
  Newmark's is a method to solve higher-order differential equations
  without passing through the equivalent first-order system.
  It generalizes the so-called `leap-frog' method.
  Here it is restricted to second-order equations.
}
\usage{
newmark(f, t0, t1, y0, ..., N = 100, zeta = 0.25, theta = 0.5)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y'' = f(x, y, y')};\cr
           defined as a function \eqn{R \times R^2 \rightarrow R}.}
  \item{t0, t1}{start and end points of the interval.}
  \item{y0}{starting values as row or column vector;
            \code{y0} needs to be a vector of length 2, the first component
            representing \code{y(t0)}, the second \code{dy/dt(t0)}.}
  \item{N}{number of steps.}
  \item{zeta, theta}{two non-negative real numbers.}
  \item{...}{Additional parameters to be passed to the function.}
}
\details{
  Solves second order differential equations using the Newmark method
  on an equispaced grid of \code{N} steps.

  Function \code{f} must return a vector, whose elements hold the evaluation
  of \code{f(t,y)}, of the same dimension as \code{y0}. Each row in the
  solution array Y corresponds to a time returned in \code{t}.

  The method is `implicit' unless \code{zeta=theta=0}, second order if
  \code{theta=1/2} and first order accurate if \code{theta!=1/2}.
  \code{theta>=1/2} ensures stability.
  The condition set \code{theta=1/2; zeta=1/4} (the defaults) is a popular
  approach that is unconditionally stable, but introduces oscillatory
  spurious solutions on long time intervals.
  (For these simulations it is preferable to use \code{theta>1/2} and
  \code{zeta>(theta+1/2)^(1/2)}.)

  No attempt is made to catch any errors in the root finding functions.
}
\value{
  List with components \code{t} for grid (or `time') points between \code{t0}
  and \code{t1}, and \code{y} an n-by-2 matrix with solution variables in
  columns, i.e. each row contains one time stamp.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  This is for demonstration purposes only; for real problems or applications
  please use \code{ode23} or \code{rk4sys}.
}
\seealso{
  \code{\link{ode23}}, \code{\link{cranknic}}
}
\examples{
# Mathematical pendulum  m l y'' + m g sin(y) = 0
pendel <- function(t, y)  -sin(y[1])
sol <- newmark(pendel, 0, 4*pi, c(pi/4, 0))

\dontrun{
plot(sol$t, sol$y[, 1], type="l", col="blue",
     xlab="Time", ylab="Elongation/Speed", main="Mathematical Pendulum")
lines(sol$t, sol$y[, 2], col="darkgreen")
grid()}
}
\keyword{ ode }
