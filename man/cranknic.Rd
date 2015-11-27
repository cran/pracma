\name{cranknic}
\alias{cranknic}
\title{
  Crank-Nicolson Method
}
\description{
  The Crank-Nicolson method for solving ordinary differential equations is a
  combination of the generic steps of the forward and backward Euler methods.
}
\usage{
cranknic(f, t0, t1, y0, ..., N = 100)
}
\arguments{
  \item{f}{function in the differential equation \eqn{y' = f(x, y)};\cr
           defined as a function \eqn{R \times R^m \rightarrow R^m}, where
           \eqn{m} is the number of equations.}
  \item{t0, t1}{start and end points of the interval.}
  \item{y0}{starting values as row or column vector;
    for \eqn{m} equations \code{y0} needs to be a vector of length \code{m}.}
  \item{N}{number of steps.}
  \item{...}{Additional parameters to be passed to the function.}
}
\details{
  Adding together forward and backword Euler method in the \code{cranknic}
  method is by finding the root of the function merging these two formulas.

  No attempt is made to catch any errors in the root finding functions.
}
\value{
  List with components \code{t} for grid (or `time') points between \code{t0}
  and \code{t1}, and \code{y} an n-by-m matrix with solution variables in
  columns, i.e. each row contains one time stamp.
}
\references{
  Quarteroni, A., and F. Saleri (2006). Scientific Computing With MATLAB and
  Octave. Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  This is for demonstration purposes only; for real problems or applications
  please use \code{ode23} or \code{rkf54}.
}
\seealso{
  \code{\link{ode23}}, \code{\link{newmark}}
}
\examples{
##  Newton's example
f <- function(x, y) 1 - 3*x + y + x^2 + x*y
sol100  <- cranknic(f, 0, 1, 0, N = 100)
sol1000 <- cranknic(f, 0, 1, 0, N = 1000)

\dontrun{
# Euler's forward approach
feuler <- function(f, t0, t1, y0, n) {
    h <- (t1 - t0)/n;  x <- seq(t0, t1, by = h)
    y <- numeric(n+1); y[1] <- y0
    for (i in 1:n) y[i+1] <- y[i] + h * f(x[i], y[i])
    return(list(x = x, y = y))
}

solode <- ode23(f, 0, 1, 0)
soleul <- feuler(f, 0, 1, 0, 100)

plot(soleul$x, soleul$y, type = "l", col = "blue", 
     xlab = "", ylab = "", main = "Newton's example")
lines(solode$t, solode$y, col = "gray", lwd = 3)
lines(sol100$t, sol100$y, col = "red")
lines(sol1000$t, sol1000$y, col = "green")
grid()

##  System of differential equations
# "Herr und Hund"
fhh <- function(x, y) {
    y1 <- y[1]; y2 <- y[2]
    s <- sqrt(y1^2 + y2^2)
    dy1 <- 0.5 - 0.5*y1/s
    dy2 <- -0.5*y2/s
    return(c(dy1, dy2))
}

sol <- cranknic(fhh, 0, 60, c(0, 10))
plot(sol$y[, 1], sol$y[, 2], type = "l", col = "blue",
     xlab = "", ylab = "", main = '"Herr und Hund"')
grid()}
}
\keyword{ ode }
