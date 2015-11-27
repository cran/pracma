\name{curvefit}
\alias{curvefit}
\title{
  Parametric Curve Fit
}
\description{
  Polynomial fitting of parametrized points on 2D curves, also
  requiring to meet some points exactly.
}
\usage{
curvefit(u, x, y, n, U = NULL, V = NULL)
}
\arguments{
  \item{u}{the parameter vector.}
  \item{x, y}{x-, y-coordinates for each parameter value.}
  \item{n}{order of the polynomials, the same in x- and y-dirction.}
  \item{U}{parameter values where points will be fixed.}
  \item{V}{matrix with two columns and \code{lemgth(U)} rows;
           first column contains the x-, the second the y-values of those
           points kept fixed.}
}
\details{
  This function will attempt to fit two polynomials to parametrized curve 
  points using the linear least squares approach with linear equality 
  constraints in \code{lsqlin}. The requirement to meet exactly some fixed 
  points is interpreted as a linear equality constraint.
}
\value{
  Returns a list with 4 components, \code{xp} and \code{yp} coordinates of 
  the fitted points, and \code{px} and \code{py} the coefficients of the
  fitting polynomials in x- and y-direction.
}
\note{
  In the same manner, derivatives/directions could be prescribed at certain
  points.
}
\seealso{
  \code{\link{circlefit}}, \code{\link{lsqlin}}
}
\examples{
##  Approximating half circle arc with small perturbations
N <- 50
u <- linspace(0, pi, N)
x <- cos(u) + 0.05 * randn(1, N)
y <- sin(u) + 0.05 * randn(1, N)
n <- 8
cfit1 <- curvefit(u, x, y, n)
\dontrun{
plot(x, y, col = "darkgray", pch = 19, asp = 1)
xp <- cfit1$xp; yp <- cfit1$yp
lines(xp, yp, col="blue")
grid()}

##  Fix the end points at t = 0 and t = pi
U <- c(0, pi)
V <- matrix(c(1, 0, -1, 0), 2, 2, byrow = TRUE)
cfit2 <- curvefit(u, x, y, n, U, V)
\dontrun{
xp <- cfit2$xp; yp <- cfit2$yp
lines(xp, yp, col="red")}

\dontrun{
##  Archimedian spiral
n <- 8
u <- linspace(0, 3*pi, 50)
a <- 1.0
x <- as.matrix(a*u*cos(u))
y <- as.matrix(a*u*sin(u))
plot(x, y, type = "p", pch = 19, col = "darkgray", asp = 1)
lines(x, y, col = "darkgray", lwd = 3)
cfit <- curvefit(u, x, y, n)
px <- c(cfit$px); py <- c(cfit$py)
v <- linspace(0, 3*pi, 200)
xs <- polyval(px, v)
ys <- polyval(py, v)
lines(xs, ys, col = "navy")
grid()}
}
\keyword{ fitting }
