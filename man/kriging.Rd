\name{kriging}
\alias{kriging}
\title{
  Interpolation by Kriging
}
\description{
  Simple and ordinary Kriging interpolation and interpolating function.
}
\usage{
kriging(u, v, u0, type = c("ordinary", "simple"))
}
\arguments{
  \item{u}{an \code{nxm}-matrix of n points in the m-dimensional space.}
  \item{v}{an \code{n}-dim. (column) vector of interpolation values.}
  \item{u0}{a \code{kxm}-matrix of k points in \code{R^m} to be interpolated.}
  \item{type}{character; values `simple' or `ordinary'; no partial matching.}
}
\details{
  Kriging is a geo-spatial estimation procedure that estimates points based
  on the variations of known points in a non-regular grid. It is especially
  suited for surfaces.
}
\note{
  In the literature, different versions and extensions are discussed.
}
\value{
  \code{kriging} returns a \code{k}-dim. vektor of interpolation values.
}
\references{
  Press, W. H., A. A. Teukolsky, W. T. Vetterling, and B. P. Flannery (2007).
  Numerical recipes: The Art of Scientific Computing (3rd Ed.). Cambridge
  University Press, New York, Sect. 3.7.4, pp. 144-147.
}
\seealso{
  \code{\link{akimaInterp}}, \code{\link{barylag2d}}, package \code{kriging}
}
\examples{
##  Interpolate the Saddle Point function
f <- function(x) x[1]^2 - x[2]^2       # saddle point function

set.seed(8237)
n <- 36
x <- c(1, 1, -1, -1, runif(n-4, -1, 1)) # add four vertices
y <- c(1, -1, 1, -1, runif(n-4, -1, 1))
u <- cbind(x, y)
v <- numeric(n)
for (i in 1:n) v[i] <- f(c(x[i], y[i]))

kriging(u, v, c(0, 0))                      #=>  0.006177183
kriging(u, v, c(0, 0), type = "simple")     #=>  0.006229557

\dontrun{
xs <- linspace(-1, 1, 101)              # interpolation on a diagonal
u0 <- cbind(xs, xs)

yo <- kriging(u, v, u0, type = "ordinary")  # ordinary kriging
ys <- kriging(u, v, u0, type = "simple")    # simple kriging
plot(xs, ys, type = "l", col = "blue", ylim = c(-0.1, 0.1),
             main = "Kriging interpolation along the diagonal")
lines(xs, yo, col = "red")
legend( -1.0, 0.10, c("simple kriging", "ordinary kriging", "function"),
        lty = c(1, 1, 1), lwd = c(1, 1, 2), col=c("blue", "red", "black"))
grid()
lines(c(-1, 1), c(0, 0), lwd = 2)}

##  Find minimum of the sphere function
f <- function(x, y) x^2 + y^2 + 100
v <- bsxfun(f, x, y)

ff <- function(w) kriging(u, v, w)
ff(c(0, 0))                                 #=>  100.0317
\dontrun{
optim(c(0.0, 0.0), ff)
# $par:   [1]  0.04490075 0.01970690
# $value: [1]  100.0291
ezcontour(ff, c(-1, 1), c(-1, 1))
points(0.04490075, 0.01970690, col = "red")}
}
\keyword{ fitting }
