\name{odregress}
\alias{odregress}
\title{
  Orthogonal Distance Regression
}
\description{
  Orthogonal Distance Regression (ODR, a.k.a. total least squares) 
  is a regression technique in which observational errors on both dependent 
  and independent variables are taken into account.
}
\usage{
odregress(x, y)
}
\arguments{
  \item{x}{matrix of independent variables.}
  \item{y}{vector representing dependent variable.}
}
\details{
  The implementation used here is applying PCA resp. the singular value 
  decomposition on the matrix of independent and dependent variables.
}
\value{
  Returns list with components \code{coeff} linear coefficients and intercept 
  term, \code{ssq} sum of squares of orthogonal distances to the linear line 
  or hyperplane, \code{err} the orthogonal distances, \code{fitted} the 
  fitted values,  \code{resid} the residuals, and \code{normal} the normal 
  vector to the hyperplane.
}
\references{
  Golub, G.H., and C.F. Van Loan (1980). An analysis of the total least 
  squares problem.\cr
  Numerical Analysis, Vol. 17, pp. 883-893.

  See ODRPACK or ODRPACK95 (TOMS Algorithm 676).\cr
  URL: https://docs.scipy.org/doc/external/odr_ams.pdf
}
\note{
  The ``geometric mean" regression not implemented because questionable.
}
\seealso{
  \code{\link{lm}}
}
\examples{
# Example in one dimension
x <- c(1.0, 0.6, 1.2, 1.4, 0.2)
y <- c(0.5, 0.3, 0.7, 1.0, 0.2)
odr <- odregress(x, y)
( cc <- odr$coeff )
# [1]  0.65145762 -0.03328271
lm(y ~ x)
# Coefficients:
# (Intercept)            x 
#    -0.01379      0.62931 

# Prediction
xnew <- seq(0, 1.5, by = 0.25)
( ynew <- cbind(xnew, 1) \%*\% cc )

\dontrun{
plot(x, y, xlim=c(0, 1.5), ylim=c(0, 1.2), main="Orthogonal Regression")
abline(lm(y ~ x), col="blue")
lines(c(0, 1.5), cc[1]*c(0, 1.5) + cc[2], col="red")
points(xnew, ynew, col = "red")
grid()}

# Example in two dimensions
x <- cbind(c(0.92, 0.89, 0.85, 0.05, 0.62, 0.55, 0.02, 0.73, 0.77, 0.57),
           c(0.66, 0.47, 0.40, 0.23, 0.17, 0.09, 0.92, 0.06, 0.09, 0.60))
y <- x \%*\% c(0.5, 1.5) + 1
odr <- odregress(x, y); odr
# $coeff
# [1] 0.5 1.5 1.0
# $ssq
# [1] 1.473336e-31

y <- y + rep(c(0.1, -0.1), 5)
odr <- odregress(x, y); odr
# $coeff
# [1] 0.5921823 1.6750269 0.8803822
# $ssq
# [1] 0.02168174

lm(y ~ x)
# Coefficients:
# (Intercept)           x1           x2  
#      0.9153       0.5671       1.6209  
}

\keyword{ fitting }
