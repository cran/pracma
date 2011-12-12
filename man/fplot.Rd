\name{fplot}
\alias{fplot}
\title{
  Plot Multivariate Function
}
\description{
  Plots components of a (multivariate) function between the specifies limits.
}
\usage{
fplot(f, a, b, n = 101, vectorized = FALSE,
      col = 1:8, lty = 1, lwd = 1, ...)
}
\arguments{
    \item{f}{multivariate function, vectorized or not.}
    \item{a, b}{begin and end of the interval.}
    \item{n}{number of grid points.}
    \item{vectorized}{logical; is the function vectorized (in column form)?)}
    \item{col}{vector of colors; default 1:8}
    \item{lty}{vector of line types.}
    \item{lwd}{vector of line widths.}
    \item{...}{more parameters to be passed to the plotting routine.}
}
\details{
  The function must be of the form \code{y=f(x)}, where \code{x} is a vector
  whose range fulfills the limits, and \code{y} is a vector. If the function
  returns more than one value for a given \code{x}, then \code{y} is a matrix
  whose columns contain each component of \code{f(x)}.

  For example, if \code{f(x)} returns \code{(f1(x),f2(x),f3(x))} then for
  input \code{(x1,x2)} the function should return the matrix

  \code{f1(x1) f2(x1) f3(x1)}\cr
  \code{f1(x2) f2(x2) f3(x2)}

  At least, this is assumed when \code{vectorized=TRUE}. If this is not the
  case or it turns out that \code{size(f(c(a,b)), 1)} is not 2, then this
  function matrix will be generated in a loop. 
}
\value{
  A graph, no return value.
}
\note{
  \code{matplot} will plot the graph if the matrix is already generated.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\seealso{
  \code{\link{matplot}}
}
\examples{
\dontrun{
f <- function(x) cbind(x, x^2, x^3, x^4, x^5, x^6, x^7, x^8)
fplot(f, 0, 1)
}
}
\keyword{ graphs }
