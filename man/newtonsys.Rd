\name{newtonsys}
\alias{newtonsys}
\title{Newton Method for Nonlinear Systems}
\description{
  Newton's method applied to multivariate nonlinear functions.
}
\usage{
newtonsys(Ffun, x0, Jfun = NULL, ...,
    	  maxiter = 100, tol = .Machine$double.eps^(1/2))
}
\arguments{
\item{Ffun}{\code{m} functions of \code{n} variables.}
\item{Jfun}{Function returning a square \code{n}-by-\code{n} matrix
            (of partial derivatives) or \code{NULL}, the default.}
\item{x0}{Numeric vector of length \code{n}.}
\item{maxiter}{Maximum number of iterations.}
\item{tol}{Tolerance, relative accuracy.}
\item{...}{Additional parameters to be passed to f.}
}
\details{
  Solves the system of equations applying Newton's method with the univariate
  derivative replaced by the Jacobian.
}
\value{
  List with components: \code{zero} the root found so far, \code{fnorm} the
  square root of sum of squares of the values of f, and \code{iter} the
  number of iterations needed.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  TODO: better error checking, e.g. when the Jacobian is not invertible.
}
\seealso{
  \code{\link{newtonRaphson}}, \code{\link{broyden}}
}
\examples{
##  Example from Quarteroni & Saleri
F1 <- function(x) c(x[1]^2 + x[2]^2 - 1, sin(pi*x[1]/2) + x[2]^3)
newtonsys(F1, x0 = c(1, 1))  # zero: 0.4760958 -0.8793934

##  Find the roots of the complex function sin(z)^2 + sqrt(z) - log(z)
F2 <- function(x) {
    z  <- x[1] + x[2]*1i
    fz <- sin(z)^2 + sqrt(z) - log(z)
    c(Re(fz), Im(fz))
}
newtonsys(F2, c(1, 1))
# $zero   0.2555197 0.8948303 , i.e.  z0 = 0.2555 + 0.8948i
# $fnorm  2.220446e-16
# $niter  8

##  Two more problematic examples
F3 <- function(x)
        c(2*x[1] - x[2] - exp(-x[1]), -x[1] + 2*x[2] - exp(-x[2]))
newtonsys(F3, c(0, 0))
# $zero   0.5671433 0.5671433
# $fnorm  0
# $niter  4

\dontrun{
F4 <- function(x)  # Dennis Schnabel
        c(x[1]^2 + x[2]^2 - 2, exp(x[1] - 1) + x[2]^3 - 2)
newtonsys(F4, c(2.0, 0.5))
# will result in an error ``missing value in  ... err<tol && niter<maxiter''}
}
\keyword{ math }
