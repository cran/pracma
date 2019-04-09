\name{broyden}
\alias{broyden}
\title{Broyden's Method}
\description{
  Broyden's method for the numerical solution of nonlinear systems of
  \code{n} equations in \code{n} variables.
}
\usage{
broyden(Ffun, x0, J0 = NULL, ...,
        maxiter = 100, tol = .Machine$double.eps^(1/2))
}
\arguments{
\item{Ffun}{\code{n} functions of \code{n} variables.}
\item{x0}{Numeric vector of length \code{n}.}
\item{J0}{Jacobian of the function at \code{x0}.}
\item{...}{additional parameters passed to the function.}
\item{maxiter}{Maximum number of iterations.}
\item{tol}{Tolerance, relative accuracy.}
}
\details{
  F as a function must return a vector of length \code{n}, and accept an
  \code{n}-dim. vector or column vector as input. F must not be univariate,
  that is \code{n} must be greater than 1.

  Broyden's method computes the Jacobian and its inverse only at the first
  iteration, and does a rank-one update thereafter, applying the so-called
  Sherman-Morrison formula that computes the inverse of the sum of an
  invertible matrix A and the dyadic product, uv', of a column vector u and
  a row vector v'.
}
\value{
  List with components: \code{zero} the best root found so far, \code{fnorm}
  the square root of sum of squares of the values of f, and \code{niter} the
  number of iterations needed.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  Applied to a system of \code{n} linear equations it will stop in
  \code{2n} steps
}
\seealso{
  \code{\link{newtonsys}}, \code{\link{fsolve}}
}
\examples{
##  Example from Quarteroni & Saleri
F1 <- function(x) c(x[1]^2 + x[2]^2 - 1, sin(pi*x[1]/2) + x[2]^3)
broyden(F1, x0 = c(1, 1))
# zero: 0.4760958 -0.8793934; fnorm: 9.092626e-09; niter: 13

F <- function(x) {
    x1 <- x[1]; x2 <- x[2]; x3 <- x[3]
    as.matrix(c(x1^2 + x2^2 + x3^2 - 1,
                x1^2 + x3^2 - 0.25,
                x1^2 + x2^2 - 4*x3), ncol = 1)
}
x0 <- as.matrix(c(1, 1, 1))
broyden(F, x0)
# zero: 0.4407629 0.8660254 0.2360680; fnorm: 1.34325e-08; niter: 8

##  Find the roots of the complex function sin(z)^2 + sqrt(z) - log(z)
F2 <- function(x) {
    z  <- x[1] + x[2]*1i
    fz <- sin(z)^2 + sqrt(z) - log(z)
    c(Re(fz), Im(fz))
}
broyden(F2, c(1, 1))
# zero   0.2555197 0.8948303 , i.e.  z0 = 0.2555 + 0.8948i
# fnorm  7.284374e-10
# niter  13

##  Two more problematic examples
F3 <- function(x)
        c(2*x[1] - x[2] - exp(-x[1]), -x[1] + 2*x[2] - exp(-x[2]))
broyden(F3, c(0, 0))
# $zero   0.5671433 0.5671433   # x = exp(-x)

F4 <- function(x)   # Dennis Schnabel
        c(x[1]^2 + x[2]^2 - 2, exp(x[1] - 1) + x[2]^3 - 2)
broyden(F4, c(2.0, 0.5), maxiter = 100)
}
\keyword{ math }
