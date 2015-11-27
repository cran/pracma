\name{complexstep}
\alias{complexstep}
\alias{grad_csd}
\alias{jacobian_csd}
\alias{hessian_csd}
\alias{laplacian_csd}
\title{Complex Step Derivatives}
\description{
  Complex step derivatives of real-valued functions, including gradients,
  Jacobians, and Hessians.
}
\usage{
complexstep(f, x0, h = 1e-20, ...)

grad_csd(f, x0, h = 1e-20, ...)
jacobian_csd(f, x0, h = 1e-20, ...)
hessian_csd(f, x0, h = 1e-20, ...)
laplacian_csd(f, x0, h = 1e-20, ...)
}
\arguments{
  \item{f}{Function that is to be differentiated.}
  \item{x0}{Point at which to differentiate the function.}
  \item{h}{Step size to be applied; shall be \emph{very} small.}
  \item{...}{Additional variables to be passed to \code{f}.}
}
\details{
  Complex step derivation is a fast and highly exact way of numerically
  differentiating a function. If the following conditions are satisfied,
  there will be no loss of accuracy between computing a function value
  and computing the derivative at a certain point.
  \itemize{
    \item \code{f} must have an analytical (i.e., complex differentiable)
    continuation into an open neighborhood of \code{x0}.
    \item \code{x0} \bold{and} \code{f(x0)} must be real.
    \item \code{h} is real and \emph{very} small: \code{0 < h << 1}.
  }

  \code{complexstep} handles differentiation of univariate functions, while
  \code{grad_csd} and \code{jacobian_csd} compute gradients and Jacobians by
  applying the complex step approach iteratively. Please understand that these 
  functions are not vectorized, but \code{complexstep} is.

  As complex step cannot be applied twice (the first derivative does not
  fullfil the conditions), \code{hessian_csd} works differently. For the
  first derivation, complex step is used, to the one time derived function
  Richardson's method is applied. The same applies to \code{lapalacian_csd}.

  
}
\value{
  \code{complexstep(f, x0)} returns the derivative \eqn{f'(x_0)} of \eqn{f}
  at \eqn{x_0}. The function is vectorized in \code{x0}.  
}
\references{
  Martins, J. R. R. A., P. Sturdza, and J. J. Alonso (2003).
  The Complex-step Derivative Approximation.
  ACM Transactions on Mathematical Software, Vol. 29, No. 3, pp. 245--262.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  This surprising approach can be easily deduced from the complex-analytic
  Taylor formula.
}
\seealso{
\code{\link{numderiv}}
}
\examples{
##  Example from Martins et al.
f <- function(x) exp(x)/sqrt(sin(x)^3 + cos(x)^3)  # derivative at x0 = 1.5
# central diff formula    # 4.05342789402801, error 1e-10
# numDeriv::grad(f, 1.5)  # 4.05342789388197, error 1e-12  Richardson
# pracma::numderiv        # 4.05342789389868, error 5e-14  Richardson
complexstep(f, 1.5)       # 4.05342789389862, error 1e-15
# Symbolic calculation:   # 4.05342789389862

jacobian_csd(f, 1.5)

f1 <- function(x) sum(sin(x))
grad_csd(f1, rep(2*pi, 3))
## [1] 1 1 1

laplacian_csd(f1, rep(pi/2, 3))
## [1] -3

f2 <- function(x) c(sin(x[1]) * exp(-x[2]))
hessian_csd(f2, c(0.1, 0.5, 0.9))
##             [,1]        [,2] [,3]
## [1,] -0.06055203 -0.60350053    0
## [2,] -0.60350053  0.06055203    0
## [3,]  0.00000000  0.00000000    0

f3 <- function(u) {
    x <- u[1]; y <- u[2]; z <- u[3]
    matrix(c(exp(x^+y^2), sin(x+y), sin(x)*cos(y), x^2 - y^2), 2, 2)
  }
jacobian_csd(f3, c(1,1,1))
##            [,1]       [,2] [,3]
## [1,]  2.7182818  0.0000000    0
## [2,] -0.4161468 -0.4161468    0
## [3,]  0.2919266 -0.7080734    0
## [4,]  2.0000000 -2.0000000    0
}
\keyword{ math }
