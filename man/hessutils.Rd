\name{Hessian utilities}
\alias{hessvec}
\alias{hessdiag}
\title{Hessian utilities}
\description{
  Fast multiplication of Hessian and vector where computation of the full 
  Hessian is not needed. Or determine the diagonal of the Hessian when
  non-diagonal entries are not needed or are nearly zero.
}
\usage{
  hessvec(f, x, v, csd = FALSE, ...)

  hessdiag(f, x, ...)
}
\arguments{
  \item{f}{function whose hessian is to be computed.}
  \item{x}{point in \code{R^n}.}
  \item{v}{vector of length \code{n}.}
  \item{csd}{logocal, shall complex-step be applied.}
  \item{...}{more arguments to be passed to the function.}
}
\details{
  \code{hessvec} computes the product of a Hessian of a function
  times a vector without deriving the full Hessian by approximating
  the gradient (see the reference). If the function allows for the
  complex-step method, the gradient can be calculated much more
  accurate (see \code{grad_csd}).

  \code{hessdiag} computes only the diagonal of the Hessian by
  applying the central difference formula of second order to
  approximate the partial derivatives.
}
\value{
  \code{hessvec} returns the product \code{H(f,x) * v} as a vector.

  \code{hessdiag} returns the diagonal of the Hessian of \code{f}.
}
\references{
  B.A. Pearlmutter, Fast Exact Multiplication by the Hessian, Neural Computation (1994), Vol. 6, Issue 1, pp. 147-160.
}
\seealso{
  \code{\link{hessian}}
}
\examples{
  \dontrun{
    set.seed(1237); n <- 100
    a <- runif(n); b <- rnorm(n)
    fn <- function(x, a, b) sum(exp(-a*x)*sin(b*pi*x))
    x0 <- rep(1, n)
    v0 <- rexp(n, rate=0.1)
    
    # compute with full hessian
    h0 <- hessian(fn, x0, a = a, b = b)             # n=100 runtimes
    v1 <- c(h0 \%*\% v0)                              # 0.167   sec
    
    v2 <- hessvec(fn, x0, v0, a = a, b = b)         # 0.00209 sec
    v3 <- hessvec(fn, x0, v0, csd=TRUE,a=a, b=b)    # 0.00145 sec
    v4 <- hessdiag(fn, x0, a = a, b = b) * v0       # 0.00204 sec
    
    # compare with exact analytical Hessian
    hex <- diag((a^2-b^2*pi^2)*exp(-a*x0)*sin(b*pi*x0) - 
                 2*a*b*pi*exp(-a*x0)*cos(b*pi*x0))
    vex <- c(hex \%*\% v0)

    max(abs(vex - v1))          # 2.48e-05
    max(abs(vex - v2))          # 7.15e-05
    max(abs(vex - v3))          # 0.09e-05
    max(abs(vex - v4))          # 2.46e-05 }
}
\keyword{ math }
