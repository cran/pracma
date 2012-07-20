\name{transfinite}
\alias{transfinite}
\title{
  Boxed Region Transformation
}
\description{
  Transformation of a box/bound constrained region to an unconstrained one.
}
\usage{
transfinite(lower, upper, n = length(lower))
}
\arguments{
  \item{lower, upper}{lower and upper box/bound constraints.}
  \item{n}{length of upper, lower if both are scalars, to which they get
           repeated.}
}
\details{
  Transforms a constraint region in n-dimensional space bijectively to the
  unconstrained \eqn{R^n} space, applying a \code{atanh} resp. \code{exp}
  transformation to each single variable that is bound constraint.

  It provides two functions, \code{g: []x...x[] --> R^n} and its inverse
  \code{ginv}. These functions can, for example, be used to add box/bound
  constraints to a constrained optimization problem that is to be solved with
  a (nonlinear) solver not allowing constraints.
}
\value{
  Returns to functions as components \code{g} and \code{ginv} of a list. 
}
\note{
  Based on an idea of Ravi Varadhan, intrinsically used in his implementation
  of Nelder-Mead in the `dfoptim' package.

  For positivity constraints, \code{x>=0}, this approach is considered to be
  numerically more stable than \code{x-->exp(x)} or \code{x-->x^2}.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\examples{
lower <- c(-Inf, 0,   0)
upper <- c( Inf, 0.5, 1)
Tf <- transfinite(lower, upper)
g <- Tf$g; h <- Tf$ginv

##  Solve Rosenbrock with one variable restricted
rosen <- function(x) {
    n <- length(x)
    x1 <- x[2:n]; x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}
f  <- function(x) rosen(h(x))   # f must be defined on all of R^n
x0 <- c(0.1, 0.1, 0.1)          # starting point not on the boundary!
nm <- nelder_mead(g(x0), f)     # unconstraint Nelder-Mead
h(nm$xmin); nm$fmin             # box/bound constraint solution
# [1] 0.7085596 0.5000000 0.2500004
# [1] 0.3353605

}
\keyword{ manip }
