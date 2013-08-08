\name{nelder_mead}
\alias{nelder_mead}
\title{
  Nelder-Mead Minimization Method
}
\description{
  An implementation of the Nelder-Mead algorithm for derivative-free
  optimization.
}
\usage{
nelder_mead(x0, f, maxiter = 1000, scale = 1,
            tol = .Machine$double.eps^(2/3), show = FALSE, ...)
}
\arguments{
  \item{x0}{starting vector.}
  \item{f}{nonlinear function to be minimized.}
  \item{maxiter}{max. number of iterations.}
  \item{scale}{scale factor, of -1 the maximum will be searched for.}
  \item{tol}{relative tolerance, to be used as stopping rule.}
  \item{show}{logical, whether to store intermediate information.}
  \item{\ldots}{additional arguments to be passed to the function.}
}
\details{
  Also called a `simplex' method for finding the local minimum of a function
  of several variables. The method is a pattern search that compares function
  values at the vertices of the simplex. The process generates a sequence of
  simplices with ever reducing sizes.
}
\value{
  List with following components:
    \item{xmin}{minimum solution found.}
    \item{fmin}{value of \code{f} at minimum.}
    \item{niter}{number of iterations performed.}
}
\references{
  J. H. Mathews and K. D. Fink (2006). Numerical Methods Using Matlab.
  Fourth Edition, Pearson Education.
}
\note{
  Used Matlab code as provided in the textbook by Mathews and Fink.

  For a more elaborate implementation of Nelder-Mead see the package
  `dfoptim'.
}
\seealso{
  \code{\link{hooke_jeeves}}
}
\examples{
##  Rosenbrock function
rosenbrock <- function(x) {
    n <- length(x)
    x1 <- x[2:n]
    x2 <- x[1:(n-1)]
    sum(100*(x1-x2^2)^2 + (1-x2)^2)
}
nelder_mead(c(0, 0), rosenbrock)
# $xmin
# [1] 0.9999969 0.9999936
# $fmin
# [1] 1.131857e-11
# $niter
# [1] 49
}
\keyword{ optimize }
