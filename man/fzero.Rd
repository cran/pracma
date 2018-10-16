\name{fzero}
\alias{fzero}
\title{
  Root Finding Algorithm
}
\description{
  Find root of continuous function of one variable.
}
\usage{
fzero(fun, x, maxiter = 500, tol = 1e-12, ...)
}
\arguments{
  \item{fun}{function whose root is sought.}
  \item{x}{a point near the root or an interval giving end points.}
  \item{maxiter}{maximum number of iterations.}
  \item{tol}{relative tolerance.}
  \item{...}{additional arguments to be passed to the function.}
}
\details{
  \code{fzero} tries to find a zero of \code{f} near \code{x}, if \code{x}
  is a scalar. Expands the interval until different signs are found at the
  endpoints or the maximum number of iterations is exceeded.
  If \code{x} is a vector of length two, \code{fzero} assumes \code{x} is
  an interval where the sign of \code{x[1]} differs from the sign of
  \code{x[1]}. An error occurs if this is not the case.

  ``This is essentially the ACM algorithm 748. The structure of the algorithm
  has been transformed non-trivially: it implement here a FSM version using
  one interior point determination and one bracketing per iteration, thus
  reducing the number of temporary variables and simplifying the structure.''

  This approach will not find zeroes of quadratic order.
}
\value{
  \code{fzero} returns a list with
    \item{x}{location of the root.}
    \item{fval}{function value at the root.}
}
\references{
  Alefeld, Potra and Shi (1995). Enclosing Zeros of Continuous Functions.
  ACM Transactions on Mathematical Software, Vol. 21, No. 3.
}
\note{
  \code{fzero} mimics the Matlab function of the same name, but is translated
  from Octave's \code{fzero} function, copyrighted (c) 2009 by Jaroslav Hajek.
}
\seealso{
  \code{\link{uniroot}}, \code{\link{brent}}
}
\examples{
fzero(sin, 3)                    # 3.141593
fzero(cos,c(1, 2))               # 1.570796
fzero(function(x) x^3-2*x-5, 2)  # 2.094551
}
\keyword{ math }
