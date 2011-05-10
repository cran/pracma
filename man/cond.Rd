\name{cond}
\alias{cond}
\title{
  Matrix Condition
}
\description{
  Condition number of a matrix.
}
\usage{
cond(M, p = 2)
}
\arguments{
  \item{M}{Numeric matrix; vectors will be considered as column vectors.}
  \item{p}{Indicates the \code{p}-norm.
           At the moment, norms other than \code{p=2} are not implemented.}
}
\details{
  The condition number of a matrix measures the sensitivity of the solution
  of a system of linear equations to small errors in the data. Values of
  \code{cond(M)} and \code{cond(M, p)} near \code{1} are indications of a
  well-conditioned matrix.
}
\value{
  \code{cond(M)} returns the 2-norm condition number, the ratio of the
  largest singular value of \code{M} to the smallest.

  \code{c = cond(M, p)} returns the matrix condition number in \code{p}-norm:

  \code{norm(X,p) * norm(inv(X),p)}.

  (Not yet implemented.)
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Philadelphia.   
}
\note{
  Not feasible for large or sparse matrices as \code{svd(M)} needs to be
  computed. The Matlab/Octave function \code{condest} for condition
  estimation has not been implemented.
}
\seealso{
  \code{\link{normest}}, \code{\link{svd}}
}
\examples{
cond(hilb(8))
}
\keyword{ array }
