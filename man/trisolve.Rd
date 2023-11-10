\name{trisolve}
\alias{trisolve}
\title{
  Tridiagonal Linear System Solver
}
\description{
  Solves tridiagonal linear systems \code{A*x=rhs} efficiently.
}
\usage{
trisolve(a, b, d, rhs)
}
\arguments{
  \item{a}{diagonal of the tridiagonal matrix \code{A}.}
  \item{b, d}{upper and lower secondary diagonal of \code{A}.}
  \item{rhs}{right hand side of the linear system \code{A*x=rhs}.}
}
\details{
  Solves tridiagonal linear systems \code{A*x=rhs} by applying Givens
  transformations.

  By only storing the three diagonals, \code{trisolve} has memory requirements 
  of \code{3*n} instead of \code{n^2} and
  is faster than the standard \code{solve} function for larger matrices.
}
\value{
  Returns the solution of the tridiagonal linear system as vector.
}
\note{
  Has applications for spline approximations and for solving boundary value
  problems (ordinary differential equations).
}
\references{
  Gander, W. (1992). Computermathematik. Birkhaeuser Verlag, Basel.
}
\seealso{
  \code{\link{qrSolve}}
}
\examples{
set.seed(8237)
a <- rep(1, 100)
e <- runif(99); f <- rnorm(99)
x <- rep(seq(0.1, 0.9, by = 0.2), times = 20)
A <- diag(100) + Diag(e, 1) + Diag(f, -1)
rhs <- A \%*\% x
s <- trisolve(a, e, f, rhs)
s[1:10]                         #=> 0.1 0.3 0.5 0.7 0.9 0.1 0.3 0.5 0.7 0.9
s[91:100]                       #=> 0.1 0.3 0.5 0.7 0.9 0.1 0.3 0.5 0.7 0.9
}
\keyword{ array }
