\name{itersolve}
\alias{itersolve}
\title{
  Iterative Methods
}
\description{
  Iterative solutions of systems of linear equations.
}
\usage{
itersolve(A, b, x0 = NULL, nmax = 1000, tol = .Machine$double.eps^(0.5),
            method = c("Gauss-Seidel", "Jacobi", "Richardson"))
}
\arguments{
\item{A}{numerical matrix, square and non-singular.}
\item{b}{numerical vector or column vector.}
\item{x0}{starting solution for iteration; defaults to null vector.}
\item{nmax}{maximum number of iterations.}
\item{tol}{relative tolerance.}
\item{method}{iterative method, Gauss-Seidel, Jacobi, or Richardson.}
}
\details{
  Iterative methods are based on splitting the matrix \code{A=(P-A)-A}
  with a so-called `preconditioner' matrix P. The methods differ in how
  to choose this preconditioner.
}
\value{
  Returns a list with components \code{x} the solution, \code{iter} the
  number of iterations, and \code{method} the name of the method applied.
}
\references{
  Quarteroni, A., and F. Saleri (2006). Scientific Computing with MATLAB
  and Octave. Springer-Verlag, Berlin Heidelberg.
}
\note{
  Richardson's method allows to specify a `preconditioner'; this has not
  been implemented yet.
}
\seealso{
  \code{\link{qrSolve}}
}
\examples{
N <- 10
A <- Diag(rep(3,N)) + Diag(rep(-2, N-1), k=-1) + Diag(rep(-1, N-1), k=1)
b <- A \%*\% rep(1, N)
x0 <- rep(0, N)

itersolve(A, b, tol = 1e-8, method = "Gauss-Seidel")
# [1]  1  1  1  1  1  1  1  1  1  1
# [1]  87
itersolve(A, b, x0 = 1:10, tol = 1e-8, method = "Jacobi")
# [1]  1  1  1  1  1  1  1  1  1  1
# [1]  177
}
\keyword{ array }
