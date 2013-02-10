\name{lu}
\alias{lu}
\title{
  LU Matrix Factorization
}
\description{
  LU decomposition of a positive definite matrix as Gaussian factorization
  (without pivoting).
}
\usage{
lu(A, scheme = c("kji", "jki", "ijk"))
}
\arguments{
  \item{A}{square positive definite numeric matrix (will not be checked).}
  \item{scheme}{order of row and column operations.}
}
\details{
  For a given matrix \code{A}, the LU decomposition exists and is unique iff
  its principal submatrices of order \code{i=1,...,n-1} are nonsingular. The
  procedure here is a simple Gauss elimination without pivoting.

  The scheme abbreviations refer to the order in which the cycles of row- and
  column-oriented operations are processed. The ``ijk'' scheme is one of the
  two compact forms, here the Doolite factorization (the Crout factorization
  would be similar).  
}
\value{
  Returns a list with components \code{L} and \code{U}, the two lower and
  upper triangular matrices such that \code{A=L\%*\%U}.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second edition, Springer-Verlag, Berlin Heidelberg.
}
\note{
  This function is not meant to process huge matrices or linear systems of
  equations.
  Without pivoting it may also be harmed by considerable inaccuracies.
}
\seealso{
  \code{\link{qr}}
}
\examples{
A <- magic(5)
LU <- lu(A, scheme = "ijk")     # Doolittle scheme
LU$L \%*\% LU$U
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   17   24    1    8   15
## [2,]   23    5    7   14   16
## [3,]    4    6   13   20   22
## [4,]   10   12   19   21    3
## [5,]   11   18   25    2    9
}

\keyword{ array }
