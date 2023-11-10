\name{lu}
\alias{lu}
\alias{lu_crout}
\alias{lufact}
\alias{lusys}
\title{
  LU Matrix Factorization
}
\description{
  LU decomposition of a positive definite matrix as Gaussian factorization.
}
\usage{
lu(A, scheme = c("kji", "jki", "ijk"))
lu_crout(A)

lufact(A)
lusys(A, b)
}
\arguments{
  \item{A}{square positive definite numeric matrix (will not be checked).}
  \item{scheme}{order of row and column operations.}
  \item{b}{right hand side of a linear system of equations.}
}
\details{
  For a given matrix \code{A}, the LU decomposition exists and is unique iff
  its principal submatrices of order \code{i=1,...,n-1} are nonsingular. The
  procedure here is a simple Gauss elimination with or without pivoting.

  The scheme abbreviations refer to the order in which the cycles of row- and
  column-oriented operations are processed. The ``ijk'' scheme is one of the
  two compact forms, here the Doolite factorization (the Crout factorization
  would be similar).

  \code{lu_crout} implements the Crout algorithm. For the Doolite algorithm,
  the \code{L} matrix has ones on its diagonal, for the Crout algorithm, the 
  diagonal of the \code{U} matrix only has ones.

  \code{lufact} applies partial pivoting (along the rows).
  \code{lusys} uses LU factorization to solve the linear system \code{A*x=b}.

  These function are not meant to process huge matrices or linear systems of
  equations.
  Without pivoting they may also be harmed by considerable inaccuracies.
}
\value{
   \code{lu} and \code{lu_crout} return a list with components \code{L}
   and \code{U}, the lower and upper triangular matrices such that
   \code{A=L\%*\%U}. 

  \code{lufact} returns a list with \code{L} and \code{U} combined into one
  matrix \code{LU}, the \code{rows} used in partial pivoting, and \code{det}
  representing the determinant of \code{A}. See the examples how to extract
  matrices \code{L} and \code{U} from \code{LU}.

  \code{lusys} returns the solution of the system as a column vector.
}
\references{
  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second edition, Springer-Verlag, Berlin Heidelberg.

  J.H. Mathews and K.D. Fink (2003). Numerical Methods Using MATLAB.
  Fourth Edition, Pearson (Prentice-Hall), updated 2006.
}
\note{
  To get the Crout decomposition of a matrix \code{A} do
  \code{Z <- lu(t(A)); L <- t(Z$U); U <- t(Z$L)}.
}
\seealso{
  \code{\link{qr}}
}
\examples{
A <- magic(5)
D <- lu(A, scheme = "ijk")     # Doolittle scheme
D$L \%*\% D$U
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   17   24    1    8   15
## [2,]   23    5    7   14   16
## [3,]    4    6   13   20   22
## [4,]   10   12   19   21    3
## [5,]   11   18   25    2    9

H4 <- hilb(4)
lufact(H4)$det
## [1] 0.0000001653439

x0 <- c(1.0, 4/3, 5/3, 2.0)
b  <- H4 \%*\% x0
lusys(H4, b)
##          [,1]
## [1,] 1.000000
## [2,] 1.333333
## [3,] 1.666667
## [4,] 2.000000
}

\keyword{ array }
