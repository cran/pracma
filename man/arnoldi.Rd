\name{arnoldi}
\alias{arnoldi}
\title{Arnoldi Iteration}
\description{
  Arnoldi iteration generates an orthonormal basis of the Krylov space
  and a Hessenberg matrix.
}
\usage{
arnoldi(A, q, m)
}
\arguments{
  \item{A}{a square n-by-n matrix.}
  \item{q}{a vector of length n.}
  \item{m}{an integer.}
}
\details{
  \code{arnoldi(A, q, m)} carries out \code{m} iterations of the
  Arnoldi iteration with n-by-n matrix \code{A} and starting vector
  \code{q} (which need not have unit 2-norm). For \code{m < n} it 
  produces an n-by-(m+1) matrix \code{Q} with orthonormal columns
  and an (m+1)-by-m upper Hessenberg matrix \code{H} such that
  \code{A*Q[,1:m] = Q[,1:m]*H[1:m,1:m] + H[m+1,m]*Q[,m+1]*t(E_m)},
  where \code{E_m} is the m-th column of the m-by-m identity matrix.
}
\value{
Returns a list with two elements:

\code{Q} A matrix of orthonormal columns that generate the Krylov
space \code{(A, A q, A^2 q, ...)}.

\code{H} A Hessenberg matrix such that \code{A = Q * H * t(Q)}.
}
\references{
  Nicholas J. Higham (2008). Functions of Matrices: Theory and 
  Computation, SIAM, Philadelphia.
}
\seealso{
  \code{\link{hessenberg}}
}
\examples{
A <- matrix(c(-149,   -50,  -154,
               537,   180,   546,
               -27,    -9,   -25), nrow = 3, byrow = TRUE)
a <- arnoldi(A, c(1,0,0))
a
## $Q
##      [,1]       [,2]       [,3]
## [1,]    1  0.0000000  0.0000000
## [2,]    0  0.9987384 -0.0502159
## [3,]    0 -0.0502159 -0.9987384
## 
## $H
##           [,1]         [,2]        [,3]
## [1,] -149.0000 -42.20367124  156.316506
## [2,]  537.6783 152.55114875 -554.927153
## [3,]    0.0000   0.07284727    2.448851

a$Q \%*\% a$H \%*\% t(a$Q)
##      [,1] [,2] [,3]
## [1,] -149  -50 -154
## [2,]  537  180  546
## [3,]  -27   -9  -25
}
\keyword{ math }
