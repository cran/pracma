\name{hessenberg}
\alias{hessenberg}
\title{
  Hessenberg Matrix
}
\description{
  Generates the Hessenberg matrix for A.
}
\usage{
hessenberg(A)
}
\arguments{
  \item{A}{square matrix}
}
\details{
  An (upper) Hessenberg matrix has zero entries below the first
  subdiagonal. 

  The function generates a Hessenberg matrix \code{H} and a unitary 
  matrix \code{P} (a similarity transformation) such that
  \code{A = P * H * t(P)}.

  The Hessenberg matrix has the same eigenvalues. If \code{A} is 
  symmetric, its Hessenberg form will be a tridiagonal matrix.
}
\value{
  Returns a list with two elements,
    \item{H}{the upper Hessenberg Form of matrix A.}
    \item{H}{a unitary matrix.}
}
\references{
  Press, Teukolsky, Vetterling, and Flannery (2007).
  Numerical Recipes: The Art of Scientific Computing. 3rd Edition,
  Cambridge University Press. (Section 11.6.2)
}
\seealso{
  \code{\link{householder}}
}
\examples{
A <- matrix(c(-149,   -50,  -154,
               537,   180,   546,
               -27,    -9,   -25), nrow = 3, byrow = TRUE)
hb  <- hessenberg(A)
hb
## $H
##           [,1]         [,2]        [,3]
## [1,] -149.0000  42.20367124 -156.316506
## [2,] -537.6783 152.55114875 -554.927153
## [3,]    0.0000   0.07284727    2.448851
## 
## $P
##      [,1]       [,2]      [,3]
## [1,]    1  0.0000000 0.0000000
## [2,]    0 -0.9987384 0.0502159
## [3,]    0  0.0502159 0.9987384

hb$P \%*\% hb$H \%*\% t(hb$P)
##      [,1] [,2] [,3]
## [1,] -149  -50 -154
## [2,]  537  180  546
## [3,]  -27   -9  -25
}
\keyword{ array }
