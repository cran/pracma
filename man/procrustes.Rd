\name{procrustes}
\alias{procrustes}
\alias{kabsch}
\title{
  Solving the Procrustes Problem
}
\description{
  \code{procrustes} solves for two matrices \code{A} and \code{B} the 
  `Procrustes Problem' of finding an orthogonal matrix \code{Q} such that
  \code{A-B*Q} has the minimal Frobenius norm.

  \code{kabsch} determines a best rotation of a given vector set into a
  second vector set by minimizing the weighted sum of squared deviations.
  The order of vectors is assumed fixed.
}
\usage{
procrustes(A, B)

kabsch(A, B, w = NULL)
}
\arguments{
  \item{A, B}{two numeric matrices of the same size.}
  \item{w}{weights , influence the distance of points}
}
\details{
  The function \code{procrustes(A,B)} uses the \code{svd} decomposition 
  to find an orthogonal matrix \code{Q} such that \code{A-B*Q} has a 
  minimal Frobenius norm, where this norm for a matrix \code{C} is defined
  as \code{sqrt(Trace(t(C)*C))}, or \code{norm(C,'F')} in R.

  Solving it with \code{B=I} means finding a nearest orthogonal matrix.

  \code{kabsch} solves a similar problem and uses the Procrustes procedure
  for its purpose. Given two sets of points, represented as columns of the
  matrices \code{A} and \code{B}, it determines an orthogonal matrix
  \code{U} and a translation vector \code{R} such that \code{U*A+R-B} 
  is minimal.
}
\value{
  \code{procrustes} returns a list with components \code{P}, which is 
  \code{B*Q}, then \code{Q}, the orthogonal matrix, and \code{d}, the
  Frobenius norm of \code{A-B*Q}.

  \code{kabsch} returns a list with \code{U} the orthogonal matrix applied, 
  \code{R} the translation vector, and \code{d} the least root mean square
  between \code{U*A+R} and \code{B}.
}
\note{
  The \code{kabsch} function does not take into account scaling of the sets,
  but this could easily be integrated.
}
\references{
  Golub, G. H., and Ch. F. van Loan (1996). Matrix Computations. 3rd Edition,
  The John Hopkins University Press, Baltimore London. [Sect. 12.4, p. 601]

  Kabsch, W. (1976). A solution for the best rotation to relate two sets 
  of vectors. Acta Cryst A, Vol. 32, p. 9223.
}
\seealso{
  \code{\link{svd}}
}
\examples{
##  Procrustes
U <- randortho(5)               # random orthogonal matrix
P <- procrustes(U, eye(5))

##  Kabsch
P <- matrix(c(0, 1, 0, 0, 1, 1, 0, 1,
              0, 0, 1, 0, 1, 0, 1, 1,
              0, 0, 0, 1, 0, 1, 1, 1), nrow = 3, ncol = 8, byrow = TRUE)
R <- c(1, 1, 1)
phi <- pi/4
U <- matrix(c(1, 0, 0,
              0, cos(phi), -sin(phi),
              0, sin(phi),  cos(phi)), nrow = 3, ncol = 3, byrow = TRUE)

Q <- U \%*\% P + R
K <- kabsch(P, Q)
# K$R == R  and  K$U %*% P + c(K$R) == Q
}
\keyword{ array }
