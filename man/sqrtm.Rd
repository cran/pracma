\name{sqrtm,rootm}
\alias{sqrtm}
\alias{signm}
\alias{rootm}
\title{
  Matrix Square and p-th Roots
}
\description{
  Computes the matrix square root and matrix p-th root of a nonsingular
  real matrix.
}
\usage{
sqrtm(A, kmax = 20, tol = .Machine$double.eps^(1/2))
signm(A, kmax = 20, tol = .Machine$double.eps^(1/2))

rootm(A, p, kmax = 20, tol = .Machine$double.eps^(1/2))
}
\arguments{
\item{A}{numeric, i.e. real, matrix.}
\item{p}{p-th root to be taken.}
\item{kmax}{maximum number of iterations.}
\item{tol}{absolut tolerance, norm distance of \code{A} and \code{B^p}.}
}
\details{
  A real matrix may or may not have a real square root; if it has no real
  negative eigenvalues. The number of square roots can vary from two to
  infinity. A positive definite matric has one distinguished square root,
  called the principal one, with the property that the eigenvalues lie in
  the segment
  \code{{z | -pi/p < arg(z) < pi/p}} (for the p-th root).

  The matrix square root \code{sqrtm(A)} is computed here through the
  Denman-Beavers iteration (see the references) with quadratic rate of
  convergence, a refinement of the common Newton iteration determining
  roots of a quadratic equation.

  The matrix p-th root \code{rootm(A)} is computed as a complex integral
  \deqn{ A^{1/p} = \frac{p \sin(\pi/p)}{\pi} A \int_0^{\infty} (x^p I + A)^{-1} dx}
  applying the trapezoidal rule along the unit circle.

  One application is the computation of the matrix logarithm as
  \deqn{\log A = 2^k log A^{1/2^k}}
  such that the argument to the logarithm is close to the identity matrix
  and the Pade approximation can be applied to \eqn{\log(I + X)}.

  The matrix sector function is defined as \code{sectm(A,m)=(A^m)^(-1/p)\%*\%A};
  for \code{p=2} this is the matrix sign function.

  \code{S=signm(A)} is real if A is and has the following properties:\cr
  \code{S^2=Id; S A = A S}\cr
  \code{singm([0 A; B 0])=[0 C; C^-1 0]} where \code{C=A(BA)^-1}

  These functions arise in control theory.
}
\value{
  \code{sqrtm(A)} returns a list with components

  \item{ B }{square root matrix of \code{A}.}
  \item{ Binv }{inverse of the square root matrix.}
  \item{ k }{number of iterations.}
  \item{ acc }{accuracy or absolute error.}

  \code{rootm(A)} returns a list with components

  \item{ B }{square root matrix of \code{A}.}
  \item{ k }{number of iterations.}
  \item{ acc }{accuracy or absolute error.}

  If \code{k} is negative the iteration has \emph{not} converged.

  \code{signm} just returns one matrix, even when there was no convergence.
}
\note{
  The p-th root of a positive definite matrix can also be computed from
  its eigenvalues as

  \code{E <- eigen(A)}\cr
  \code{V <- E\$vectors; U <- solve(V)}\cr
  \code{D <- diag(E\$values)}\cr
  \code{B <- V \%*\% D^(1/p) \%*\% U}

  or by applying the functions \code{expm}, \code{logm} in package `expm':

  \code{B <- expm(1/p * logm(A))}

  As these approaches all calculate the principal branch, the results are
  identical (but will numerically slightly differ).
}
\references{
  N. J. Higham (1997). Stable Iterations for the Matrix Square Root.
  Numerical Algorithms, Vol. 15, pp. 227--242.

  D. A. Bini, N. J. Higham, and B. Meini (2005). Algorithms for the
  matrix pth root. Numerical Algorithms, Vol. 39, pp. 349--378.
}
\seealso{
  \code{\link{expm}}, \code{expm::sqrtm}
}
\examples{
A1 <- matrix(c(10,  7,  8,  7,
                7,  5,  6,  5,
                8,  6, 10,  9,
                7,  5,  9, 10), nrow = 4, ncol = 4, byrow = TRUE)

X <- sqrtm(A1)$B    # accuracy: 2.352583e-13
X %*% X             # A1

A2 <- matrix(c(90.81, 8.33, 0.68, 0.06, 0.08, 0.02, 0.01, 0.01,
                0.70, 90.65, 7.79, 0.64, 0.06, 0.13, 0.02, 0.01,
                0.09, 2.27, 91.05, 5.52, 0.74, 0.26, 0.01, 0.06,
                0.02, 0.33, 5.95, 85.93, 5.30, 1.17, 1.12, 0.18,
                0.03, 0.14, 0.67, 7.73, 80.53, 8.84, 1.00, 1.06,
                0.01, 0.11, 0.24, 0.43, 6.48, 83.46, 4.07, 5.20,
                0.21, 0, 0.22, 1.30, 2.38, 11.24, 64.86, 19.79,
                0, 0, 0, 0, 0, 0, 0, 100
              ) / 100, nrow = 8, ncol = 8, byrow = TRUE)

X <- rootm(A2, 12)  # k = 6, accuracy: 2.208596e-14

##  Matrix sign function
signm(A1)                               # 4x4 identity matrix
B <- rbind(cbind(zeros(4,4), A1),
           cbind(eye(4), zeros(4,4)))
signm(B)                                # [0, signm(A1)$B; signm(A1)$Binv 0]
}
\keyword{ math }
