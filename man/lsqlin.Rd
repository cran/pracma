\name{lsqlin}
\alias{lsqlin}
\title{
  Linear Least-Squares Fitting
}
\description{
  Solves linearly constrained linear least-squares problems.
}
\usage{
  lsqlin(A, b, C, d, tol = 1e-13)
}
\arguments{
  \item{A}{\code{nxm}-matrix defining the least-squares problem.}
  \item{b}{vector or colum matrix with \code{n} rows; when it has more than 
           one column it describes several least-squares problems.}
  \item{C}{\code{pxm}-matrix for the constraint system.}
  \item{d}{vector or \code{px1}-matrix, right hand side for the constraints.}
  \item{tol}{tolerance to be passed to \code{pinv}.}
}
\details{
  \code{lsqlin(A, b, C, d)} minimizes \code{||A*x - b||} (i.e., in the 
  least-squares sense) subject to \code{C*x = d}.
}
\value{
  Returns a least-squares solution as column vector, or a matrix of solutions 
  in the columns if \code{b} is a matrix with several columns.
}
\note{
  The Matlab function \code{lsqlin} solves a more general problem, allowing
  additional linear inequalities and bound constraints. In \code{pracma} this
  task is solved applying function \code{lsqlincon}.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra.
  SIAM, Society for Industrial and Applied Mathematics, Philadelphia.
}
\seealso{
  \code{\link{nullspace}}, \code{\link{pinv}}, \code{\link{lsqlincon}}
}
\examples{
A <- matrix(c(
    0.8147,    0.1576,    0.6557,
    0.9058,    0.9706,    0.0357,
    0.1270,    0.9572,    0.8491,
    0.9134,    0.4854,    0.9340,
    0.6324,    0.8003,    0.6787,
    0.0975,    0.1419,    0.7577,
    0.2785,    0.4218,    0.7431,
    0.5469,    0.9157,    0.3922,
    0.9575,    0.7922,    0.6555,
    0.9649,    0.9595,    0.1712), 10, 3, byrow = TRUE)
b <- matrix(c(
    0.7060,    0.4387,
    0.0318,    0.3816,
    0.2769,    0.7655,
    0.0462,    0.7952,
    0.0971,    0.1869,
    0.8235,    0.4898,
    0.6948,    0.4456,
    0.3171,    0.6463,
    0.9502,    0.7094,
    0.0344,    0.7547), 10, 2, byrow = TRUE)
C <- matrix(c(
    1.0000,    1.0000,    1.0000,
    1.0000,   -1.0000,    0.5000), 2, 3, byrow = TRUE)
d <- as.matrix(c(1, 0.5))

# With a full rank constraint system
(L <- lsqlin(A, b, C, d))
#  0.10326838 0.3740381
#  0.03442279 0.1246794
#  0.86230882 0.5012825
C \%*\% L
#  1.0  1.0
#  0.5  0.5

\dontrun{
# With a rank deficient constraint system
C <- str2num('[1 1 1;1 1 1]')
d <- str2num('[1;1]')
(L <- lsqlin(A, b[, 1], C, d))
#  0.2583340
# -0.1464215
#  0.8880875
C \%*\% L         # 1 1  as column vector

# Where both A and C are rank deficient
A2 <- repmat(A[, 1:2], 1, 2)
C <- ones(2, 4) # d as above
(L <- lsqlin(A2, b[, 2], C, d))
#  0.2244121
#  0.2755879
#  0.2244121
#  0.2755879
C \%*\% L         # 1 1  as column vector}
}
\keyword{ fitting }
