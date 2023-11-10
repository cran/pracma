\name{linearproj, affineproj}
\alias{linearproj}
\alias{affineproj}
\title{
  Linear Projection onto a Subspace
}
\description{
  Computes the projection of points in the columns of B onto the
  linear subspace spaned by the columns of A, resp. the projection
  of a point onto an affine subspace and its distance.
}
\usage{
  linearproj(A, B)

  affineproj(x0, C, b, unbound = TRUE, maxniter = 100)
}
\arguments{
  \item{A}{Matrix whose columns span a subspace of some R^n.}
  \item{B}{Matrix whose columns are to be projected.}
  \item{x0}{Point in R^n to be projected onto C x = b.}
  \item{C, b}{Matrix and vector, defining an affine subspace as C x = b}
  \item{unbound}{Logical; require all x >= 0 if unbound is false.}
  \item{maxniter}{Maximum number of iterations (if  is unbound is false).}
}
\details{
  \code{linearproj} projects points onto a \emph{linear} subspace in R^n.
  The columns of A are assumed be the basis of a linear subspace, esp.
  they are required to be linearly independent. The columns of matrix B
  define points in R^n that will be projected onto A, and their resp.
  coefficients in terms of the basis in A are computed.

  The columns of A need to be linearly independent; if not, generate an 
  orthonormal basis of this subspace with \code{orth(A)}. If you want to 
  project points onto a subspace that is defined by \code{A x = 0}, then 
  generate an orthonormal basis of the nullspace of A with \code{null(A)}.

  Technically, the orthogonal projection can be determined by a finite 
  'Fourier expansion' with coefficients calculated as scalar products,
  see the examples.

  \code{affineproj} projects (single) points onto an affine subspace 
  defined by \code{A x = b} and calculates the distance of \code{x0} from 
  this subspace. The calculation is based on the following formula:
  \deqn{p = (I - A' (A A')^{-1}) x0 + A' (A A')^{-1} b}

  Technically, if \code{a} is one solution of \code{C x = b}, then the 
  projection onto C can be derived from the projection onto
  \code{S = {C x = 0}} with \code{proj_C(x) = a + proj_S(x - a)},
  see the examples.

  In case the user requests the coordinates of the projected point to be 
  positive, an iteration procedure is started where negative coordinates 
  are set to zero in each iteration.
}
\value{
  The functions \code{linearproj} returns a list with components P and Q. 
  The columns of P contain the coefficients -- in the basis of A -- of the 
  corresponding projected points in B, and the columns of Q are the the 
  coordinates of these points in the natural coordinate system of R^n.

  \code{affineproj} returns a list with components \code{proj}, \code{dist}, 
  and \code{niter}. \code{proj} is the projected point, \code{dist} the 
  distance from the subspace (and \code{niter} the number of iterations 
  if positivity of the coordinates was requested.).
}
\references{
  G. Strang (2006). Linear Algebra and Its Applications. Fourth Edition,
  Cengage Learning, Boston, MA.
}
\note{
  Some timings show that these implementations are to a certain extent
  competitive with direct applications of quadprog.
}
\author{
  Hans W. Borchers, partly based on code snippets by Ravi Varadhan.
}
\seealso{
  \code{\link{nullspace}}, \code{\link{orth}}
}
\examples{
#-- Linear projection --------------------------------------------------

# Projection onto the line (1,1,1) in R^3
A <- matrix(c(1,1,1), 3, 1)
B <- matrix(c(1,0,0, 1,2,3, -1,0,1), 3, 3)
S <- linearproj(A, B)
## S$Q
##           [,1] [,2] [,3]
## [1,] 0.3333333    2    0
## [2,] 0.3333333    2    0
## [3,] 0.3333333    2    0

# Fourier expansion': sum(<x0, a_i> a_i /<a_i, a_i>), a_i = A[ ,i]
dot(c(1,2,3), A) * A / dot(A, A)    # A has only one column

#-- Affine projection --------------------------------------------------

# Projection onto the (hyper-)surface x+y+z = 1 in R^3
A <- t(A); b <- 1
x0 <- c(1,2,3)
affineproj(x0, A, b)            # (-2/3, 1/3, 4/3)

# Linear translation: Let S be the linear subspace and A the parallel
# affine subspace of A x = b, a the solution of the linear system, then
#   proj_A(x) = a + proj_S(x-a)
a <- qr.solve(A, b)
A0 <- nullspace(A)
xp <- c(a + linearproj(A0, x0 - a)$Q)
## [1] -0.6666667  0.3333333  1.3333333

#-- Projection with positivity ----------------------- 24 ms -- 1.3 s --
s <- affineproj(x0, A, b, unbound = FALSE)
zapsmall(s$proj)                 # [1] 0 0 1
## $x     : 0.000000e+00 3.833092e-17 1.000000e+00
## $niter : 35

#-- Extended Example ------------------------------------------ 80 ms --
\dontrun{
set.seed(65537)
n = 1000; m = 100                       # dimension, codimension
x0 <- rep(0, n)                         # project (0, ..., 0)
A <- matrix(runif(m*n), nrow = m)       # 100 x 1000
b <- rep(1, m)                          # A x = b, linear system
a <- qr.solve(A, b)                     # A a = b, LS solution
A0 <- nullspace(A)                      # 1000 x 900, base of <A>
xp <- a+drop(A0 \%*\% dot(x0-a, A0))      # projection
Norm(xp - x0)                           # [1] 0.06597077
}

#-- Solution with quadprog ------------------------------------ 40 ms --
# D <- diag(1, n)             # quadratic form
# A1 <- rbind(A, diag(1, n))  # A x = b and
# b1 <- c(b, rep(0, n))       #   x >= 0
# n <- nrow(A)
# sol = quadprog::solve.QP(D, x0, t(A1), b1, meq = n)
# xp <- sol$solution

#-- Solution with CVXR ---------------------------------------- 50 ms --
# library(CVXR)
# x = Variable(n)                             # n decision variables
# objective = Minimize(p_norm(x0 - x))        # min! || p0 - x ||
# constraint = list(A %*% x == b, x >= 0)     # A x = b, x >= 0
# problem = Problem(objective, constraint)
# solution = solve(problem)                   # Solver: ECOS
# solution$value                              # 
# xp <- solution$getValue(x)                  # 
}
\keyword{ math }
