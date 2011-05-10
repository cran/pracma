\name{mrank}
\alias{mrank}
\title{
  Matrix Rank
}
\description{
  Provides an estimate of the rank of a matrix \code{M}.
}
\usage{
mrank(M)
}
\arguments{
  \item{M}{Numeric matrix; vectors will be considered as column vectors.}
}
\details{
  Provides an estimate of the number of linearly independent rows or columns
  of a matrix \code{M}. Compares an approach using QR-decomposition with one
  counting singular values larger than a certain tolerance (Matlab).
}
\value{
  Matrix rank as integer between \code{0} and \code{min(ncol(M), nrow(M))}.
}
\references{
  Trefethen, L. N., and D. Bau III. (1997). Numerical Linear Algebra. SIAM,
  Philadelphia. 
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\note{
  The corresponding function in Matlab is called \code{rank}, but that term
  has a different meaning in R.
}
\seealso{
  \code{\link{nullspace}}
}
\examples{
mrank(magic(10))   #=> 7
mrank(magic(100))  #=> 3 (!)
mrank(hilb(8))     #=> 8 , but qr(hilb(8))$rank says, rank is 7.
# Warning message:
# In mrank(hilb(8)) : Rank calculation may be problematic.
}
\keyword{ array }
