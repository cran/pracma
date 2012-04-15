\name{invperm}
\alias{invperm}
\title{
Inverse Permutation
}
\description{
Generates the inverse permutation.
}
\usage{
invperm(p)
}
\arguments{
  \item{p}{permutation, i.e. the vector \code{1:n} permuted.}
}
\details{
  Generates the inverse permutation.
}
\value{
  Vector of permuted elements of \code{1:length(p)}.
}
\seealso{
\code{\link{randperm}}
}
\examples{
p1 <- c(1,3,5,2,4)
p1inv <- invperm(p1)            # 1 4 2 5 3

# Define two more functions for permutations
compoperm <- function(p, q) q[p]        # composition of permutations
compoperm(p1, p1inv)                    # 1 2 3 4 5
applyperm <- function(p, x) x[p]        # apply permutation to vector
p2 <- c(2,1,3,5,4)
x  <- c(11,12,13,14,15)
applyperm(compoperm(p2, p1), x) == applyperm(p2, applyperm(p1, x))  # all TRUE
}
\keyword{ arith }
