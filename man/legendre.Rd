\name{legendre}
\alias{legendre}
\title{
  Legendre Functions (Matlab Style)
}
\description{
  Calculate the values of (associated) Legendre functions.
}
\usage{
legendre(n, x)
}
\arguments{
  \item{n}{degree of the Legendre polynomial involved.}
  \item{x}{real points to evaluate Legendre's functions at.}
}
\details{
  \code{legendre(n,x)} computes the associated Legendre functions of degree
  \code{n} and order \code{m=0,1,...,n}, evaluated for each element of
  \code{x} where \code{x} must contain real values in \code{[-1,1]}.

  If \code{x} is a vector, then \code{L=legendre(n,x)} is an
  \code{(n+1)}-by-\code{N} matrix, where \code{N=length(x)}. Each element 
  \code{L[m+1,i]} corresponds to the associated Legendre function of degree
  \code{legendre(n,x)} and order \code{m} evaluated at \code{x[i]}.

  Note that the first row of \code{L} is the Legendre polynomial evaluated at
  \code{x}.
}
\value{
  Returns a matrix of size \code{(n+1)}-by-\code{N} where \code{N=length(x)}.
}
\note{
  Legendre functions are solutions to Legendre's differential equation (it
  occurs when solving Laplace's equation in spherical coordinates).
}
\seealso{
  \code{\link{chebPoly}}
}
\examples{
x <- c(0.0, 0.1, 0.2)
legendre(2, x)
#      [,1]       [,2]       [,3]
# [1,] -0.5 -0.4850000 -0.4400000
# [2,]  0.0 -0.2984962 -0.5878775
# [3,]  3.0  2.9700000  2.8800000

\dontrun{
x <- seq(0, 1, len = 50)
L <- legendre(2, x)
plot(x, L[1, ], type = "l", col = 1, ylim = c(-2, 3), ylab = "y",
                main = "Legendre Functions of degree 2")
lines(x, L[2, ], col = 2)
lines(x, L[3, ], col = 3)
grid()}
## Generate Legendre's Polynomial as function
# legendre_P <- function(n, x) {
#     L <- legendre(n, x)
#     return(L[1, ])
# }
}
\keyword{ math }
