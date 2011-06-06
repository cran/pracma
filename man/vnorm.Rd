\name{vnorm}
\alias{vnorm}
\title{
Vector Norm
}
\description{
  The \code{vnorm} function calculates several different types of vector
  norms for \code{x}, depending on the argument \code{p}. 
}
\usage{
vnorm(x, p = 2)
}
\arguments{
  \item{x}{Numeric vector; matrices not allowed.}
  \item{p}{Numeric scalar or Inf, -Inf; default is 2}
}
\details{
  \code{vnorm} returns a scalar that gives some measure of the magnitude
  of the elements of \code{x}. It is called the \eqn{p}-norm for values
  \eqn{-Inf \le p \le Inf}, defining Hilbert spaces on \eqn{R^n}.

  \code{vnorm(x)} is the Euclidean length of a vecor \code{x}; same as
  \code{vnorm(x, 2)}.\cr
  \code{vnorm(x, p)} for finite p is defined as \code{sum(abs(A)^p)^(1/p)}.\cr
  \code{vnorm(x, Inf)} returns \code{max(abs(x))},
  while \code{vnorm(x, -Inf)} returns \code{min(abs(x))}.
}
\value{
  Numeric scalar (or \code{Inf}), or \code{NA} if an element of \code{x}
  is \code{NA}.
}
\note{
  In Matlab/Octave this is called \code{norm}; R's \code{norm} function
  \code{norm(x, "F")} (`Frobenius Norm') is the same as \code{vnorm(x)}.
}
\seealso{
  \code{\link{norm}} of a matrix
}
\examples{
vnorm(c(3, 4))          #=> 5  Pythagoras triple
vnorm(c(1, 1, 1), p=2)  #   sqrt(3)
vnorm(1:10, p = 1)      #   sum(1:10)
vnorm(1:10, p = 0)      #   Inf
vnorm(1:10, p = Inf)    #   max(1:10)
vnorm(1:10, p = -Inf)   #   min(1:10)
}
\keyword{ array }
