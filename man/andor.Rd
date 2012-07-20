\name{And, Or}
\alias{And}
\alias{Or}
\title{
  Logical AND, OR (Matlab Style)
}
\description{
  \code{And(l, k)} resp. \code{Or(l, k)} the same as \code{(l & k) + 0} resp.
  \code{(l | k) + 0}.
}
\usage{
And(l, k)
Or(l, k)
}
\arguments{
  \item{l, k}{Arrays.}
}
\details{
  Performs a logical operation of arrays \code{l} and \code{k} and returns an
  array containing elements set to either 1 (\code{TRUE}) or 0 (\code{FALSE}),
  that is in Matlab style.
}
\value{
  Logical vector.
}
\examples{
A <- matrix(c(0.5,  0.5,  0,    0.75, 0,
              0.5,  0,    0.75, 0.05, 0.85,
              0.35, 0,    0,    0,    0.01,
              0.5,  0.65, 0.65, 0.05, 0), 4, 5, byrow=TRUE)
B <- matrix(c( 0, 1, 0, 1, 0,
               1, 1, 1, 0, 1,
               0, 1, 1, 1, 0,
               0, 1, 0, 0, 1), 4, 5, byrow=TRUE)

And(A, B)
Or(A, B)
}
\keyword{ logic }
