\name{isempty}
\alias{isempty}
\title{isempty Property}
\description{
  Determine if an object is empty.
}
\usage{
isempty(x)
}
\arguments{
  \item{x}{an R object}
}
\details{
  An empty object has length zero.
}
\value{
  \code{TRUE} if \code{x} has length 0; otherwise, \code{FALSE}.
}
\examples{
isempty(c(0))            # FALSE
isempty(matrix(0, 1, 0)) # TRUE
}
\keyword{logic}

