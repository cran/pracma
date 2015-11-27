\name{std_err}
\alias{std_err}
\title{Standard Error}
\description{
  Standard error of the values of \code{x}.
}
\usage{
std_err(x)
}
\arguments{
  \item{x}{numeric vector or matrix}
}
\details{
  Standard error is computed as \code{var(x)/length(x)}.
}
\value{
  Returns the standard error of all elements of the vector or matrix.
}
\examples{
std_err(1:10)  #=> 0.9574271
}
\keyword{ stat }

