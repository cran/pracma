\name{ceil}
\alias{ceil}
\alias{Fix}
\title{Integer Functions (Matlab Style)}
\description{
  Functions for rounding and truncating numeric values towards near
  integer values.
}
\usage{
ceil(n)
Fix(n)
}
\arguments{
  \item{n}{a numeric vector or matrix}
}
\details{
  \code{ceil()} is an alias for \code{ceiling()} and rounds to the smallest
  integer equal to or above \code{n}.

  \code{Fix()} truncates values towards 0 and is an alias for 
  \code{trunc()}. Uses \code{ml} prefix to indicate Matlab style.

  The corresponding functions \code{floor()} (rounding to the largest interger
  equal to or smaller than \code{n}) and \code{round()} (rounding to the
  specified number of digits after the decimal point, default being 0) are
  already part of R base.
}
\value{
  integer values
}
\examples{
x <- c(-1.2, -0.8, 0, 0.5, 1.1, 2.9)
ceil(x)
Fix(x)
}
\keyword{ arith }
