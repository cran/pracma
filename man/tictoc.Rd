\name{tictoc}
\alias{tic}
\alias{toc}
\title{MATLAB timer functions}
\description{
  Provides stopwatch timer. Function \code{tic} starts the timer and \code{toc}
  updates the elapsed time since the timer was started. 
}
\usage{
tic(gcFirst=FALSE)
toc(echo=TRUE)
}
\arguments{
  \item{gcFirst}{logical scalar. If \code{TRUE}, perform garbage collection
    prior to starting stopwatch}
  \item{echo}{logical scalar. If \code{TRUE}, print elapsed time to screen}
}
\details{
  Provides analog to \code{\link[base]{system.time}}.
  Function \code{toc} can be invoked multiple times in a row.
}
\examples{
tic()
for(i in 1:100) mad(runif(1000))	# kill time
toc()
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\keyword{utilities}

