\name{cot,csc,sec, etc.}
\alias{cot}
\alias{csc}
\alias{sec}
\alias{acot}
\alias{acsc}
\alias{asec}
\title{
  More Trigonometric Functions
}
\description{
  More trigonometric functions not available in R.
}
\usage{
cot(z)
csc(z)
sec(z)
acot(z)
acsc(z)
asec(z)
}
\arguments{
  \item{z}{numeric or complex scalar or vector.}
}
\details{
  The usual trigonometric cotangens, cosecans, and secans functions
  and their inverses, computed through the other well known -- in R --
  sine, cosine, and tangens functions.
}
\value{
  Result vector of numeric or complex values.
}
\note{
  These function names are available in Matlab, that is the reason they have
  been added to the `pracma' package.
}
\seealso{
  Trigonometric and hyperbolic functions in R.
}
\examples{
cot(1+1i)       # 0.2176 - 0.8680i
csc(1+1i)       # 0.6215 - 0.3039i
sec(1+1i)       # 0.4983 + 0.5911i
acot(1+1i)      # 0.5536 - 0.4024i
acsc(1+1i)      # 0.4523 - 0.5306i
asec(1+1i)      # 1.1185 + 0.5306i
}
\keyword{ math }
