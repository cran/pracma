\name{coth,csch,sech, etc.}
\alias{coth}
\alias{csch}
\alias{sech}
\alias{acoth}
\alias{acsch}
\alias{asech}
\title{
  More Hyperbolic Functions
}
\description{
  More hyperbolic functions not available in R.
}
\usage{
coth(z)
csch(z)
sech(z)
acoth(z)
acsch(z)
asech(z)
}
\arguments{
  \item{z}{numeric or complex scalar or vector.}
}
\details{
  The usual hyperbolic cotangens, cosecans, and secans functions and
  their inverses, computed through the other well known -- in R --
  hyperbolic sine, cosine, and tangens functions.
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
coth(1+1i)      # 0.8680 - 0.2176i
csch(1+1i)      # 0.3039 - 0.6215i
sech(1+1i)      # 0.4983 - 0.5911i
acoth(1+1i)     # 0.4024 - 0.5536i
acsch(1+1i)     # 0.5306 - 0.4523i
asech(1+1i)     # 0.5306 - 1.1185i
}
\keyword{ math }
