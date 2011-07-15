\name{droplet_e}
\alias{droplet_e}
\title{
  Droplet for e
}
\description{
  Generates digits for the Euler number e.
}
\usage{
droplet_e(n)
}
\arguments{
  \item{n}{number of digits after the decimal point; should not exceed 1000
           much as otherwise it will be \emph{very} slow.}
}
\details{
  Based on a formula discovered by S. Rabinowitz and S. Wagon.
}
\value{
  String containing ``2.718281828..." with \code{n} digits after the decimal point.
}
\references{
  Borwein, J., and K. Devlin (2009). The Computer as Crucible: An Introduction
  to Experimental Mathematics. A K Peters, Ltd.
}
\examples{
##  Example
E <- droplet_e(1002)
table(strsplit(substring(E, 3, 1002), ""))
#    0   1   2   3   4   5   6   7   8   9 
#  100  96  97 109 100  85  99  99 103 112 
}
\keyword{ math }

