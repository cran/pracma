\name{invlap}
\alias{invlap}
\title{
  Inverse Laplacian
}
\description{
  Numerical inversion of Laplace transforms.
}
\usage{
invlap(Fs, t1, t2, nnt, a = 6, ns = 20, nd = 19)
}
\arguments{
  \item{Fs}{function representing the function to be inverse-transformed.}
  \item{t1, t2}{end points of the interval.}
  \item{nnt}{number of grid points between t1 and t2.}
  \item{a}{shift parameter; it is recommended to preserve value 6.}
  \item{ns, nd}{further parameters, increasing them leads to lower error.}
}
\details{
  The transform Fs may be any reasonable function of a variable s^a, where a 
  is a real exponent. Thus, the function \code{invlap} can solve fractional 
  problems and invert functions Fs containing (ir)rational or transcendental 
  expressions.
}
\value{
  Returns a list with components \code{x} the x-coordinates and \code{y}
  the y-coordinates representing the original function in the interval
  \code{[t1,t2]}.
}
\note{
  Based on a presentation in the first reference. The function \code{invlap} 
  on MatlabCentral (by ) served as guide. The Talbot procedure from the 
  second reference could be an interesting alternative.
}
\references{
  J. Valsa and L. Brancik (1998). Approximate Formulae for Numerical Inversion 
  of Laplace Transforms. Intern. Journal of Numerical Modelling: Electronic 
  Networks, Devices and Fields, Vol. 11, (1998), pp. 153-166.

  L.N.Trefethen, J.A.C.Weideman, and T.Schmelzer (2006). Talbot quadratures 
  and rational approximations. BIT. Numerical Mathematics, 46(3):653--670.
}
\examples{
Fs <- function(s) 1/(s^2 + 1)           # sine function
Li <- invlap(Fs, 0, 2*pi, 100)

\dontrun{
plot(Li[[1]], Li[[2]], type = "l", col = "blue"); grid()

Fs <- function(s) tanh(s)/s             # step function
L1 <- invlap(Fs, 0.01, 20, 1000)
plot(L1[[1]], L1[[2]], type = "l", col = "blue")
L2 <- invlap(Fs, 0.01, 20, 2000, 6, 280, 59)
lines(L2[[1]], L2[[2]], col="darkred"); grid()

Fs <- function(s) 1/(sqrt(s)*s)
L1 <- invlap(Fs, 0.01, 5, 200, 6, 40, 20)
plot(L1[[1]], L1[[2]], type = "l", col = "blue"); grid()

Fs <- function(s) 1/(s^2 - 1)           # hyperbolic sine function
Li <- invlap(Fs, 0, 2*pi, 100)
plot(Li[[1]], Li[[2]], type = "l", col = "blue"); grid()

Fs <- function(s) 1/s/(s + 1)           # exponential approach
Li <- invlap(Fs, 0, 2*pi, 100)
plot(Li[[1]], Li[[2]], type = "l", col = "blue"); grid()

gamma <- 0.577215664901532              # Euler-Mascheroni constant
Fs <- function(s) -1/s * (log(s)+gamma) # natural logarithm
Li <- invlap(Fs, 0, 2*pi, 100)
plot(Li[[1]], Li[[2]], type = "l", col = "blue"); grid()

Fs <- function(s) (20.5+3.7343*s^1.15)/(21.5+3.7343*s^1.15+0.8*s^2.2+0.5*s^0.9)/s
L1 <- invlap(Fs, 0.01, 5, 200, 6, 40, 20)
plot(L1[[1]], L1[[2]], type = "l", col = "blue")
grid()}
}
\keyword{ timeseries }
