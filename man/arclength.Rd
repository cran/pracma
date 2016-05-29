\name{arclength}
\alias{arclength}
\title{
  Arc Length of a Curve
}
\description{
  Calculates the arc length of a parametrized curve.
}
\usage{
arclength(f, a, b, nmax = 20, tol = 1e-05, ...)
}
\arguments{
  \item{f}{parametrization of a curve in n-dim. space.}
  \item{a,b}{begin and end of the parameter interval.}
  \item{nmax}{maximal number of iterations.}
  \item{tol}{relative tolerance requested.}
  \item{...}{additional arguments to be passed to the function.}
}
\details{
  Calculates the arc length of a parametrized curve in \code{R^n}. It applies
  Richardson's extrapolation by refining polygon approximations to the curve.

  The parametrization of the curve must be vectorized:
  if \code{t-->F(t)} is the parametrization, \code{F(c(t1,t1,...))} must
  return \code{c(F(t1),F(t2),...)}.

  Can be directly applied to determine the arc length of a one-dimensional 
  function \code{f:R-->R} by defining \code{F} (if \code{f} is vectorized)
  as \code{F:t-->c(t,f(t))}.
}
\value{
  Returns a list with components \code{length} the calculated arc length,
  \code{niter} the number of iterations, and \code{rel.err} the relative
  error generated from the extrapolation.
}
\author{
  HwB <hwborchers@googlemail.com>
}
\note{
  If by chance certain equidistant points of the curve lie on a straight line,
  the result may be wrong, then use \code{polylength} below.
}
\seealso{
  \code{\link{poly_length}}
}
\examples{
##  Example: parametrized 3D-curve with t in 0..3*pi
f <- function(t) c(sin(2*t), cos(t), t)
arclength(f, 0, 3*pi)
# $length:  17.22203            # true length 17.222032...

##  Example: length of the sine curve
f <- function(t) c(t, sin(t))
arclength(f, 0, pi)             # true length  3.82019...

## Example: Length of an ellipse with axes a = 1 and b = 0.5
# parametrization x = a*cos(t), y = b*sin(t)
a <- 1.0; b <- 0.5
f <- function(t) c(a*cos(t), b*sin(t))
L <- arclength(f, 0, 2*pi, tol = 1e-10)     #=> 4.84422411027
# compare with elliptic integral of the second kind
e <- sqrt(1 - b^2/a^2)                      # ellipticity
L <- 4 * a * ellipke(e^2)$e                 #=> 4.84422411027

\dontrun{
##  Example: oscillating 1-dimensional function (from 0 to 5)
f <- function(x) x * cos(0.1*exp(x)) * sin(0.1*pi*exp(x))
F <- function(t) c(t, f(t))
L <- arclength(F, 0, 5, tol = 1e-12, nmax = 25)
print(L$length, digits = 16)
# [1] 82.81020372882217         # true length 82.810203728822172...

# Split this computation in 10 steps (run time drops from 2 to 0.2 secs)
L <- 0
for (i in 1:10)
	L <- L + arclength(F, (i-1)*0.5, i*0.5, tol = 1e-10)$length
print(L, digits = 16)
# [1] 82.81020372882216

# Alternative calculation of arc length
f1 <- function(x) sqrt(1 + complexstep(f, x)^2)
L1 <- quadgk(f1, 0, 5, tol = 1e-14)
print(L1, digits = 16)
# [1] 82.81020372882216
  }

\dontrun{
#-- --------------------------------------------------------------------
#   Arc-length parametrization of Fermat's spiral
#-- --------------------------------------------------------------------
# Fermat's spiral: r = a * sqrt(t) 
f <- function(t) 0.25 * sqrt(t) * c(cos(t), sin(t))

t1 <- 0; t2 <- 6*pi
a  <- 0; b  <- arclength(f, t1, t2)$length
fParam <- function(w) {
    fct <- function(u) arclength(f, a, u)$length - w
    urt <- uniroot(fct, c(a, 6*pi))
    urt$root
}

ts <- linspace(0, 6*pi, 250)
plot(matrix(f(ts), ncol=2), type='l', col="blue", 
     asp=1, xlab="", ylab = "",
     main = "Fermat's Spiral", sub="20 subparts of equal length")

for (i in seq(0.05, 0.95, by=0.05)) {
    v <- fParam(i*b); fv <- f(v)
    points(fv[1], f(v)[2], col="darkred", pch=20)
} }
}
\keyword{ math }
