\name{lsqnonlin}
\alias{lsqnonlin}
\alias{lsqnonneg}
\title{
  Nonlinear Least-Squares Fitting
}
\description{
  \code{lsqnonlin} solves nonlinear least-squares problems, including
  nonlinear data-fitting problems, through the Levenberg-Marquardt approach.

  \code{lsqnonneg} solve nonnegative least-squares constraints problem.
}
\usage{
lsqnonlin(fun, x0, options = list(), ...)

lsqnonneg(C, d)
}
\arguments{
  \item{fun}{User-defined, vector-valued function.}
  \item{x0}{starting point.}
  \item{...}{additional parameters passed to the function.}
  \item{options}{list of options, for details see below.}
  \item{C, d}{matrix and vector such that \code{C x - d} will be
              minimized with \code{x >= 0}.}
}
\details{
  \code{lsqnonlin} computes the sum-of-squares of the vector-valued function
  \code{fun}, that is if \eqn{f(x) = (f_1(x), \ldots ,f_n(x))} then
  \deqn{min || f(x) ||_2^2 = min(f_1(x)^2 + \ldots + f_n(x)^2)}
  will be minimized.

  \code{x=lsqnonlin(fun,x0)} starts at point \code{x0} and finds a minimum
  of the sum of squares of the functions described in fun. \code{fun} shall
  return a vector of values and not the sum of squares of the values.
  (The algorithm implicitly sums and squares fun(x).)

  \code{options} is a list with the following components and defaults:
  \itemize{
    \item \code{tau}: used in starting value for Marquardt parameter.
    \item \code{tolx}: stopping parameter for step length.
    \item \code{tolg}: stopping parameter for gradient.
    \item \code{maxeval} the maximum number of function evaluations.
  }
  Typical values for \code{tau} are from \code{1e-6...1e-3...1} with small
  values for good starting points and larger values for not so good or known
  bad starting points.

  \code{lsqnonneg} solves the linear least-squares problem \code{C x - d},
  \code{x} nonnegative, transforming it with the `trick' \code{x --> exp(x)}
  into a nonlinear one and solving it applying \code{lsqnonlin}.
}
\value{
  Returns a list with the following elements:
  \itemize{
    \item \code{x}: the point with least sum of squares value.
    \item \code{ssq}: the sum of squares.
    \item \code{ng}: norm of last gradient.
    \item \code{nh}: norm of last step used.
    \item \code{mu}: damping parameter of Levenberg-Marquardt.
    \item \code{neval}: number of function evaluations.
    \item \code{errno}: error number, corresponds to error message.
    \item \code{errmess}: error message, i.e. reason for stopping.
  }
}
\note{
  The refined approach, Fletcher's version of the Levenberg-Marquardt
  algorithm, may be added at a later time; see the references.
}
\references{
  Madsen, K., and H. B.Nielsen (2010).Introduction to Optimization and
  Data Fitting. Technical University of Denmark, Intitute of Computer
  Science and Mathematical Modelling.

  Fletcher, R., (1971). A Modified Marquardt Subroutine for Nonlinear Least
  Squares. Report AERE-R 6799, Harwell.
}
\seealso{
  \code{\link{nlm}}, \code{\link{nls}}
}
\examples{
##  Rosenberg function as least-squares problem
x0  <- c(0, 0)
fun <- function(x) c(10*(x[2]-x[1]^2), 1-x[1])
lsqnonlin(fun, x0)

##  Example from R-help
y <- c(5.5199668,  1.5234525,  3.3557000,  6.7211704,  7.4237955,  1.9703127,
       4.3939336, -1.4380091,  3.2650180,  3.5760906,  0.2947972,  1.0569417)
x <- c(1,   0,   0,   4,   3,   5,  12,  10,  12, 100, 100, 100)
# Define target function as difference
f <- function(b)
     b[1] * (exp((b[2] - x)/b[3]) * (1/b[3]))/(1 + exp((b[2] - x)/b[3]))^2 - y
x0 <- c(21.16322, 8.83669, 2.957765)
lsqnonlin(f, x0)        # ssq 50.50144 at c(36.133144, 2.572373, 1.079811)

# nls() will break down
# nls(Y ~ a*(exp((b-X)/c)*(1/c))/(1 + exp((b-X)/c))^2,
#     start=list(a=21.16322, b=8.83669, c=2.957765), algorithm = "plinear")
# Error: step factor 0.000488281 reduced below 'minFactor' of 0.000976563

##  Example: Hougon function
x1 <- c(470, 285, 470, 470, 470, 100, 100, 470, 100, 100, 100, 285, 285)
x2 <- c(300,  80, 300,  80,  80, 190,  80, 190, 300, 300,  80, 300, 190)
x3 <- c( 10,  10, 120, 120,  10,  10,  65,  65,  54, 120, 120,  10, 120)
rate <- c(8.55,  3.79, 4.82, 0.02,  2.75, 14.39, 2.54,
          4.35, 13.00, 8.50, 0.05, 11.32,  3.13)
fun <- function(b)
        (b[1]*x2 - x3/b[5])/(1 + b[2]*x1 + b[3]*x2 + b[4]*x3) - rate
lsqnonlin(fun, rep(1, 5))
# $x    [1.25258502 0.06277577 0.04004772 0.11241472 1.19137819]
# $ssq  0.298901

## Least-squares data fitting
# Define fun(p, x)
lsqcurvefit <- function(fun, p0, xdata, ydata) {
    fn <- function(p, x) fun(p, xdata) - ydata
    lsqnonlin(fn, p0)    
}

##  Lanczos1 data (artificial data)
#   f(x) = 0.0951*exp(-x) + 0.8607*exp(-3*x) + 1.5576*exp(-5*x)
x <- linspace(0, 1.15, 24)
y <- c(2.51340000, 2.04433337, 1.66840444, 1.36641802, 1.12323249, 0.92688972,
       0.76793386, 0.63887755, 0.53378353, 0.44793636, 0.37758479, 0.31973932,
       0.27201308, 0.23249655, 0.19965895, 0.17227041, 0.14934057, 0.13007002,
       0.11381193, 0.10004156, 0.08833209, 0.07833544, 0.06976694, 0.06239313)

p0 <- c(1.2, 0.3, 5.6, 5.5, 6.5, 7.6)
fp <- function(p, x) p[1]*exp(-p[2]*x) + p[3]*exp(-p[4]*x) + p[5]*exp(-p[6]*x)
lsqcurvefit(fp, p0, x, y)

##  Example for lsqnonneg()
C <- matrix(c(0.0372, 0.2868,
              0.6861, 0.7071,
              0.6233, 0.6245,
              0.6344, 0.6170), nrow = 4, ncol = 2, byrow = TRUE)
d <- c(0.8587, 0.1781, 0.0747, 0.8405)

sol <- lsqnonneg(C, d)
cbind(qr.solve(C, d), sol$x)
# -2.563884  5.515869e-08
#  3.111911  6.929003e-01
}
\keyword{ fitting }
