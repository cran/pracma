\name{mexpfit}
\alias{mexpfit}
\title{
  Multi-exponential Fitting
}
\description{
  Multi-exponential fitting means fitting of data points by a sum of
  (decaying) exponential functions, with or without a constant term.
}
\usage{
mexpfit(x, y, p0, w = NULL, const = TRUE, options = list())
}
\arguments{
  \item{x, y}{x-, y-coordinates of data points to be fitted.}
  \item{p0}{starting values for the exponentials alone; can be positive
            or negative, but not zero.}
  \item{w}{weight vector; not used in this version.}
  \item{const}{logical; shall an absolute term be included.}
  \item{options}{list of options for \code{lsqnonlin}, see there.}
}
\details{
  The multi-exponential fitting problem is solved here with with a separable
  nonlinear least-squares approach. If the following function is to be fitted,
  \deqn{y = a_0 + a_1 e^{b_1 x} + \ldots + a_n e^{b_n x}}
  it will be looked at as a nonlinear optimization problem of the coefficients
  \eqn{b_i} alone. Given the \eqn{b_i}, coefficients \eqn{a_i} are uniquely
  determined as solution of an (overdetermined) system of linear equations.

  This approach reduces the dimension of the search space by half and improves
  numerical stability and accuracy. As a convex problem, the solution is unique
  and global.

  To solve the nonlinear part, the function \code{lsqnonlin} that uses the
  Levenberg-Marquard algorithm will be applied.
}
\value{
  \code{mexpfit} returns a list with the following elements:
  \itemize{
    \item \code{a0}: the absolute term, 0 if \code{const} is false.
    \item \code{a}: linear coefficients.
    \item \code{b}: coefficient in the exponential functions.
    \item \code{ssq}: the sum of squares for the final fitting.
    \item \code{iter}: number of iterations resp. function calls.
    \item \code{errmess}: an error or info message.
  }
}
\note{
  As the Jacobian for this expression is known, a more specialized approch
  would be possible, without using \code{lsqnonlin};
  see the \code{immoptibox} of H. B. Nielsen, Techn. University of Denmark.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\references{
  Madsen, K., and H. B. Nielsen (2010). Introduction to Optimization and
  Data Fitting. Technical University of Denmark, Intitute of Computer
  Science and Mathematical Modelling.

  Nielsen, H. B. (2000). Separable Nonlinear Least Squares. IMM, DTU,
  Report IMM-REP-2000-01.
}
\seealso{
  \code{\link{lsqsep}}, \code{\link{lsqnonlin}}
}
\examples{
#   Lanczos1 data (artificial data)
#   f(x) = 0.0951*exp(-x) + 0.8607*exp(-3*x) + 1.5576*exp(-5*x)
x <- linspace(0, 1.15, 24)
y <- c(2.51340000, 2.04433337, 1.66840444, 1.36641802, 1.12323249, 0.92688972,
       0.76793386, 0.63887755, 0.53378353, 0.44793636, 0.37758479, 0.31973932,
       0.27201308, 0.23249655, 0.19965895, 0.17227041, 0.14934057, 0.13007002,
       0.11381193, 0.10004156, 0.08833209, 0.07833544, 0.06976694, 0.06239313)
p0 <- c(-0.3, -5.5, -7.6)
mexpfit(x, y, p0, const = FALSE)
## $a0
## [1] 0
## $a
## [1] 0.09510431 0.86071171 1.55758398
## $b
## [1] -1.000022 -3.000028 -5.000009
## $ssq
## [1] 1.936163e-16
## $iter
## [1] 26
## $errmess
## [1] "Stopped by small gradient."
}
\keyword{ fitting }
