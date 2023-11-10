\name{ridders}
\alias{ridders}
\title{
  Ridders' Root Finding Method
}
\description{
  Ridders' root finding method is a powerful variant of `regula falsi' (and
  `false position'). In reliability and speed, this method is competitive
  with Brent-Dekker and similar approaches.
}
\usage{
ridders(fun, a, b, maxiter = 500, tol = 1e-12, ...)
}
\arguments{
\item{fun}{function whose root is to be found.}
\item{a, b}{left and right interval bounds.}
\item{maxiter}{maximum number of iterations (function calls).}
\item{tol}{tolerance, length of the last interval.}
\item{...}{additional parameters passed on to the function.}
}
\details{
  Given a bracketing interval $[x_1, x_2]$, the method first calculates the
  midpoint \eqn{x_3 = (x_1 + x_2)/2} and the uses an updating formula
  \deqn{x_4 = x_3 + (x_3 - x_1) \frac{sgn(f(x_1) - f(x_2)) f(x_3)}{\sqrt{f(x_3)^2 - f(x_1) f(x_2)}}} 
}
\value{
  Returns a list with components
  \item{root}{root of the function.}
  \item{f.root}{value of the function at the found root.}
  \item{niter}{number of iterations,or more specifically: number of function calls.}
  \item{estim.prec}{the estimated precision, coming from the last brackett.}
}
\note{
  See function \code{f12} whose zero at \eqn{\sqrt{e}} is difficult to find
  exactly for all root finders.
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\references{
  Press, Teukolsky, Vetterling, and Flannery (1992). Numerical Recipes in C.
  Cambridge University Press.
}
\seealso{
  \code{\link{brent}}
}
\examples{
##  Test functions
f1  <- function(x)                          # [0, 1.2],     0.399 422 2917
            x^2 * (x^2/3 + sqrt(2)*sin(x)) - sqrt(3)/18
f2  <- function(x) 11*x^11 - 1              # [0.4, 1.6],   0.804 133 0975
f3  <- function(x) 35*x^35 - 1              # [-0.5, 1.9],  0.903 407 6632
f4  <- function(x)                          # [-0.5, 0.7],  0.077 014 24135
            2*(x*exp(-9) - exp(-9*x)) + 1 
f5  <- function(x) x^2 - (1 - x)^9          # [-1.4, 1],    0.259 204 4937
f6  <- function(x) (x-1)*exp(-9*x) + x^9    # [-0.8, 1.6],  0.536 741 6626
f7  <- function(x) x^2 + sin(x/9) - 1/4     # [-0.5, 1.9],  0.4475417621
f8  <- function(x) 1/8 * (9 - 1/x)        # [0.001, 1.201], 0.111 111 1111 
f9  <- function(x) tan(x) - x - 0.0463025   # [-0.9, 1.5],  0.500 000 0340
f10 <- function(x)                          # [0.4, 1],     0.679 808 9215
            x^2 + x*sin(sqrt(75)*x) - 0.2
f11 <- function(x) x^9 + 0.0001             # [-1.2, 0],   -0.359 381 3664 
f12 <- function(x)                          # [1, 3.4],     1.648 721 27070
            log(x) + x^2/(2*exp(1)) - 2 * x/sqrt(exp(1)) + 1

r <- ridders(f1 , 0, 1.2);       r$root; r$niter # 18
r <- ridders(f2 , 0.4, 1.6);     r$root; r$niter # 14
r <- ridders(f3 ,-0.5, 1.9);     r$root; r$niter # 20
r <- ridders(f4 ,-0.5, 0.7);     r$root; r$niter # 12
r <- ridders(f5 ,-1.4, 1);       r$root; r$niter # 16
r <- ridders(f6 ,-0.8, 1.6);     r$root; r$niter # 20
r <- ridders(f7 ,-0.5, 1.9);     r$root; r$niter # 16
r <- ridders(f8 ,0.001, 1.201);  r$root; r$niter # 18
r <- ridders(f9 ,-0.9, 1.5);     r$root; r$niter # 20
r <- ridders(f10,0.4, 1);        r$root; r$niter # 14
r <- ridders(f11,-1.2, 0);       r$root; r$niter # 12
r <- ridders(f12,1, 3.4);        r$root; r$niter # 30, err = 1e-5

\dontrun{
##  Use ridders() with Rmpfr
options(digits=16)
library("Rmpfr") # unirootR
prec <- 256
.N <- function(.) mpfr(., precBits = prec)

f12 <- function(x) {
    e1 <- exp(.N(1))
    log(x) + x^2/(2*e1) - 2*x/sqrt(e1) + 1
}
sqrte <- sqrt(exp(.N(1)))  # 1.648721270700128...
f12(sqrte)                 # 0

unirootR(f12, interval=mpfr(c(1, 3.4), prec), tol=1e-20)
# $root
# 1 'mpfr' number of precision  200   bits 
# [1] 1.648721270700128...

ridders(f12, .N(1), .N(3.4), maxiter=200, tol=1e-20)
# $root
# 1 'mpfr' number of precision  200   bits 
# [1] 1.648721270700128...
}
}
\keyword{ math }
