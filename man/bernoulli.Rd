\name{bernoulli}
\alias{bernoulli}
\title{
  Bernoulli Numbers and Polynomials
}
\description{
  The Bernoulli numbers are a sequence of rational numbers that play an
  important role for the series expansion of hyperbolic functions, in the
  Euler-MacLaurin formula, or for certain values of Riemann's function at
  negative integers.
}
\usage{
bernoulli(n, x)
}
\arguments{
  \item{n}{the index, a whole number greater or equal to 0.}
  \item{x}{real number or vector of real numbers; if missing, the
           Bernoulli numbers will be given, otherwise the polynomial.}
}
\details{
  The calculation of the Bernoulli numbers uses the values of the zeta function
  at negative integers, i.e. \eqn{B_n = -n \, zeta(1-n)}. Bernoulli numbers
  \eqn{B_n} for odd \code{n} are 0 except \eqn{B_1} which is set to -0.5 on 
  purpose.

  The Bernoulli polynomials can be directly defined as
  \deqn{ B_n(x) = \sum_{k=0}^n {n \choose k} b_{n-k}\, x^k }
  and it is immediately clear that the Bernoulli numbers are then given as
  \eqn{B_n = B_n(0)}.
}
\value{
  Returns the first \code{n+1} Bernoulli numbers, if \code{x} is missing, or
  the value of the Bernoulli polynomial at point(s) \code{x}.
}
\references{
  See the entry on Bernoulli numbers in the Wikipedia.
}
\note{
  The definition uses \code{B_1 = -1/2} in accordance with the definition of
  the Bernoulli polynomials.
}
\seealso{
  \code{\link{zeta}}
}
\examples{
bernoulli(10)
# 1.00000000 -0.50000000  0.16666667  0.00000000 -0.03333333
# 0.00000000  0.02380952  0.00000000 -0.03333333  0.00000000  0.07575758
                #
\dontrun{
x1 <- linspace(0.3, 0.7, 2)
y1 <- bernoulli(1, x1)
plot(x1, y1, type='l', col='red', lwd=2,
     xlim=c(0.0, 1.0), ylim=c(-0.2, 0.2),
     xlab="", ylab="", main="Bernoulli Polynomials")
grid()
xs <- linspace(0, 1, 51)
lines(xs, bernoulli(2, xs), col="green", lwd=2)
lines(xs, bernoulli(3, xs), col="blue", lwd=2)
lines(xs, bernoulli(4, xs), col="cyan", lwd=2)
lines(xs, bernoulli(5, xs), col="brown", lwd=2)
lines(xs, bernoulli(6, xs), col="magenta", lwd=2)
legend(0.75, 0.2, c("B_1", "B_2", "B_3", "B_4", "B_5", "B_6"),
       col=c("red", "green", "blue", "cyan", "brown", "magenta"),
       lty=1, lwd=2)
  }
}
\keyword{ specfun }
