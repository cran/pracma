\name{ellipke,ellipj}
\alias{ellipke}
\alias{ellipj}
\title{
  Elliptic and Jacobi Elliptic Integrals
}
\description{
  Complete elliptic integrals of the first and second kind, and
  Jacobi elliptic integrals.
}
\usage{
ellipke(m, tol = .Machine$double.eps)

ellipj(u, m, tol = .Machine$double.eps)
}
\arguments{
  \item{u}{numeric vector.}
  \item{m}{input vector, all input elements must satisfy \code{0 <= x <= 1}.}
  \item{tol}{tolerance; default is machine precision.}
}
\details{
  \code{ellipke} computes the complete elliptic integrals to accuracy 
  \code{tol}, based on the algebraic-geometric mean.

  \code{ellipj} computes the Jacobi elliptic integrals \code{sn}, \code{cn},
  and \code{dn}. For instance, \eqn{sn} is the inverse function for
  \deqn{u = \int_0^\phi dt / \sqrt{1 - m \sin^2 t}}
  with \eqn{sn(u) = \sin(\phi)}.

  Some definitions of the elliptic functions use the modules \code{k} instead
  of the parameter \code{m}. They are related by \code{k^2=m=sin(a)^2} where
  \code{a} is the `modular angle'.
}
\value{
  \code{ellipke} returns list with two components, \code{k} the values for 
  the first kind, \code{e} the values for the second kind.

  \code{ellipj} returns a list with components the three Jacobi elliptic
  integrals \code{sn}, \code{cn}, and \code{dn}.
}
\references{
  Abramowitz, M., and I. A. Stegun (1965).
  Handbook of Mathematical Functions. Dover Publications, New York.
}
\seealso{
  \code{elliptic::sn,cn,dn}
}
\examples{
x <- linspace(0, 1, 20)
ke <- ellipke(x)

\dontrun{
plot(x, ke$k, type = "l", col ="darkblue", ylim = c(0, 5),
     main = "Elliptic Integrals")
lines(x, ke$e, col = "darkgreen")
legend( 0.01, 4.5,
        legend = c("Elliptic integral of first kind",
                   "Elliptic integral of second kind"),
        col = c("darkblue", "darkgreen"), lty = 1)
grid()}

## ellipse circumference with axes a, b
ellipse_cf <- function(a, b) {
    return(4*a*ellipke(1 - (b^2/a^2))$e)
}
print(ellipse_cf(1.0, 0.8), digits = 10)
# [1] 5.672333578

## Jacobi elliptic integrals
u <- c(0, 1, 2, 3, 4, 5)
m <- seq(0.0, 1.0, by = 0.2)
je <- ellipj(u, m)
# $sn       0.0000  0.8265  0.9851  0.7433  0.4771  0.9999
# $cn       1.0000  0.5630 -0.1720 -0.6690 -0.8789  0.0135
# $dn       1.0000  0.9292  0.7822  0.8176  0.9044  0.0135
je$sn^2 + je$cn^2       # 1 1 1 1 1 1
je$dn^2 + m * je$sn^2   # 1 1 1 1 1 1
}
\keyword{ specfun }
