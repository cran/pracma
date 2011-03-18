\name{modinv}
\alias{modinv}
\title{Modular Inverse}
\description{
  Computes the modular inverse of \code{n} modulo \code{m}.
}
\usage{
modinv(n, m)
}
\arguments{
  \item{n, m}{integer scalars}
}
\details{
  The modular inverse of \code{n} modulo \code{m} is the unique natural
  number \code{0 < n0 < m} such that \code{n * n0 = 1 mod m}.
}
\value{
  a natural number smaller \code{m}, if \code{n} and \code{m} are coprime,
  else \code{NA}.
}
\seealso{
\code{\link{extgcd}}
}
\examples{
modinv(5, 1001)  #=> 801, as 5*801 = 4005 = 1 mod 1001

Modinv <- Vectorize(modinv, "n")
mod((1:10)*Modinv(1:10, 11), 11)  #=> 1 1 1 1 1 1 1 1 1 1
}
\keyword{ arith }
