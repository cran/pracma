\name{bits}
\alias{bits}
\title{Binary Representation}
\description{
  Literal bit representation.
}
\usage{
bits(x, k = 54, pos_sign = FALSE, break0 = FALSE)
}
\arguments{
  \item{x}{a positive or negative floating point number.}
  \item{k}{number of binary digits after the decimal point}
  \item{pos_sign}{logical; shall the '+' sign be included.}
  \item{break0}{logical; shall trailing zeros be included.}
}
\details{
  The literal bit/binary representation of a floating point number is computed
  by subtracting powers of 2.
}
\value{
  Returns a string containing the binary representation.
}
\seealso{
\code{\link{nextpow2}}
}
\examples{
bits(2^10)        # "10000000000"
bits(1 + 2^-10)   #  "1.000000000100000000000000000000000000000000000000000000"
bits(pi)          # "11.001001000011111101101010100010001000010110100011000000"
bits(1/3.0)       #  "0.010101010101010101010101010101010101010101010101010101"
bits(1 + eps())   #  "1.000000000000000000000000000000000000000000000000000100"
}
\keyword{ arith }
