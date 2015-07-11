\name{psi}
\alias{psi}
\title{
  Psi (Polygamma) Function
}
\description{
  Arbitrary order Polygamma function valid in the entire complex plane.
}
\usage{
psi(k, z)
}
\arguments{
  \item{k}{order of the polygamma function, whole number greater or equal 0.}
  \item{z}{numeric complex number or vector.}
}
\details{
  Computes the Polygamma function of arbitrary order, and valid in the entire
  complex plane. The polygamma function is defined as

  \deqn{\psi(n, z) = \frac{d^{n+1}}{dz^{n+1}} \log(\Gamma(z))}

  If \code{n} is 0 or absent then \code{psi} will be the Digamma function.
  If \code{n=1,2,3,4,5} etc. then \code{psi} will be the
  tri-, tetra-, penta-, hexa-, hepta- etc. gamma function.
}
\value{
  Returns a complex number or a vector of complex numbers.
}
\examples{
psi(2) - psi(1)         # 1
-psi(1)                 # Eulers constant: 0.57721566490153  [or, -psi(0, 1)]
psi(1, 2)               # pi^2/6 - 1     : 0.64493406684823
psi(10, -11.5-0.577007813568142i)
                        # is near a root of the decagamma function
}
