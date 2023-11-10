\name{roots, polyroots}
\alias{roots}
\alias{rootsmult}
\alias{polyroots}
\title{Polynomial Roots}
\description{
  Computes the roots (and multiplicities) of a polynomial.
}
\usage{
  roots(p)
  polyroots(p, ntol = 1e-04, ztol = 1e-08)

  rootsmult(p, r, tol=1e-12)
}
\arguments{
  \item{p}{vector of real or complex numbers representing the polynomial.}
  \item{r}{a possible root of the polynomial.}
  \item{tol, ntol, ztol}{norm tolerance and accuracy for polyroots.}
}
\details{
  The function \code{roots} computes roots of a polynomial as eigenvalues 
  of the companion matrix. 

  \code{polyroots} attempts to refine the results of \code{roots} with special 
  attention to multiple roots. For a reference of this implementation see 
  F. C. Chang, "Solving multiple-root polynomials", 
  IEEE Antennas and Propagation Magazine Vol. 51, No. 6 (2010), pp. 151-155.

  \code{rootsmult} determines te order of a possible root \code{r}. As this 
  computation is problematic in double precision, the result should be taken 
  with a grain of salt.
}
\value{
  \code{roots} returns a vector holding the roots of the polynomial, 
  \code{rootsmult} the multiplicity of a root as an integer. And 
  \code{polyroots} returns a data frame witha column 'root' and a column 
  'mult' giving the multiplicity of that root.
}
\seealso{
  \code{\link{polyroot}}
}
\examples{
  roots(c(1, 0, 1, 0, 0))                     # 0 0 1i -1i
  p <- Poly(c(-2, -1, 0, 1, 2))               # 1*x^5 - 5*x^3 + 4*x
  roots(p)                                    # 0 -2  2 -1  1

  p <- Poly(c(rep(1, 4), rep(-1, 4), 0, 0))   # 1  0 -4  0  6  0 -4  0  1
  rootsmult(p, 1.0); rootsmult(p, -1.0)       # 4  4
  polyroots(p)
  ##   root mult
  ## 1    0    2
  ## 2    1    4
  ## 3   -1    4
}
\keyword{ math }
