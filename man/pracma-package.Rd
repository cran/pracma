\name{pracma-package}
\alias{pracma-package}
\alias{pracma}
\docType{package}
\title{
Practical Numerical Math Routines
}
\description{
  This package provides R implementations of more advanced
  math functions from Matlab and Octave (and the Euler Math Toolbox)
  with a special view on optimization and time series routines.

  Some of these realizations are the result of courses on Scientific
  Computing (``Wissenschaftliches Rechnen'') and are mostly intended to
  demonstrate how to implement certain algorithms in R/S.
}
\details{
  Several implementations of root finding routines and numerical sorting
  are provided as examples.
}
\author{
Hans W Borchers

Maintainer: HwB  <hwborchers@googlemail.com>
}
\references{
  Abramowitz, M., and I. A. Stegun (1972). Handbook of Mathematical Functions
  (with Formulas, Graphs, and Mathematical Tables). Dover, New York.
  \url{http://www.nr.com/aands/}.

  Arndt, J. (2010). Matters Computational: Ideas, Algorithms, Source Code.
  Springer-Verlag, Berlin Heidelberg Dordrecht.
  FXT: a library of algorithms: \url{http://www.jjj.de/fxt/}.

  Cormen, Th. H., Ch. E. Leiserson, and R. L. Rivest (2009). Introduction
  to Algorithms. Third Edition, The MIT Press, Cambridge, MA.

  Gautschi, W. (1997). Numerical Analysis: An Introduction.
  Birkhaeuser, Boston.

  Gentle, J. E. (2009). Computational Statistics.
  Springer Science+Business Media LCC, New York.

  Hazewinkel, M., Editor (2002). Encyclopaedia of Mathematics.
  Springer-Verlag, Berlin Heidelberg New York.
  \url{http://eom.springer.de/}.

  MathWorld.com (2011).
  Matlab Central: \url{http://www.mathworks.com/matlabcentral/}.
  Mathtools.net: \url{http://www.mathtools.net/}.

  NIST: National Institute of Standards and Technology.
  Olver, F. W. J., et al. (2010). NIST Handbook of Mathematical Functions.
  Cambridge University Press. 
  Internet: NIST Digital Library of Mathematical Functions, 
  \url{http://dlmf.nist.gov/};
  Dictionary of Algorithms and Data Structures,
  \url{http://xlinux.nist.gov/dads/};
  Guide to Available Mathematical Software, \url{http://gams.nist.gov/}

  Press, W. H., S. A. Teukolsky, W. T Vetterling, and B. P. Flannery (2007).
  Numerical Recipes: The Art of Numerical Computing. Third Edition, incl.
  Numerical Recipes Software, Cambridge University Press, New York.
  \url{http://www.nrbook.com/a/bookcpdf.php} [chapters], or
  \url{http://apps.nrbook.com/c/index.html} [pages].

  Quarteroni, A., and F. Saleri (2006). Scientific Computing with Matlab
  and Octave. Second Edition, Springer-Verlag, Berlin Heidelberg.

  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.

  Skiena, St. S. (2008). The Algorithm Design Manual. Second Edition,
  Springer-Verlag, London. The Stony Brook Algorithm Repository:
  \url{http://www.cs.sunysb.edu/~algorith/}.

  Stoer, J., and R. Bulirsch (2002). Introduction to Numerical Analysis.
  Third Edition, Springer-Verlag, New York.

  Strang, G. (2007). Computational Science and Engineering.
  Wellesley-Cambridge Press.
  Matlab Codes: \url{http://www-math.mit.edu/cse/}

  Weisstein, E. W. (2003). CRC Concise Encyclopedia of Mathematics.
  Second Edition, Chapman & Hall/CRC Press.
  Wolfram MathWorld: \url{http://mathworld.wolfram.com/}.  
}
\keyword{ package }
\seealso{
  The R package `matlab' contains some of the basic routines from Matlab,
  but unfortunately not any of the higher math routines.
}
\examples{
\dontrun{
tail(primes(1e7))  # the last 5 prime numbers below 10,000,000 .}
}
