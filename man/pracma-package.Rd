\name{pracma-package}
\alias{pracma-package}
\alias{pracma}
\docType{package}
\title{
Practical Numerical Math Routines
}
\description{
  This package provides R implementations of more advanced functions in
  numerical analysis, with a special view on on optimization and time
  series routines. Uses Matlab/Octave function names where appropriate
  to simplify porting.

  Some of these implementations are the result of courses on Scientific
  Computing (``Wissenschaftliches Rechnen'') and are mostly intended to
  demonstrate how to implement certain algorithms in R/S. Others are
  implementations of algorithms found in textbooks.
}
\details{
  The package encompasses functions from all areas of numerical analysis,
  for example:
  \itemize{
    \item Root finding and minimization of univariate functions,\cr
    e.g. Newton-Raphson, Brent-Dekker, Fibonacci or `golden ratio' search.
    \item Handling polynomials, including roots and polynomial fitting,\cr
    e.g. Laguerre's and Muller's methods.
    \item Interpolation and function approximation,\cr
    barycentric Lagrange interpolation, Pade and rational interpolation,
    Chebyshev or trigonometric approximation.
    \item Some special functions,\cr
    e.g. Fresnel integrals, Riemann's Zeta or the complex Gamma function,
    and Lambert's W computed iteratively through Newton's method.
    \item Special matrices, e.g. Hankel, Rosser, Wilkinson
    \item Numerical differentiation and integration,\cr
    Richardson approach and ``complex step'' derivatives, adaptive
    Simpson and Lobatto integration and adaptive Gauss-Kronrod quadrature.
    \item Solvers for ordinary differential equations and systems,\cr
    Euler-Heun, classical Runge-Kutta, ode23, or predictor-corrector method 
    such as the Adams-Bashford-Moulton.
    \item Some functions from number theory,\cr
    such as primes and prime factorization, extended Euclidean algorithm.
    \item Sorting routines, e.g. recursive quickstep.
    \item Several functions for string manipulation and regular search,
    all wrapped and named similar to their Matlab analogues.
  }

  It serves three main goals:
  \itemize{
    \item Collecting R scripts that can be demonstrated in courses on
    `Numerical Analysis' or `Scientific Computing' using R/S as the chosen
    programming language.
    \item Wrapping functions with appropriate Matlab names to simplify
    porting programs from Matlab or Octave to R.
    \item Providing an environment in which R can be used as a full-blown
    numerical computing system.
  }

  Besides that, many of these functions could be called in R applications
  as they do not have comparable counterparts in other R packages (at least
  at this moment, as far as I know). 

  All referenced books have been utilized in one way or another.
  Web links have been provided where reasonable.
}
\note{
  The following 220 functions are emulations of correspondingly named Matlab 
  functions and bear the same signature as their Matlab cousins if possible:
  
  \code{accumarray, acosd, acot, acotd, acoth, acsc, acscd, acsch, and, angle, ans,}\cr
  \code{    arrayfun, asec, asecd, asech, asind, atand, atan2d,}\cr
  \code{beep, bernoulli, blank, blkdiag, bsxfun,}\cr
  \code{cart2pol, cart2sph, cd, ceil, circshift, clear, compan, cond, conv,}\cr
  \code{    cosd, cot, cotd, coth, cross, csc, cscd, csch, cumtrapz,}\cr
  \code{dblquad, deblank, deconv, deg2rad, detrend, deval, disp, dot,}\cr
  \code{eig, eigint, ellipj, ellipke, eps, erf, erfc, erfcinv, erfcx, erfi, erfinv,}\cr
  \code{    errorbar, expint, expm, eye, ezcontour, ezmesh, ezplot, ezpolar, ezsurf,}\cr
  \code{fact, fftshift, figure, findpeaks, findstr, flipdim, fliplr, flipud,}\cr
  \code{    fminbnd, fmincon, fminsearch, fminunc, fplot, fprintf, fsolve, fzero,}\cr
  \code{gammainc, gcd, geomean, gmres, gradient,}\cr
  \code{hadamard, hankel, harmmean, hilb, histc, humps, hypot,}\cr
  \code{idivide, ifft, ifftshift, inpolygon, integral, integral2, integral3,}\cr
  \code{    interp1, interp2, inv, isempty, isprime,}\cr
  \code{kron,}\cr
  \code{legendre, linprog, linspace, loglog, logm, logseq, logspace, lsqcurvefit,}\cr
  \code{    lsqlin, lsqnonlin, lsqnonneg, lu,}\cr
  \code{magic, meshgrid, mkpp, mldivide, mod, mrdivide,}\cr
  \code{nchoosek, ndims, nextpow2, nnz, normest, nthroot, null, num2str, numel,}\cr
  \code{ode23, ode23s, ones, or, orth,}\cr
  \code{pascal, pchip, pdist, pdist2, peaks, perms, piecewise, pinv, plotyy,}\cr
  \code{    pol2cart, polar, polyfit, polyint, polylog, polyval, pow2, ppval,}\cr
  \code{    primes, psi, pwd,}\cr
  \code{quad, quad2d, quadgk, quadl, quadprog, quadv, quiver,}\cr
  \code{rad2deg, randi, randn, randsample, rat, rats, regexp, regexpi,}\cr
  \code{    regexpreg, rem, repmat, roots, rosser, rot90, rref, runge,}\cr
  \code{sec, secd, sech, semilogx, semilogy, sinc, sind, size, sortrows, sph2cart,}\cr
  \code{    sqrtm, squareform, std, str2num, strcat, strcmp, strcmpi,}\cr
  \code{    strfind, strfindi, strjust, subspace,}\cr
  \code{tand, tic, toc, trapz, tril, trimmean, triplequad, triu,}\cr
  \code{vander, vectorfield, ver,}\cr
  \code{what, who, whos, wilkinson,}\cr
  \code{zeros, zeta}

  The following Matlab function names have been capitalized in `pracma' to
  avoid shadowing functions from R base or one of its recommended packages
  (on request of Bill Venables and because of Brian Ripley's CRAN policies):

  \code{Diag, factos, finds, Fix, Imag, Lcm, Mode, Norm, nullspace (<- null)},\cr
  \code{Poly, Rank, Real, Reshape, strRep, strTrim, Toeplitz, Trace, uniq (<- unique).}

  To use ``ans'' instead of ``ans()'' -- as is common practice in Matlab --
  type (and similar for other Matlab commands):

  \code{makeActiveBinding("ans", function() .Last.value, .GlobalEnv)}\cr
  \code{makeActiveBinding("who", who(), .GlobalEnv)}
}
\author{
Hans Werner Borchers

Maintainer: Hans W Borchers  <hwborchers@googlemail.com>
}
\references{
  Abramowitz, M., and I. A. Stegun (1972). Handbook of Mathematical Functions
  (with Formulas, Graphs, and Mathematical Tables). Dover, New York.  
  URL: https://www.math.ubc.ca/~cbm/aands/notes.htm

  Arndt, J. (2010). Matters Computational: Ideas, Algorithms, Source Code.
  Springer-Verlag, Berlin Heidelberg Dordrecht.
  FXT: a library of algorithms: \url{https://www.jjj.de/fxt/}.

  Cormen, Th. H., Ch. E. Leiserson, and R. L. Rivest (2009). Introduction
  to Algorithms. Third Edition, The MIT Press, Cambridge, MA.

  Encyclopedia of Mathematics (2012). Editor-in-Chief: Ulf Rehmann.
  \url{https://encyclopediaofmath.org/wiki/Main_Page}.

  Gautschi, W. (1997). Numerical Analysis: An Introduction.
  Birkhaeuser, Boston.

  Gentle, J. E. (2009). Computational Statistics.
  Springer Science+Business Media LCC, New York.

  MathWorld.com (2011).
  Matlab Central: \url{https://www.mathworks.com/matlabcentral/}.

  NIST: National Institute of Standards and Technology.
  Olver, F. W. J., et al. (2010). NIST Handbook of Mathematical Functions.
  Cambridge University Press. 
  Internet: NIST Digital Library of Mathematical Functions, 
  \url{https://dlmf.nist.gov/};
  Guide to Available Mathematical Software, \url{https://gams.nist.gov/}.

  Press, W. H., S. A. Teukolsky, W. T Vetterling, and B. P. Flannery (2007).
  Numerical Recipes: The Art of Numerical Computing. Third Edition, incl.
  Numerical Recipes Software, Cambridge University Press, New York.
  URL: numerical.recipes/book/book.html.

  Quarteroni, A., R. Sacco, and F. Saleri (2007). Numerical Mathematics.
  Second Edition, Springer-Verlag, Berlin Heidelberg.

  Skiena, St. S. (2008). The Algorithm Design Manual. Second Edition,
  Springer-Verlag, London. The Stony Brook Algorithm Repository:
  \url{https://algorist.com/algorist.html}.

  Stoer, J., and R. Bulirsch (2002). Introduction to Numerical Analysis.
  Third Edition, Springer-Verlag, New York.

  Strang, G. (2007). Computational Science and Engineering.
  Wellesley-Cambridge Press.

  Weisstein, E. W. (2003). CRC Concise Encyclopedia of Mathematics.
  Second Edition, Chapman & Hall/CRC Press.
  Wolfram MathWorld: \url{https://mathworld.wolfram.com/}.  

  Zhang, S., and J. Jin (1996). Computation of Special Functions.
  John Wiley & Sons.
}
\seealso{
  The R package `matlab' contains some of the basic routines from Matlab,
  but unfortunately not any of the higher math routines.
}
\examples{
\dontrun{
##  See examples in the help files for all functions.
    }
}
\keyword{ package }
