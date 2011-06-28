\name{NEWS}
\title{pracma News}
\encoding{UTF-8}

\section{Changes in Version 0.6-1 (2011-06-25)}{
    \itemize{
      \item Changed gauss_kronrod() and clenshaw_curtis() to become adaptive
        integration routines.
    }
}

\section{Changes in Version 0.6-0 (2011-06-24)}{
    \itemize{
      \item muller() Muller's root finding method.

      \item Added differential equation example to expm()'s help page.

      \item Changed NEWS file to become simpler (no subsections);
        updated the NEWS.Rd, NEWS.pdf files.
    }
}

\section{Changes in Version 0.5-9 (2011-06-23)}{
    \itemize{
      \item quadl() recursive adaptive Gauss-Lobatto quadrature.

      \item simpadpt() another recursively adaptive Simpson's rule.

      \item Added testing procedures for all integration routines;
        corrected, refined some of these procedures.
    }
}

\section{Changes in Version 0.5-8 (2011-06-20)}{
    \itemize{
      \item quadgr() Gaussian Quadrature with Richardson extrapolation, can
        handle singularities at endpoints and (half-)infinite intervals.
    }
}

\section{Changes in Version 0.5-7 (2011-06-18)}{
    \itemize{
      \item expm() for matrix exponentials.

      \item clenshaw_curtis() the Clenshaw-Curtis quadrature formula.
    }
}

\section{Changes in Version 0.5-6 (2011-06-17)}{
    \itemize{
      \item simpson2d() as non-adaptive 2-dimensional Simpson integration.

      \item dblquad() twofold application of internal function integrate().
    }
}

\section{Changes in Version 0.5-5 (2011-06-15)}{
    \itemize{
      \item gaussHermite() and gaussLaguerre() for infinite intervals.

      \item Fresnel integrals fresnelS() and frenelC().
    }
}

\section{Changes in Version 0.5-4 (2011-06-12)}{
    \itemize{
      \item gaussLegendre() computes coefficients for Gauss Quadrature,
        and quad2d() uses these weights for 2-dimensional integration.

      \item quadinf() wrapper for integrate() on infinite intervals.

      \item Added a version for rapid pi computation to the agm() examples.
    }
}

\section{Changes in Version 0.5-3 (2011-06-06)}{
    \itemize{
      \item ode23() solving first order (systems of) differential equations.

      \item barylag2d() 2-dimensional barycentric Lagrange interpolation.
    }
}

\section{Changes in Version 0.5-2 (2011-06-04)}{
    \itemize{
      \item interp2() for two-dimensional interpolation.

      \item gradient() now works in two dimensions too.
    }
}

\section{Changes in Version 0.5-1 (2011-06-01)}{
    \itemize{
      \item fzero(), fminbnd(), fminsearch(), fsolve() as aliases for
        uniroot(), optimize(), optim() with Nelder-Mead, newtonsys().
    }
}

\section{Changes in Version 0.5-0 (2011-05-31)}{
    \itemize{
      \item Corrections to help pages.
    }
}

\section{Changes in Version 0.4-9 (2011-05-30)}{
    \itemize{
      \item romberg() and gauss_kronrod() for numerical integration.

      \item Richardson's extrapolation in numderiv(), numdiff().

      \item Discrete numerical derivatives (one dimension): gradient().
    }
}

\section{Changes in Version 0.4-8 (2011-05-28)}{
    \itemize{
      \item Numerical function derivatives: fderiv(), grad().

      \item Specialized operators: hessian(), laplacian().

      \item Application: taylor().
    }
}

\section{Changes in Version 0.4-7 (2011-05-27)}{
    \itemize{
      \item plot vector fields: quiver() and vectorfield().

      \item findintervals().

      \item Corrections in deval(), deeve(), using findintervals().
    }
}

\section{Changes in Version 0.4-6 (2011-05-26)}{
    \itemize{
      \item Laguerre's method laguerre().

      \item rk4() and rk4sys() classical fourth order Runge-Kutta.

      \item deval(), deeve() evaluate ODE solutions.
    }
}

\section{Changes in Version 0.4-5 (2011-05-24)}{
    \itemize{
      \item Lebesgue coefficient: lebesgue().

      \item poly2str() for string representation of a polynomial.
    }
}

\section{Changes in Version 0.4-4 (2001-05-23)}{
    \itemize{
      \item Dirichlet's eta() and Riemann's zeta() function.

      \item rmserr() different accuracy measures; std_err() standard error.
    }
}

\section{Changes in Version 0.4-3 (2001-05-22)}{
    \itemize{
      \item polypow() and polytrans() for polynomials.

      \item polyApprox() polynomial approximation using Chebyshev.

      \item trigPoly(), trigApprox() for trigonometric regression.
    }
}

\section{Changes in Version 0.4-2 (2001-05-17)}{
    \itemize{
      \item segm_intersect() and segm_distance() segment distances.

      \item inpolygon().
    }
}

\section{Changes in Version 0.4-1 (2011-05-13)}{
    \itemize{
      \item polyadd() polynomial addition.

      \item conv() and deconv() time series (de)convolution.

      \item detrend() removes (piecewise) linear trends.

      \item ifft() for normalized inverse Fast Fourier Transform.
    }
}

\section{Changes in Version 0.4-0 (2011-05-10)}{
    \itemize{
      \item Added tests for functions since version 0.3-7.
    }
}

\section{Changes in Version 0.3-9 (2011-05-09)}{
    \itemize{
      \item and() and or().
    }
}

\section{Changes in Version 0.3-8 (2011-05-06)}{
    \itemize{
      \item pchip() and option `cubic' for interp1() interpolation.

      \item The complex gamma functions gammaz().

      \item hadamard() and toeplitz() matrices.
    }
}

\section{Changes in Version 0.3-7 (2011-05-04)}{
    \itemize{
      \item Rank of a matrix, mrank(), and nullspace() for the kernel.

      \item orth(), orthogonal basis of the image space, and subspace()
        determines the angle between two subspaces.

      \item normest() for estimating the (Frobenius) norm of a matrix, and
        cond() determines the condition number of a matrix.
    }
}

\section{Changes in Version 0.3-6 (2011-04-30)}{
    \itemize{
      \item fact(), more accurate than the R internal function `factorial'.

      \item ezplot() as an alias for curve(), but with option ``fill = TRUE''.

      \item aitken() for accelerating iterations.

      \item Renamed polycnv() to polymul().

      \item Renamed outlierMAD() to hampel().
    }
}

\section{Changes in Version 0.3-5 (2011-04-23)}{
    \itemize{
      \item agm() for the arithmetic-geometric mean.

      \item Lambert W function lambertWp() for the real principal branch.

      \item ``Complex Step'' derivation with complexstep() and jacobiancsd().
    }
}

\section{Changes in Version 0.3-4 (2011-04-21)}{
    \itemize{
      \item Barycentric Lagrange interpolation through barylag().

      \item polyfit2() fits a polynomial that exactly meets one additional
        point.

      \item Added more references to the help entry `pracma-package.Rd'.
    }
}

\section{Changes in Version 0.3-3  (2011-04-19)}{
    \itemize{
      \item hornerdefl() for also returning the deflated polynomial.

      \item newtonHorner() combining Newton's method and the Horner scheme
        for root finding for polynomials.

      \item jacobian() computes the Jacobian of a function R^n --> R^m
        as simple numerical derivative.

      \item newtonsys() applies Newton's method to functions R^n --> R^n
        with special application to root finding of complex functions.

      \item newton() renamed to newtonRaphson().
    }
}

\section{Changes in Version 0.3-2 (2011-04-17)}{
    \itemize{
      \item Sorting functions: bubbleSort(), insertionSort(), selectionSort(),
        shellSort(), heapSort(), mergeSort(), mergeOrdered(), quickSort(),
        quickSortx(), is.sorted(), and testSort().

      \item Functions from number theory: eulersPhi(), moebiusFun() and the 
        mertensFun(), sigma(), tau(), omega(), and Omega().
    }
}

\section{Changes in Version 0.3-1 (2011-04-16)}{
    \itemize{
      \item Chebyshev polynomials of the first kind: chebPoly(),
        chebCoeff(),and chebApprox().
    }
}

\section{Changes in Version 0.3-0 (2011-04-09)}{
    \itemize{
      \item New version of news.Rd, news.pdf.

      \item More test functions for root finding and quadrature.
    }
}

\section{Changes in Version 0.2-9}{
    \itemize{
      \item fnorm() and the Runge function runge().

      \item contfrac(), rat(), and rats() for continuous fractions.

      \item meshgrid() and magic().
    }
}

\section{Changes in Version 0.2-8}{
    \itemize{
      \item quad() adaptive Simpson quadrature.

      \item Minimum finding with fibsearch() and golden_ratio().

      \item Root finding with newton(), secant(), and brentDekker().
    }
}

\section{Changes in Version 0.2-7}{
    \itemize{
      \item Regular expression functions regexp(), regexpi(), regexprep()
      and refindall().
    }
}

\section{Changes in Version 0.2-6}{
    \itemize{
      \item String functions blanks(), strtrim(), deblank(), strjust(),
      and strrep().

      \item interp1() one-dimensional interpolation (incl. spline)
    }
}

\section{Changes in Version 0.2-5}{
    \itemize{
      \item Matlab functions mode(), clear() and beep().
    }
}

\section{Changes in Version 0.2-4}{
    \itemize{
      \item primroot() finds the smallest primitive root modulo a given n;
      needed functions are modpower() and modorder().

      \item humps() and sinc(): Matlab test functions.

      \item Root finding through bisection: bisect(), regulaFalsi().

      \item outlierMAD(), findpeaks(), and piecewise().

      \item polycnv() for polynomial multiplication.

      \item Functions extgcd(), gcd(), and lcm() have been renamed to
      extGCD(), GCD(), and LCM() respectively.
    }
}

\section{Changes in Version 0.2-3}{
    \itemize{
      \item strfind(), strfindi(), and findstr().

      \item circlefit() fitting a circle to plane points.

      \item mldivide() and mrdivide(), emulating the Matlab backslash operator.
    }
}

\section{Changes in Version 0.2-2}{
    \itemize{
      \item vnorm() vector norm

      \item Warning about a nasty ``non-ASCII input'' in the savgol.RD file
      has been resolved.
    }
}

\section{Changes in Version 0.2-1}{
    \itemize{
      \item horner() implementing the horner scheme for evaluating
      a polynomial and its derivative.

      \item savgol() Savitzki-Golay smoothing and needed pseudoinverse pinv().
    }
}

\section{Restarted as Version 0.2-0}{
    \itemize{
      \item Package renamed to `pracma' to avoid name clashes with packages
      such as `matlab' that are sticking closer to the original.

      \item Added `pracma-package' section to the manual.
    }
}

\section{Changes in Version 0.1-9}{
    \itemize{
      \item reshape(), repmat(), and blkdiag() matrix functions.

      \item combs() chooses all combinations of k elements out of n, and
      randcomb() generates a random selection.

      \item perms() generates all permutations, randperm() a random
      permutation.

      \item Pascal triangle as pascal(); nchoosek() returns binomial
      coefficients.

      \item Some string functions: strcmp(), strcmpi(), strcat().
    }
}

\section{Changes in Version 0.1-8}{
    \itemize{
      \item std() as refinement of the standard deviation function.

      \item ceil() and fix() as aliases for ceiling() and trunc().
      [floor() and round() already exist in R.]

      \item Modulo functions mod(), rem() and integer division idiv().

      \item Integer functions related to the Euclidean algorithm:
      extgcd(), gcd(), lcm(), coprime(), and modinv().

      \item distmat() and crossn(), the vector product in n-dimensional space.
    }
}

\section{Changes in Version 0.1-7}{
    \itemize{
      \item size(), numel(), ndims(), isempty(), and find().

      \item eye(), ones(), zeros().

      \item Functions returning random numbers: rand(), randn(), randi().

      \item linspace(), logspace(), and logseq() for linearly, logarithmically,
      and exponentially spaced sequences.

      Note that the functions in the `matlab' package are not exactly
      mimicking the corresponding Matlab/Octave functions.
    }
}

\section{Changes in Version 0.1-6}{
    \itemize{
      \item Matrix functions mdiag() and mtrace() added. inv() is introduced
      as an alias for solve() in R.

      \item Generate special matrices hankel(), rosser(), and wilkinson().
      kron() is an alias for the R function kronecker().

      \item Renamed factors() to ifactor() to distiguish it more clearly from
      factors as used in R.
    }
}

\section{Changes in Version 0.1-5}{
    \itemize{
      \item Added functions for flipping or rotating numeric and complex
        matrices: flipdim(), flipud(), fliplr(), and rot90().
    }
}

\section{Changes in Version 0.1-4}{
    \itemize{
      \item Added basic complex functions real(), imag(), conj(), and angle()
      which are essentially only aliases of the R functions Re(), Im(), 
      and Conj().

      angle() returns the angle of a complex number in radians.
      The \R function Mod() is here only available as abs().
    }
}

\section{Changes in Version 0.1-3}{
    \itemize{
      \item Added compan() function for the `companion' matrix; the eig() 
      function is an alias for the R eigen()values function.

      \item Added the polynomial functions poly(), polyder(), polyfit(),
      polyint(), and polyval().
      
      \item roots() returns real and complex roots of polynomials.

      \item Simplified the trapz() function.
    }
}

\section{Changes in Version 0.1-2}{
    \itemize{
      \item Added functions from number theory: primes(), isprime() and 
      factors().

      \item The corresponding function for factors() in Matlab/Octave is
      called factor(), but that name should not be shadowed in R!

      \item Added the polyarea() and trapz() functions.
    }
}

\section{Changes in Version 0.1-1}{
    \itemize{
      \item Added some simple functions such as nthroot(), pow2(), and 
      nextpow2().

      \item dot() and cross() functions for scalar and vector product.

      \item Generate matrices through vander() and hilb().
    }
}

\section{INITIAL VERSION 0.1-0}{
  \subsection{INSTALLATION}{
    `matlab4r' will be a pure R package without using any source code.
    Therefore, installation will be immediate on all platforms.
  }

  \subsection{INTENTION}{
    This package provides R implementations of more advanced math
    functions from Matlab and Octave (and the Euler Math Toolbox)
    with a special view on optimization and time series routines.
  }

  \subsection{Remark}{
    Typeset this document as:

    \code{R CMD Rd2pdf NEWS.Rd --title="NEWS for pracma version 0.6-0"}.
  }
}
