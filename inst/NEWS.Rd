\name{NEWS}
\title{pracma News}
\encoding{UTF-8}


\section{Changes in Version 0.9.1 (2011-12-12)}{
    \itemize{
      \item plotyy() plots curves with y-axes on both left and right side.
      \item fplot() plots components of a multivariate function.
    }
}

\section{Changes in Version 0.9.0 (2011-12-11)}{
    \itemize{
      \item errorbar() routine for plotting error bars in both directions.
      \item Whittaker-Henderson smoothing  ** Not yet running** .
      \item rref() reduced row echelon form.
    }
}

\section{Changes in Version 0.8.9 (2011-12-08)}{
    \itemize{
      \item cutpoints() automatically finds cutting points based on gaps.
      \item hausdorff_dist calculates the Hausdorff distance / Hausdorff dimension.
      \item nnz() number of non-zeros elements (Matlab style).
    }
}

\section{Changes in Version 0.8.8 (2011-12-06)}{
    \itemize{
      \item polar() for polar plots (Matlab style), see the example plots.
      \item andrewsplot() plots Andrews curves in polar coordinates.
      \item Vectorized: cart2sph(), sph2cart(), cart2pol(), pol2cart().
    }
}

\section{Changes in Version 0.8.7 (2011-11-30)}{
    \itemize{
      \item deg2rad(), rad2deg()
      \item figure() Matlab style, and pltcross() plotting crosses.
    }
}

\section{Changes in Version 0.8.6 (2011-11-21)}{
    \itemize{
      \item ridder() Ridder's method for zero finding of univariate functions.
    }
}

\section{Changes in Version 0.8.5 (2011-11-19)}{
    \itemize{
      \item sqrtm() matrix square root, based on Denman-Beavers iteration,
        rootm() matrix p-th root, computing a complex contour integral,
        signm() matrix sign function.
      \item fzero() now uses the new zeroin() function,
	    i.e., a Brent-Dekker approach instead of refering to uniroot().
      \item twinPrimes() twin primes in a given interval, and \code{nextPrime}
        will find the next higher prime.
    }
}

\section{Changes in Version 0.8.4 (2011-11-14)}{
    \itemize{
      \item Transformations between cartesian, spherical, polar and cylindrical
        coordinate systems: cart2sph(), sph2cart(), cart2pol(), pol2cart().
      \item polar() uniformly random points in the unit circle (till Matlab 5).
    }
}

\section{Changes in Version 0.8.3 (2011-11-11)}{
    \itemize{
      \item accumarray() grouping elements and applying a function to each group.
      \item uniq() Matlab-style `unique' function, allsums() in the examples.
      \item small correction to fsolve(), mentioned on the `check summary' page.
    }
}

\section{Changes in Version 0.8.2 (2011-11-04)}{
    \itemize{
      \item newmark() Newmark's method for solving second order differential
        equations of the form  y''(t) = f(t, y(t), y'(t)) on [t1, t2].
      \item cranknic() Crank-Nicolson `ivp' solver, combining the forward and
      backward Euler methods for ordinary differential equations.
    }
}

\section{Changes in Version 0.8.1 (2011-10-30)}{
    \itemize{
      \item Corrected pinv() for (nearly) singular matrices.
      \item Renamed ifactor() to factorize().
    }
}

\section{Changes in Version 0.8.0 (2011-10-27)}{
    \itemize{
      \item Minor corrections and improvements to the `pracma.pdf' manual,
        incl. numdiff(), refindall(), trigApprox(), and subspace().
    }
}

\section{Changes in Version 0.7.9 (2011-10-22)}{
    \itemize{
      \item spinterp() monotonic (and later on shape-preserving) interpolation
        following the approach of Delbourgo and Gregory.
    }
}

\section{Changes in Version 0.7.8 (2011-10-17)}{
    \itemize{
      \item bvp() solves boundary value problems of the following kind:\cr
            -u''(x) + c1 u'(x) + c2 u(x) = f(x) for x in [a, b]. 
    }
}

\section{Changes in Version 0.7.7 (2011-10-14)}{
    \itemize{
      \item primes2(n1, n2) will return all prime numbers betweeen n1 and n2
        (without storing the numbers from sqrt(n2) up to n1).
    }
}

\section{Changes in Version 0.7.6 (2011-08-05)}{
    \itemize{
      \item gaussNewton() for function minimization and solving systems
        of nonlinear equations. fsolve() as a wrapper for it.
      \item fzsolve() for root finding of complex functions.
      \item softline() Fletcher's inexact linesearch algorithm.
    }
}

\section{Changes in Version 0.7.5 (2011-07-26)}{
    \itemize{
      \item Put NEWS.Rd in the /inst subdirectory (and NEWS.pdf in /doc),
            thanks to Kurt Hornik; slightly changed the version numbering.
    }
}

\section{Changes in Version 0.7.4 (2011-07-22)}{
    \itemize{
      \item rortho() generate random orthogonal matrix of size n.
      \item Titanium data set for testing fitting procedures.
    }
}

\section{Changes in Version 0.7.3 (2011-07-15)}{
    \itemize{
      \item erf() and erfc() error and complementary error functions
            (Matlab style) as (almost) aliases for pnorm().
      \item erfz() complex error function.
    }
}

\section{Changes in Version 0.7.2 (2011-07-11)}{
    \itemize{
      \item broyden() quasi-Newton root finding method for systems of
            nonlinear equations.
    }
}

\section{Changes in Version 0.7.1 (2011-07-09)}{
    \itemize{
      \item cross() has been vectorized (remark on R-help).
    }
}

\section{Changes in Version 0.7.0 (2011-07-07)}{
    \itemize{
      \item Sigmoid and Einstein functions.
    }
}

\section{Changes in Version 0.6.9 (2011-07-06)}{
    \itemize{
      \item Runge-Kutta-Fehlberg method of order (5,4).
    }
}

\section{Changes in Version 0.6.8 (2011-07-05)}{
    \itemize{
      \item triquad() Gaussian quadrature over triangles.
      \item cotes() Newton-Cotes integration formulae for 2 to 8 nodes.
    }
}

\section{Changes in Version 0.6.7 (2011-07-04)}{
    \itemize{
      \item lagrangeInterp(), newtonInterp() Lagrange and Newton polynomial
        interpolation, neville() Neville's methods.
      \item tril(), triu() extracting triangular matrices (Matlab style).
    }
}

\section{Changes in Version 0.6.6 (2011-07-02)}{
    \itemize{
      \item charpoly() computes the characteristic polynomial, the determinant,
        and the inverse for matrices that are relativly small, applying the
        Faddejew-Leverrier method.
      \item froots() to find *all* roots (also of second or higher order) of
          a univariate function in a given interval. The same with fmins()
          to find all minima.
    }
}

\section{Changes in Version 0.6.5 (2011-07-01)}{
    \itemize{
      \item Adams-Bashford and Adams-Moulton (i.e., multi-step) methods
        for ordinary differential equations in function abm3pc().
    }
}

\section{Changes in Version 0.6.4 (2011-06-30)}{
    \itemize{
      \item Changed the description to be more precise about the package.
      \item droplet_e() generation of digits for the Euler number.
        (Should be followed by a function droplet_pi().)
    }
}

\section{Changes in Version 0.6.3 (2011-06-28)}{
    \itemize{
      \item rationalfit() rational function approximation
      \item ratinterp() rational interpolation a la Burlisch-Stoer.
    }
}

\section{Changes in Version 0.6.2 (2011-06-26)}{
    \itemize{
      \item pade() Pade approximation.
    }
}

\section{Changes in Version 0.6.1 (2011-06-25)}{
    \itemize{
      \item quadgk() adaptive Gauss-Kronrod quadrature.
    }
}

\section{Changes in Version 0.6.0 (2011-06-24)}{
    \itemize{
      \item muller() Muller's root finding method.
      \item Added differential equation example to expm()'s help page.
      \item Changed NEWS file to become simpler (no subsections).
    }
}

\section{Changes in Version 0.5.9 (2011-06-23)}{
    \itemize{
      \item quadl() recursive adaptive Gauss-Lobatto quadrature.
      \item simpadpt() another recursively adaptive Simpson's rule.
      \item Added testing procedures for all integration routines;
        corrected, refined some of these procedures.
    }
}

\section{Changes in Version 0.5.8 (2011-06-20)}{
    \itemize{
      \item quadgr() Gaussian Quadrature with Richardson extrapolation, can
        handle singularities at endpoints and (half-)infinite intervals.
    }
}

\section{Changes in Version 0.5.7 (2011-06-18)}{
    \itemize{
      \item expm() for matrix exponentials.
      \item clenshaw_curtis() the Clenshaw-Curtis quadrature formula.
    }
}

\section{Changes in Version 0.5.6 (2011-06-17)}{
    \itemize{
      \item simpson2d() as non-adaptive 2-dimensional Simpson integration.
      \item dblquad() twofold application of internal function integrate().
    }
}

\section{Changes in Version 0.5.5 (2011-06-15)}{
    \itemize{
      \item gaussHermite() and gaussLaguerre() for infinite intervals.
      \item Fresnel integrals fresnelS() and frenelC().
    }
}

\section{Changes in Version 0.5.4 (2011-06-12)}{
    \itemize{
      \item gaussLegendre() computes coefficients for Gauss Quadrature,
        and quad2d() uses these weights for 2-dimensional integration.
      \item quadinf() wrapper for integrate() on infinite intervals.
      \item Added a version for rapid pi computation to the agm() examples.
    }
}

\section{Changes in Version 0.5.3 (2011-06-06)}{
    \itemize{
      \item ode23() solving first order (systems of) differential equations.
      \item barylag2d() 2-dimensional barycentric Lagrange interpolation.
    }
}

\section{Changes in Version 0.5.2 (2011-06-04)}{
    \itemize{
      \item interp2() for two-dimensional interpolation.
      \item gradient() now works in two dimensions too.
    }
}

\section{Changes in Version 0.5.1 (2011-06-01)}{
    \itemize{
      \item fzero(), fminbnd(), fminsearch(), fsolve() as aliases for
        uniroot(), optimize(), optim() with Nelder-Mead, newtonsys().
    }
}

\section{Changes in Version 0.5.0 (2011-05-31)}{
    \itemize{
      \item Corrections to help pages.
    }
}

\section{Changes in Version 0.4.9 (2011-05-30)}{
    \itemize{
      \item romberg() and gauss_kronrod() for numerical integration.
      \item Richardson's extrapolation in numderiv(), numdiff().
      \item Discrete numerical derivatives (one dimension): gradient().
    }
}

\section{Changes in Version 0.4.8 (2011-05-28)}{
    \itemize{
      \item Numerical function derivatives: fderiv(), grad().
      \item Specialized operators: hessian(), laplacian().
      \item Application: taylor().
    }
}

\section{Changes in Version 0.4.7 (2011-05-27)}{
    \itemize{
      \item plot vector fields: quiver() and vectorfield().
      \item findintervals().
      \item Corrections in deval(), deeve(), using findintervals().
    }
}

\section{Changes in Version 0.4.6 (2011-05-26)}{
    \itemize{
      \item Laguerre's method laguerre().
      \item rk4() and rk4sys() classical fourth order Runge-Kutta.
      \item deval(), deeve() evaluate ODE solutions.
    }
}

\section{Changes in Version 0.4.5 (2011-05-24)}{
    \itemize{
      \item Lebesgue coefficient: lebesgue().
      \item poly2str() for string representation of a polynomial.
    }
}

\section{Changes in Version 0.4.4 (2001-05-23)}{
    \itemize{
      \item Dirichlet's eta() and Riemann's zeta() function.
      \item rmserr() different accuracy measures; std_err() standard error.
    }
}

\section{Changes in Version 0.4.3 (2001-05-22)}{
    \itemize{
      \item polypow() and polytrans() for polynomials.
      \item polyApprox() polynomial approximation using Chebyshev.
      \item trigPoly(), trigApprox() for trigonometric regression.
    }
}

\section{Changes in Version 0.4.2 (2001-05-17)}{
    \itemize{
      \item segm_intersect() and segm_distance() segment distances.
      \item inpolygon().
    }
}

\section{Changes in Version 0.4.1 (2011-05-13)}{
    \itemize{
      \item polyadd() polynomial addition.
      \item conv() and deconv() time series (de)convolution.
      \item detrend() removes (piecewise) linear trends.
      \item ifft() for normalized inverse Fast Fourier Transform.
    }
}

\section{Changes in Version 0.4.0 (2011-05-10)}{
    \itemize{
      \item Added tests for functions since version 0.3-7.
    }
}

\section{Changes in Version 0.3.9 (2011-05-09)}{
    \itemize{
      \item and() and or().
    }
}

\section{Changes in Version 0.3.8 (2011-05-06)}{
    \itemize{
      \item pchip() and option `cubic' for interp1() interpolation.
      \item The complex gamma functions gammaz().
      \item hadamard() and toeplitz() matrices.
    }
}

\section{Changes in Version 0.3.7 (2011-05-04)}{
    \itemize{
      \item Rank of a matrix, mrank(), and nullspace() for the kernel.
      \item orth(), orthogonal basis of the image space, and subspace()
        determines the angle between two subspaces.
      \item normest() for estimating the (Frobenius) norm of a matrix, and
        cond() determines the condition number of a matrix.
    }
}

\section{Changes in Version 0.3.6 (2011-04-30)}{
    \itemize{
      \item fact(), more accurate than the R internal function `factorial'.
      \item ezplot() as an alias for curve(), but with option ``fill = TRUE''.
      \item aitken() for accelerating iterations.
      \item Renamed polycnv() to polymul().
      \item Renamed outlierMAD() to hampel().
    }
}

\section{Changes in Version 0.3.5 (2011-04-23)}{
    \itemize{
      \item agm() for the arithmetic-geometric mean.
      \item Lambert W function lambertWp() for the real principal branch.
      \item ``Complex Step'' derivation with complexstep() and complexstepJ().
    }
}

\section{Changes in Version 0.3.4 (2011-04-21)}{
    \itemize{
      \item Barycentric Lagrange interpolation through barylag().
      \item polyfit2() fits a polynomial that exactly meets one additional
        point.
      \item Added more references to the help entry `pracma-package.Rd'.
    }
}

\section{Changes in Version 0.3.3  (2011-04-19)}{
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

\section{Changes in Version 0.3.2 (2011-04-17)}{
    \itemize{
      \item Sorting functions: bubbleSort(), insertionSort(), selectionSort(),
        shellSort(), heapSort(), mergeSort(), mergeOrdered(), quickSort(),
        quickSortx(), is.sorted(), and testSort().
      \item Functions from number theory: eulersPhi(), moebiusFun() and the 
        mertensFun(), sigma(), tau(), omega(), and Omega().
    }
}

\section{Changes in Version 0.3.1 (2011-04-16)}{
    \itemize{
      \item Chebyshev polynomials of the first kind: chebPoly(),
        chebCoeff(),and chebApprox().
    }
}

\section{Changes in Version 0.3.0 (2011-04-09)}{
    \itemize{
      \item New version of news.Rd, news.pdf.
      \item More test functions for root finding and quadrature.
    }
}

\section{Changes in Version 0.2.9}{
    \itemize{
      \item fnorm() and the Runge function runge().
      \item contfrac(), rat(), and rats() for continuous fractions.
      \item meshgrid() and magic().
    }
}

\section{Changes in Version 0.2.8}{
    \itemize{
      \item quad() adaptive Simpson quadrature.
      \item Minimum finding with fibsearch() and golden_ratio().
      \item Root finding with newton(), secant(), and brentDekker().
    }
}

\section{Changes in Version 0.2.7}{
    \itemize{
      \item Regular expression functions regexp(), regexpi(), regexprep()
      and refindall().
    }
}

\section{Changes in Version 0.2.6}{
    \itemize{
      \item String functions blanks(), strtrim(), deblank(), strjust(),
      and strrep().
      \item interp1() one-dimensional interpolation (incl. spline)
    }
}

\section{Changes in Version 0.2.5}{
    \itemize{
      \item Matlab functions mode(), clear() and beep().
    }
}

\section{Changes in Version 0.2.4}{
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

\section{Changes in Version 0.2.3}{
    \itemize{
      \item strfind(), strfindi(), and findstr().
      \item circlefit() fitting a circle to plane points.
      \item mldivide() and mrdivide(), emulating the Matlab backslash operator.
    }
}

\section{Changes in Version 0.2.2}{
    \itemize{
      \item vnorm() vector norm
      \item Warning about a nasty ``non-ASCII input'' in the savgol.RD file
      has been resolved.
    }
}

\section{Changes in Version 0.2.1}{
    \itemize{
      \item horner() implementing the horner scheme for evaluating
      a polynomial and its derivative.
      \item savgol() Savitzki-Golay smoothing and needed pseudoinverse pinv().
    }
}

\section{Changes in Version 0.2.0}{
    \itemize{
      \item Package renamed to `pracma' to avoid name clashes with packages
      such as `matlab' that are sticking closer to the original.
      \item Added `pracma-package' section to the manual.
    }
}

\section{Changes in Version 0.1.9}{
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

\section{Changes in Version 0.1.8}{
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

\section{Changes in Version 0.1.7}{
    \itemize{
      \item size(), numel(), ndims(), isempty(), and find().
      \item eye(), ones(), zeros().
      \item Functions returning random numbers: rand(), randn(), randi().
      \item linspace(), logspace(), and logseq() for linearly, logarithmically,
        and exponentially spaced sequences.\cr
        Note that the functions in the `matlab' package are not exactly
        mimicking the corresponding Matlab/Octave functions.
    }
}

\section{Changes in Version 0.1.6}{
    \itemize{
      \item Matrix functions mdiag() and mtrace() added. inv() is introduced
      as an alias for solve() in R.
      \item Generate special matrices hankel(), rosser(), and wilkinson().
      kron() is an alias for the R function kronecker().
      \item Renamed factors() to ifactor() to distiguish it more clearly from
      factors as used in R.
    }
}

\section{Changes in Version 0.1.5}{
    \itemize{
      \item Added functions for flipping or rotating numeric and complex
        matrices: flipdim(), flipud(), fliplr(), and rot90().
    }
}

\section{Changes in Version 0.1.4}{
    \itemize{
      \item Added basic complex functions real(), imag(), conj(), and angle()
      which are essentially only aliases of the R functions Re(), Im(), 
      and Conj().\cr
      angle() returns the angle of a complex number in radians.
      The \R function Mod() is here only available as abs().
    }
}

\section{Changes in Version 0.1.3}{
    \itemize{
      \item Added compan() function for the `companion' matrix; the eig() 
      function is an alias for the R eigen()values function.
      \item Added the polynomial functions poly(), polyder(), polyfit(),
      polyint(), and polyval().
      \item roots() returns real and complex roots of polynomials.
      \item Simplified the trapz() function.
    }
}

\section{Changes in Version 0.1.2}{
    \itemize{
      \item Added functions from number theory: primes(), isprime() and 
      factors().
      \item The corresponding function for factors() in Matlab/Octave is
      called factor(), but that name should not be shadowed in R!
      \item Added the polyarea() and trapz() functions.
    }
}

\section{Changes in Version 0.1.1}{
    \itemize{
      \item Added some simple functions such as nthroot(), pow2(), and 
      nextpow2().
      \item dot() and cross() functions for scalar and vector product.
      \item Generate matrices through vander() and hilb().
    }
}

\section{Changes in Version 0.1.0}{
    \itemize{
      \item Installation

      `pracma' will be a pure R package without using source code in C or
      Fortran. Therefore, installation will be immediate on all platforms.

      \item Intention

      This package provides R implementations of more advanced math
      functions from Matlab and Octave (and the Euler Math Toolbox)
      with a special view on optimization and time series routines.

      \item Remark: Typeset this document as:

      \code{R CMD Rd2pdf NEWS.Rd --title="NEWS for pracma version 0.9.1"}.
    }
}
