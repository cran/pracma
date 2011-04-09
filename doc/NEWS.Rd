\name{NEWS}
\title{pracma News}
\encoding{UTF-8}

\section{CHANGES IN VERSION 0.3-0}{
  \subsection{OTHER CHANGES}{
    \itemize{
      \item New version of news.Rd, news.pdf.

      \item More test functions for root finding and quadrature.
    }
  }
}

\section{CHANGES IN VERSION 0.2-9}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item fnorm() and the Runge function runge().

      \item contfrac(), rat(), and rats() for continuous fractions.

      \item meshgrid() and magic().
    }
  }
}

\section{CHANGES IN VERSION 0.2-8}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item quad() adaptive Simpson quadrature.

      \item Minimum finding with fibsearch() and golden_ratio().

      \item Root finding with newton(), secant(), and brentDekker().
    }
  }
}

\section{CHANGES IN VERSION 0.2-7}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Regular expression functions regexp(), regexpi(), regexprep()
      and refindall().
    }
  }
}

\section{CHANGES IN VERSION 0.2-6}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item String functions blanks(), strtrim(), deblank(), strjust(),
      and strrep().

      \item interp1() one-dimensional interpolation (incl. spline)
    }
  }
}

\section{CHANGES IN VERSION 0.2-5}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Matlab functions mode(), clear() and beep().
    }
  }
}

\section{CHANGES IN VERSION 0.2-4}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item primroot() finds the smallest primitive root modulo a given n;
      needed functions are modpower() and modorder().

      \item humps() and sinc(): Matlab test functions.

      \item Root finding through bisection: bisect(), regulaFalsi().

      \item outlierMAD(), findpeaks(), and piecewise().

      \item polycnv() for polynomial multiplication.
    }
  }
  \subsection{OTHER CHANGES}{
    \itemize{
      \item Functions extgcd(), gcd(), and lcm() have been renamed to
      extGCD(), GCD(), and LCM() respectively.
    }
  }
}

\section{CHANGES IN VERSION 0.2-3}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item strfind(), strfindi(), and findstr().

      \item circlefit() fitting a circle to plane points.

      \item mldivide() and mrdivide(), emulating the Matlab backslash operator.
    }
  }
}

\section{CHANGES IN VERSION 0.2-2}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item vnorm() vector norm
    }
  }
  \subsection{CORRECTIONS}{
    \itemize{
      \item Warning about a nasty ``non-ASCII input'' in the savgol.RD file
      has been resolved.
    }
  }
}

\section{CHANGES IN VERSION 0.2-1}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item horner() implementing the horner scheme for evaluating
      a polynomial and its derivative.

      \item savgol() Savitzki-Golay smoothing and needed pseudoinverse pinv().
    }
  }
}

\section{RESTARTED AS VERSION 0.2-0}{
  \subsection{NAME CHANGE}{
    \itemize{
      \item Package renamed to `pracma' to avoid name clashes with packages
      such as `matlab' that are sticking closer to the original.
    }
  }
  \subsection{OTHER CHANGES}{
    \itemize{
      \item Added `pracma-package' section to the manual.
    }
  }
}

\section{CHANGES IN VERSION 0.1-9}{
  \subsection{NEW FUNCTIONS}{
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
}

\section{CHANGES IN VERSION 0.1-8}{
  \subsection{NEW FUNCTIONS}{
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
}

\section{CHANGES IN VERSION 0.1-7}{
  \subsection{NEW FUNCTIONS}{
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
}

\section{CHANGES IN VERSION 0.1-6}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Matrix functions mdiag() and mtrace() added. inv() is introduced
      as an alias for solve() in R.

      \item Generate special matrices hankel(), rosser(), and wilkinson().
      kron() is an alias for the R function kronecker().
    }
  }
  \subsection{OTHER CHANGES}{
    \itemize{
      \item Renamed factors() to ifactor() to distiguish it more clearly from
      factors as used in R.
    }
  }
}

\section{CHANGES IN VERSION 0.1-5}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Added functions for flipping or rotating numeric and complex
        matrices: flipdim(), flipud(), fliplr(), and rot90().
    }
  }
}

\section{CHANGES IN VERSION 0.1-4}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Added basic complex functions real(), imag(), conj(), and angle()
      which are essentially only aliases of the R functions Re(), Im(), 
      and Conj().

      angle() returns the angle of a complex number in radians.
      The \R function Mod() is here only available as abs().
    }
  }
}

\section{CHANGES IN VERSION 0.1-3}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Added compan() function for the `companion' matrix; the eig() 
      function is an alias for the R eigen()values function.

      \item Added the polynomial functions poly(), polyder(), polyfit(),
      polyint(), and polyval().
      
      \item roots() returns real and complex roots of polynomials.
    }
  }
  \subsection{OTHER CHANGES}{
    \itemize{
      \item Simplified the trapz() function.
    }
  }
}

\section{CHANGES IN VERSION 0.1-2}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Added functions from number theory: primes(), isprime() and 
      factors().

      \item The corresponding function for factors() in Matlab/Octave is
      called factor(), but that name should not be shadowed in R!

      \item Added the polyarea() and trapz() functions.
    }
  }
}

\section{CHANGES IN VERSION 0.1-1}{
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Added some simple functions such as nthroot(), pow2(), and 
      nextpow2().

      \item dot() and cross() functions for scalar and vector product.

      \item Generate matrices through vander() and hilb().
    }
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

    \code{R CMD Rd2pdf NEWS.Rd --title="NEWS for pracma version 0.3-0"}.
  }
}
