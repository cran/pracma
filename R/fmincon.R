##
##  f m i n c o n . R
##


fmincon <- function(x0, fn, gr = NULL, ..., method = "SQP",
                    A = NULL, b = NULL, Aeq = NULL, beq = NULL,
                    lb = NULL, ub = NULL, hin = NULL, heq = NULL,
                    tol = 1e-06, maxfeval = 10000, maxiter = 5000) {

    if (method != "SQP")
        stop("Methods other than 'SQP' are not yet implemented.")
    if (!is.numeric(x0) || length(x0) <= 1)
        stop("'x0' must be a numeric vector of length greater 1.")
    if (!is.null(gr))
        warning("Gradient function is not used for SQP approach.")

    if (!requireNamespace("NlcOptim", quietly =TRUE))
        stop("Package 'NlcOptim' missing -- install from CRAN.")
    if (!requireNamespace("quadprog", quietly =TRUE))
        stop("Package 'quadprog' missing -- install from CRAN.")

    fun <- match.fun(fn)
    fn  <- function(x) fun(x, ...)

    if (!is.null(A)) {
        if (!is.matrix(A) || ncol(A) != length(x0))
            stop("Argument 'A' must be a matrix with length(x0) columns.")
        if (is.null(b) || nrow(A) != length(b))
            stop("Argument 'b' must be a vector of length(b) = nrow(A).")
    }
    if (!is.null(Aeq)) {
        if (!is.matrix(Aeq) || ncol(Aeq) != length(x0))
            stop("Argument 'Aeq' must be a matrix with length(x0) columns.")
        if (is.null(beq) || nrow(Aeq) != length(beq))
            stop("Argument 'beq' must be a vector of length(beq) = nrow(Aeq).")
    }
    if (!is.null(lb) && length(lb) != length(x0)) {
        if (length(lb == 1)) lb <- rep(lb, length(x0))
        else stop("Length of argument 'lb' must be equal to length(x0).")
    }
    if (!is.null(ub) && length(ub) != length(x0)) {
        if (length(ub == 1)) ub <- rep(ub, length(x0))
        else stop("Length of argument 'ub' must be equal to length(x0).")
    }
    
    if (is.null(hin) && is.null(heq)) { confun = NULL
    } else if (is.null(heq)) {
        confun <- function(x) list(ceq = NULL, c = hin(x))
    } else if (is.null(hin)) {
        confun <- function(x) list(ceq = heq(x), c = NULL)
    } else
        confun <- function(x) list(ceq = heq(x), c = hin(x))

    sol <- NlcOptim::solnl(X = x0, objfun = fn, confun = confun,
                           A = A, B = b, Aeq = Aeq, Beq = beq,
                           lb = lb, ub = ub,
                           tolX = tol, tolFun = 0.1*tol, tolCon = 0.1*tol)
    ans <- list(par = c(sol$par), value = sol$fn, convergence = 0,
                info = list(lambda = sol$lambda, grad = sol$grad,
                            hessian = sol$hessian))
    return(ans)
}

