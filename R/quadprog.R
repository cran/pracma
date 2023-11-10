##
##  q u a d p r o g . R  Quadratic Programming
##


quadprog <- function(
    C, d,                     # min! 1/2 x'Cx + d'x
    A = NULL,   b = NULL,     # A x <= b
    Aeq = NULL, beq = NULL,   # Aeq x == beq
    lb = NULL,  ub = NULL)    # lb <= x <= ub
{
    if (!requireNamespace("quadprog", quietly = TRUE)) {
        stop("quadprog needed for this function to work. Please install it.",
              call. = FALSE)
    }

    stopifnot(is.numeric(C), is.matrix(C), is.numeric(d), is.vector(d))
    if (is.null(A) && !is.null(b) || !is.null(A) && is.null(b))
        stop("If any, both 'A' and 'b' must be NULL.")
    if (is.null(Aeq) && !is.null(beq) || !is.null(Aeq) && is.null(beq))
        stop("If any, both 'Aeq' and 'beq' must be NULL.")

    if (any(C != t(C)))
        warning("Argument 'C' is not a symmetric matrix.")
    if (any(eigen(C)$values <= 0))
        warning("Matrix 'C' is not positive definite.")

    # check matrix sizes for C, A, and Aeq
    mc  <- nrow(C); nc  <- ncol(C); n <- nc
    if (mc != nc)
        stop("Argument 'C' must be a quadratic matrix")
    if (length(d) != nc)
        stop("Dimensions of 'C' and 'd' do not fit.")
    if (is.null(Aeq)) {
        meq <- 0
    } else if (is.vector(Aeq)) {
        Aeq <- matrix(Aeq, 1)
        meq <- 1
    } else {
        meq <- nrow(Aeq)
    }
    if (!is.null(Aeq) && ncol(Aeq) != n)
        stop("Dimensions of 'C' and 'Aeq' do not fit.")

    # check lower and upper bounds
    if (is.null(lb)) {
        diag_lb <- NULL
    } else {
        if (length(lb) == 1) {
            lb <- rep(lb, n)
        } else if (length(lb) != n) {
            stop("Length of 'lb' and dimensions of C do not fit.")
        }
        diag_lb <- diag(n)
    }
    if (is.null(ub)) {
        diag_ub <- NULL
    } else {
        if (length(ub) == 1) {
            ub <- rep(ub, n)
        } else if (length(ub) != n) {
            stop("Length of 'ub' and dimensions of C do not fit.")
        }
        diag_ub <- -diag(n) # quadprog requires -x >= -ub
        ub <- -ub
    }

    # collect all constraints into one matrix H and vector
    if (!is.null(A)) {
        A <- -A
        b <- -b
    }
    H <- rbind(Aeq, A, diag_lb, diag_ub)
    f <- c(beq, b, lb, ub)

    qps <- quadprog::solve.QP(C, -d, t(H), f, meq = meq)

    eflag <- 0
    if (all(C == t(C)) && all(eigen(C)$values > 0)) eflag <- 1

    list(xmin = qps$solution, fval = qps$value, eflag = eflag)
}
