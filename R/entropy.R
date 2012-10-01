##
##  e n t r o p y . R  (Fast) Approximate Entropy
##


approx_entropy <- function(ts, edim = 2, r = 0.2*sd(ts), elag = 1) {

    N <- length(ts)
    result <- numeric(2)

    for (j in 1:2) {
        m <- edim + j - 1
        phi <- zeros(1, N-m+1)
        dataMat <- zeros(m, N-m+1)
        for (i in 1:m)
            dataMat[i, ] <- ts[i:(N-m+i)]

        for (i in 1:(N-m+1)) {
            tempMat <- abs(dataMat - repmat(dataMat[, i, drop = FALSE], 1, N-m+1))
            boolMat <- apply(tempMat > r, 2, max)
            phi[i]  <- sum(!boolMat)/(N-m+1)
        }
        result[j] <- sum(phi)/(N-m+1)
    }

    apen <- log(result[1]/result[2])
    return(apen)
}
