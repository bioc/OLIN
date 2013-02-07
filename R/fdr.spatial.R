fdr.spatial <- function (X, delta = 2, N = 100, av = "median", edgeNA = FALSE) 
{
    XavP <- double(length(X) * N) + NA

    ### GENERATING EMPIRICAL BACKGROUND DISTRIBUTION BASED ON RANDOM PERMUTATION 
    for (i in 1:N) {
        tmp <- ma.matrix(matrix(sample(as.vector(X)), ncol = dim(X)[[2]]), 
            delta = delta, av = av, edgeNA = edgeNA)
        XavP[((i - 1) * length(X) + 1):(i * length(X))] <- as.vector(tmp)
    }
    XavP <- XavP[!is.na(XavP)]
    XavP.l <- length(XavP)

    #### AV. M STATISTICS FOR ORIGINAL DATA 
    Xav <- as.vector(ma.matrix(X, delta = delta, av = av, edgeNA = edgeNA))
    o <- 1:length(Xav)
    ro <- o[rank(Xav)]
    XavS <- sort(Xav)
    XavS.l <- length(XavS)
    XN <- double(length = length(XavS)) + NA

    #### COMPARINING STATISTIC OF ORIGINAL DATA WITH EMPIRICAL BACKGROUND DISTRIBUTION 
    for (i in 1:XavS.l) {
        XN[i] <- sum(XavP >= XavS[i], na.rm = TRUE)
    }
    XN <- XN/(XavP.l/XavS.l)


    #### DETERMINING FALSE DISCOVERY RATES
    pFDR <- double(length = length(Xav)) + NA
    for (i in 1:XavS.l) {
        pFDR[XavS.l - i + 1] <- XN[XavS.l - i + 1]/(XN[XavS.l - 
            i + 1] + i)
    }
    pFDR[pFDR == 0] <- 1/(XavS.l * N)
    nFDR <- double(length = length(Xav)) + NA
    for (i in 1:XavS.l) {
        nFDR[i] <- (XavS.l - XN[i])/((XavS.l - XN[i]) + i)
    }
    nFDR[nFDR == 0] <- 1/(XavS.l * N)
    pFDR <- matrix(pFDR[ro], ncol = dim(X)[[2]])
    nFDR <- matrix(nFDR[ro], ncol = dim(X)[[2]])
    list(FDRp = pFDR, FDRn = nFDR)
}
##########################################################################



