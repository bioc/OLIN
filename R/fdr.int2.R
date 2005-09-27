fdr.int2 <- function (object, delta = 50, N = 100, av = "median") 
{

 if (!(class(object)=="marrayRaw") & !(class(object)=="marrayNorm")){
     stop("Object should be of class marrayRaw or marrayNorm")
   }

 
     FDRpL <- list(NULL)
     FDRnL <- list(NULL) 
     AL <- maA(object)   
     ML <- maM(object)   
     index <- c(1:dim(object)[[2]])
  
     for (ii in index){
     A <- AL[,ii]
     M <- ML[,ii]    


    XavP <- real(length(M) * N)

    ### GENERATING BACKGROUND DISTRIBUTION 
    for (i in 1:N) {
        XavP[((i - 1) * length(M) + 1):(i * length(M))] <- ma.vector(A, 
            sample(M), av = av, delta = delta)
    }
    XavP <- XavP[!is.na(XavP)]
    XavP.l <- length(XavP)

    ### STATISTICS OF ORIGINAL DATA 
    Xav <- ma.vector(A, M, av = av, delta = delta)

    ### COMPARING STATISTICS OF ORIGINAL DATA AND PERMUTATED DATA 
    o <- 1:length(Xav)
    ro <- o[rank(Xav)]
    XavS <- sort(Xav)
    XavS.l <- length(XavS)
    XN <- real(length = length(XavS)) + NA
    for (i in 1:XavS.l) {
        XN[i] <- sum(XavP >= XavS[i], na.rm = TRUE)
    }
    XN <- XN/(XavP.l/XavS.l)

    ### CALCULATION OF FALSE POSITIVES RATES
    pFDR <- real(length = length(Xav)) + NA
    for (i in (delta + 1):XavS.l) {
        pFDR[XavS.l - i + 1] <- XN[XavS.l - i + 1]/(XN[XavS.l - 
            i + 1] + i)
    }
    pFDR[pFDR == 0] <- 1/(XavS.l * N)
    nFDR <- real(length = length(Xav)) + NA
    for (i in 1:XavS.l) {
        nFDR[i] <- (XavS.l - XN[i])/((XavS.l - XN[i]) + i)
    }
    nFDR[nFDR == 0] <- 1/(XavS.l * N)
    FDRpL[[ii]] <- pFDR[ro]
    FDRnL[[ii]] <- nFDR[ro]
 }
list(FDRp=FDRpL,FDRn=FDRnL)


}
#######################################################################


