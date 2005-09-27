p.int2 <- function (object, delta = 50, N = -1, av = "median", p.adjust.method = "none") 
{
  if (!(class(object)=="marrayRaw") & !(class(object)=="marrayNorm")){
     stop("Object should be of class marrayRaw or marrayNorm")
   }

 
     PpL <- list(NULL)
     PnL <- list(NULL) 
     AL <- maA(object)   
     ML <- maM(object)   
     index <- c(1:dim(object)[[2]])
  
     for (ii in index){
     A <- AL[,ii]
     M <- ML[,ii]    

    if (N < 0) {
        N <- 100 * length(A)
    }
    MavP <- real(N)

    #### GERNERATING EMPIRICAL DISTRIBUTION 
    if (av == "mean") {
        for (i in 1:N) {
            MavP[i] <- mean(sample(M, 2 * delta + 1))
        }
    }
    if (av == "median") {
        for (i in 1:N) {
            MavP[i] <- median(sample(M, 2 * delta + 1))
        }
    }
    #### STATISTIC FOR ORIGINAL DATA
    Mav <- ma.vector(A, M, delta = delta, av = av)
    Mav.l <- length(Mav)
    pP <- real(length = length(A)) + NA
    MavP.l <- length(MavP)

    #### DETERMINING P-VALUES
    for (i in 1:Mav.l) {
        pP[i] <- sum(MavP >= Mav[i], na.rm = TRUE)/MavP.l
    }
    nP <- real(length = length(M)) + NA
    for (i in 1:Mav.l) {
        nP[i] <- sum(MavP <= Mav[i], na.rm = TRUE)/MavP.l
    }
 
    ### ADJUSTMENT 
    pP[pP == 0] <- 1/N
    nP[nP == 0] <- 1/N
    
    pP.adjust <- p.adjust(pP, method = p.adjust.method)
    nP.adjust <- p.adjust(nP, method = p.adjust.method)
    pP.adjust[is.na(Mav)] <- NA
    nP.adjust[is.na(Mav)] <- NA
     PpL[[ii]] <- pP.adjust
     PnL[[ii]] <- nP.adjust
 }
 list(Pp=PpL, Pn=PnL)
}
#######################################################################



