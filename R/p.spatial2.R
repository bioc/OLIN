p.spatial2 <- function (object, delta = 2, N = -1, av = "median", p.adjust.method = "none") 
{
 if (!(class(object)=="marrayRaw") & !(class(object)=="marrayNorm")){
  stop("Object should be of class marrayRaw or marrayNorm")
}    
    
     
     PpL <- list(NULL)
     PnL <- list(NULL)
     ML <- maM(object)   
     index <- c(1:dim(object)[[2]])

    for (ii in index){
  
      X <- v2m(ML[,ii],Ngr=maNgr(object),Ngc=maNgc(object),Nsr=maNsr(object),Nsc=maNsc(object))
   
    
    if (N < 0) {
        N <- 100 * dim(X)[[1]] * dim(X)[[2]]
    }
    XavP <- real(N)

    #### GENERATING EMPIRICAL DISTRIBUTION 
    if (av == "mean") {
        for (i in 1:N) {
            XavP[i] <- mean(sample(as.vector(X), (delta + 1)^2))
        }
    }
    if (av == "median") {
        for (i in 1:N) {
            XavP[i] <- median(sample(as.vector(X), (delta + 1)^2))
        }
    }
    
    #### STATS FOR ORIGINAL DATA
    Xav <- as.vector(ma.matrix(X, delta = delta, av = av))
    Xav.l <- length(Xav)
   
    #### DETERMINING SIGNIFICANCE
    pP <- real(length = length(as.vector(X))) + NA
    XavP.l <- length(XavP)
    for (i in 1:Xav.l) {
        pP[i] <- sum(XavP >= Xav[i], na.rm = TRUE)/XavP.l
    }
    nP <- real(length = length(as.vector(X))) + NA
    for (i in 1:Xav.l) {
        nP[i] <- sum(XavP <= Xav[i], na.rm = TRUE)/XavP.l
    }

    #### ADJUSTMENTS
    pP[pP == 0] <- 1/N
    nP[nP == 0] <- 1/N
    pP.adjust <- p.adjust(pP, method = p.adjust.method)
    nP.adjust <- p.adjust(nP, method = p.adjust.method)
    pP.adjust <- matrix(pP.adjust, ncol = dim(X)[[2]])
    nP.adjust <- matrix(nP.adjust, ncol = dim(X)[[2]])
      PpL[[ii]] <-  m2v(pP.adjust,Ngr=maNgr(object),Ngc=maNgc(object),Nsr=maNsr(object),Nsc=maNsc(object))
      PnL[[ii]] <-  m2v(nP.adjust,Ngr=maNgr(object),Ngc=maNgc(object),Nsr=maNsr(object),Nsc=maNsc(object))
               
   }
  list(Pp=PpL, Pn=PnL)
}
#########################################################################


