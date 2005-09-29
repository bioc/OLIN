ino <- function(object, alpha=0.3, weights= NA,bg.corr="subtract",...){

    Mn <- matrix(NA, nrow = dim(maM(object))[1], ncol = dim(maM(object))[2])

    ### SET WEIGHTS TO 1 IF NOT PRESENT
    if (missing(weights)) {       
        weights <- matrix(1, nrow=dim(maM(object))[1], ncol=dim(maM(object))[2])
       # cat("NOTE: Spot weights adjusted to 1\n")
    }

  
    ### NORMALISATION
    
    if (bg.corr=="none" & class(object) =="marrayRaw"){
        A <- 0.5*(log2(maRf(object)) + log2(maGf(object)))
        M <- log2(maRf(object)) -  log2(maGf(object)) 
      } else {
        A <- maA(object)
        M <- maM(object)
      }

  
    for (i in 1:dim(maA(object))[[2]]) {
 
        Atmp <- A[, i]
        Mtmp <- M[, i]
      
        lo <- locfit(Mtmp ~ Atmp, alpha = alpha, weights=weights[,i],...)
        Atmp[is.na(A[, i])] <- 0
        Mtmp <- Mtmp - predict.locfit(lo, data.frame(Atmp = Atmp))
        Mn[, i] <- Mtmp
    }
  
    object2 <- new("marrayNorm", maA = A, maM = Mn,
        maLayout = maLayout(object), maGnames = maGnames(object), 
        maTargets = maTargets(object), maNotes = maNotes(object), 
        maNormCall = match.call())
}
###############################################################




