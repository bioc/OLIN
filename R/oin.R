oin <- function (object,alpha = seq(0.1, 1, 0.1),weights= NA, bg.corr="subtract", ...) 
{
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
      } else  {        
        A <- maA(object)
        M <- maM(object)
      }

    
  
    for (i in 1:dim(A)[[2]]) {
        Atmp <- A[, i]
        Mtmp <- M[, i]
    
   
       
            x <- data.frame(summary(gcvplot(Mtmp ~ Atmp, data = data.frame(Mtmp, 
                Atmp), alpha = alpha,  weights=weights[,i])), alpha)
            alphaopt <- x[which(x[, 2] == min(x[, 2])), 3]
            lo <- locfit(Mtmp ~ Atmp, alpha = alphaopt, weights=weights[,i],...)
            Atmp[is.na(A[, i])] <- 0
            Mtmp <- Mtmp - predict(lo, data.frame(Atmp = Atmp))
            Mtmp[is.na(A[, i])] <- NA
           
        
        Mn[, i] <- Mtmp
  
    }
   
    object2 <- new("marrayNorm", maA = A, maM = Mn, 
        maLayout = maLayout(object), maGnames = maGnames(object), 
        maTargets = maTargets(object), maNotes = maNotes(object), 
        maNormCall = match.call())
}
#############################################################################


