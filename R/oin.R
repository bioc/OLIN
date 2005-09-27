oin <- function (object,alpha = seq(0.1, 1, 0.1),weights= NA, ...) 
{
    Mn <- matrix(NA, nrow = dim(maM(object))[1], ncol = dim(maM(object))[2])
 

    ### SET WEIGHTS TO 1 IF NOT PRESENT
    if (missing(weights)) {       
        weights <- matrix(1, nrow=dim(maM(object))[1], ncol=dim(maM(object))[2])
       # cat("NOTE: Spot weights adjusted to 1\n")
    }

    ### NORMALISATION 
    for (i in 1:dim(maA(object))[[2]]) {
        Atmp <- maA(object)[, i]
        Mtmp <- maM(object)[, i]
    
   
       
            x <- data.frame(summary(gcvplot(Mtmp ~ Atmp, data = data.frame(Mtmp, 
                Atmp), alpha = alpha,  weights=weights[,i])), alpha)
            alphaopt <- x[which(x[, 2] == min(x[, 2])), 3]
            lo <- locfit(Mtmp ~ Atmp, alpha = alphaopt, weights=weights[,i],...)
            Atmp[is.na(maA(object)[, i])] <- 0
            Mtmp <- Mtmp - predict.locfit(lo, data.frame(Atmp = Atmp))
            Mtmp[is.na(maA(object)[, i])] <- NA
           
        
        Mn[, i] <- Mtmp
  
    }
   
    object2 <- new("marrayNorm", maA = maA(object), maM = Mn, 
        maLayout = maLayout(object), maGnames = maGnames(object), 
        maTargets = maTargets(object), maNotes = maNotes(object), 
        maNormCall = match.call())
}
#############################################################################


