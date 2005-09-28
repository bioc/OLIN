lin <- function (object, X = NA, Y = NA, alpha  = 0.3, iter=2, scale = TRUE, weights= NA,bg.corr="sub", ...) 
{

    Mn <- matrix(NA, nrow = dim(maM(object))[1], ncol = dim(maM(object))[2])
    Layout <- maLayout(object)

    ### SET WEIGHTS TO 1 IF NOT PRESENT
    if (missing(weights)) {       
        weights <- matrix(1, nrow=dim(maM(object))[1], ncol=dim(maM(object))[2])
       # cat("NOTE: Spot weights adjusted to 1\n")
    }

    ### MAPPING LAYOUT ON X AND Y
    if (is.na(X[1]) | is.na(Y[1])) {
        X <- matrix(0, ncol = dim(maA(object))[[2]], nrow = dim(maA(object))[[1]])
        Y <- matrix(0, ncol = dim(maA(object))[[2]], nrow = dim(maA(object))[[1]])
        Xtmp <- integer(dim(maA(object))[[1]])
        Ytmp <- integer(dim(maA(object))[[1]])
        blocksize <- maNsc(Layout) * maNsr(Layout)
        for (i in 1:dim(maA(object))[[1]]) {
            iblock <- (i - 1)%/%blocksize
            ispot <- (i - 1)%%blocksize + 1
            Xtmp[i] <- (ispot - 1)%%maNsc(Layout) + 1 + (iblock%%maNgc(Layout)) * 
                maNsc(Layout)
            Ytmp[i] <- (ispot - 1)%/%maNsc(Layout) + 1 + (iblock%/%maNgc(Layout)) * 
                maNsr(Layout)
        }
        X <- X + Xtmp
        Y <- Y + Ytmp
    } else {
    X <- as.matrix(X)
    Y <- as.matrix(Y)
}
    ### NORMALISATION
    if (bg.corr=="none" & class(object) =="marrayRaw"){
        A <- 0.5*(log2(maRf(object)) + log2(maGf(object)))
        M <- log2(maRf(object)) -  log2(maGf(object)) 
      } else {
        A <- maA(object)
        M <- maM(object)
      }

  
    for (i in 1:dim(A)[[2]]) {
        Atmp <- A[, i]
        Mtmp <- M[, i]
        Xtmp <- X[, i]
        Ytmp <- Y[, i]
       for (ii in 1:iter) {
            lo <- locfit(Mtmp ~ Atmp, alpha = alpha, weights=weights[,i],...)
            Atmp[is.na(A[, i])] <- 0
            Mtmp <- Mtmp - predict.locfit(lo, data.frame(Atmp = Atmp))
            Mtmp[is.na(maA(object)[, i])] <- NA
            
            lo <- locfit(Mtmp ~ Xtmp * Ytmp,  weights=weights[,i],alpha = alpha, 
                scale = TRUE, ...)

            Mtmp <- Mtmp - predict.locfit(lo, data.frame(Xtmp = Xtmp, 
                Ytmp = Ytmp))

            Mtmp[is.na(A[, i])] <- NA
        
           
     }
            Mn[, i] <- Mtmp
    }
 
    object2 <- new("marrayNorm", maA = A, maM = Mn, 
        maLayout = maLayout(object), maGnames = maGnames(object), 
        maTargets = maTargets(object), maNotes = maNotes(object), 
        maNormCall = match.call())
}
#############################################################################


