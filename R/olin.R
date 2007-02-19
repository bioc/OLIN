olin <- function (object, X = NA, Y = NA, alpha = seq(0.1, 1, 0.1), 
    iter = 3,  scale = c(0.05, 0.1, 0.5, 1, 2,10, 20), OSLIN  = FALSE, weights= NA,
                  genepix=FALSE,bg.corr="subtract",...) 
{
    Mn <- matrix(NA, nrow = dim(maM(object))[1], ncol = dim(maM(object))[2])
    Layout <- maLayout(object)

    ### SET WEIGHTS TO 1 IF NOT PRESENT
    if (missing(weights)) {       
        weights <- matrix(1, nrow=dim(maM(object))[1], ncol=dim(maM(object))[2])
      }
       # cat("NOTE: Spot weights adjusted to 1\n")
    if (genepix){
      weights[weights>=0] <- 1;
      weights[weights < 0 ] <- 0;

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
  # BACKGROUND CORRECTION 
  if ((bg.corr=="half" | bg.corr=="minimum" |  bg.corr=="movingmin" |
       bg.corr=="edwards" | bg.corr=="normexp") & class(object) =="marrayRaw"){
          object.b <- backgroundCorrect2(object,method=bg.corr)
          A <- maA(object.b)
          M <- maM(object.b)
  } else { if (bg.corr=="none" & class(object) =="marrayRaw"){
        A <- 0.5*(log2(maRf(object)) + log2(maGf(object)))
        M <- log2(maRf(object)) -  log2(maGf(object))
        M[!is.finite(A)] <- NA
        A[!is.finite(A)] <- NA
      } else {
        A <- maA(object)
        M <- maM(object)
      }
         }
    
    ### NORMALISATION
    
    for (i in 1:dim(maA(object))[[2]]) {
        Atmp <- A[, i]
        Mtmp <- M[, i]
        Xtmp <- X[, i]
        Ytmp <- Y[, i]
        CVA <- real(length = length(alpha)) + NA
        for (ii in 1:iter) {
            for (j in 1:length(alpha)) {
            CVA <- gcv(Mtmp ~ Atmp, data = data.frame(Mtmp,Atmp), alpha = alpha[j],  weights=weights[,i],...)[4]
            }
            alphaopt <- alpha[(which(min(CVA) == CVA))]
            lo <- locfit(Mtmp ~ Atmp, alpha = alphaopt, weights=weights[,i],...)
            Atmp[is.na(A[, i])] <- 0
            Mtmp <- Mtmp - predict.locfit(lo, data.frame(Atmp = Atmp))
            Mtmp[is.na(A[, i])] <- NA
            CVA <- matrix(NA, ncol = length(scale), nrow = length(alpha))
            for (j in 1:length(alpha)) {
                for (jj in 1:length(scale)) {
                  CVA[j, jj] <- gcv(Mtmp ~ Xtmp * Ytmp,data = data.frame(Xtmp = Xtmp, 
                    Ytmp = Ytmp), weights=weights[,i], alpha = alpha[j], scale = c(1,scale[jj]),...)[4]
                }
            }
            alphaopt <- alpha[((which(min(CVA) == CVA)) - 1)%%length(alpha) + 1]
            scaleopt <- scale[(which(min(CVA) == CVA) - 1)%/%length(alpha) +  1]
            lo <- locfit(Mtmp ~ Xtmp * Ytmp,  weights=weights[,i],alpha = alphaopt,scale = c(1, scaleopt),...)
            Mtmp <- Mtmp - predict.locfit(lo, data.frame(Xtmp = Xtmp,Ytmp = Ytmp))
            Mtmp[is.na(A[, i])] <- NA
        }
        Mn[, i] <- Mtmp

        ### OSLIN
        if (OSLIN) {
            absMtmp <- abs(Mtmp)
            absMtmp[is.na(A[, i])] <- NA
            for (j in 1:length(alpha)) {
                for (jj in 1:length(scale)) {
                  CVA[j, jj] <- gcv(absMtmp ~ Xtmp + Ytmp, data = data.frame(Xtmp = Xtmp, 
                    Ytmp = Ytmp), weights=weights[,i], alpha = alpha[j], scale = c(1, 
                    scale[jj]),...)[4]
                }
            }
            alphaoptS <- alpha[((which(min(CVA) == CVA)) - 1)%%length(alpha) + 
                1]
            scaleoptS <- scale[(which(min(CVA) == CVA) - 1)%/%length(alpha) + 
                1]
            lo <- locfit(absMtmp ~ Xtmp + Ytmp, weights=weights[,i], alpha = alphaoptS, 
                scale = scaleoptS,...)
            absMp <- predict.locfit(lo, data.frame(Xtmp = Xtmp, 
                Ytmp = Ytmp))
            Mtmp <- (Mtmp/absMp) * sqrt(var(Mtmp, na.rm = TRUE)/var((Mtmp/absMp), 
                na.rm = TRUE))
            Mn[, i] <- Mtmp
        }
    }
    
    object2 <- new("marrayNorm", maA = A, maM = Mn, 
        maLayout = maLayout(object), maGnames = maGnames(object), 
        maTargets = maTargets(object), maNotes = maNotes(object), 
        maNormCall = match.call())
}
#############################################################################

