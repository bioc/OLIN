fgbg.visu <- function (obj,label)
{
    Rf <- maRf(obj)
    Gf <- maGf(obj)
    Rb <- maRb(obj); if (all(dim(Rb)==c(0,0))){Rb <- matrix(1,ncol=dim(Rf)[[2]],nrow=dim(Rf)[[1]])} 
    Gb <- maGb(obj); if (all(dim(Gb)==c(0,0))){Gb <- matrix(1,ncol=dim(Rf)[[2]],nrow=dim(Rf)[[1]]) }
    Rb1 <- maRb(obj); if (all(dim(Rb1)==c(0,0))){Rb1 <- matrix(0,ncol=dim(Rf)[[2]],nrow=dim(Rf)[[1]])} 
    Gb1 <- maGb(obj); if (all(dim(Gb1)==c(0,0))){Gb1 <- matrix(0,ncol=dim(Rf)[[2]],nrow=dim(Rf)[[1]]) }
    

    for (i in 1:dim(maM(obj))[[2]]) {
        if (missing(label)) label2 <- paste("Array",i)
        par(mfrow = c(2, 3))
        tmp <- v2m(log2(Rf[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(-max(log2(Rf[, i])), 
                max(log2(Rf[, i]))), main = paste(label2,
                ":  log2(Fg)"), visu = TRUE)
        tmp <- v2m(log2(Rb[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(-max(log2(Rf[, i])), 
                max(log2(Rf[, i]))), main = "log2(Bg)", visu = TRUE)
        tmp <- Rf[, i] - Rb1[, i]; tmp[tmp <= 0] <- NaN ; 
        tmp <- v2m(log2(tmp), Nsr = maNsr(obj), 
            Nsc = maNsc(obj), Ngc = maNgc(obj), Ngr = maNgr(obj), 
            color.lim = c(- log2(max(tmp,na.rm=TRUE)), log2(max(tmp,na.rm=TRUE))),
            main = "log2(Fg-Bg)", 
            visu = TRUE)
        tmp <- v2m(-log2(Gf[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(-max(log2(Gf[,i])),
            max(log2(Gf[,i]))), main = "log2(Fg)", visu = TRUE)
        tmp <- v2m(-log2(Gb[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(-max(log2(Gf[,i])),
          max(log2(Gf[,i]))), main = "log2(Bg)", visu = TRUE)
       tmp <- Gf[, i] - Gb1[, i]; tmp[tmp <= 0] <- NaN ; 
       tmp <- v2m(-log2(tmp), Nsr = maNsr(obj), 
            Nsc = maNsc(obj), Ngc = maNgc(obj), Ngr = maNgr(obj), 
            color.lim = c(-max(log2(tmp),na.rm=TRUE),max(log2(tmp),na.rm=TRUE)), 
            main = "log2(Fg-Bg)", visu = TRUE)
      
        if  (dim(obj)[2]>1){
        cat("Pause. Press <Enter> to continue...")
        readline()
        invisible()
     }
    }
}
##############################################################################


