fgbg.visu <- function (obj) 
{
    Rf <- maRf(obj)
    Gf <- maGf(obj)
    Rb <- maRb(obj)
    Gb <- maGb(obj)
    for (i in 1:dim(maM(obj))[[2]]) {
        par(mfrow = c(2, 3))
        tmp <- v2m(log2(Rf[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(0, 
                max(log2(Rf[, i]))), main = paste("Array ", i, 
                ":  log2(Fg)"), visu = TRUE)
        tmp <- v2m(log2(Rb[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(0, 
                max(log2(Rb[, i]))), main = "log2(Bg)", visu = TRUE)
        tmp <- v2m(log2(Rf[, i] - Rb[, i]), Nsr = maNsr(obj), 
            Nsc = maNsc(obj), Ngc = maNgc(obj), Ngr = maNgr(obj), 
            color.lim = c(0, max(log2(Rf[, i] - Rb[, i]))), main = "log2(Fg-Bg)", 
            visu = TRUE)
        tmp <- v2m(-log2(Gf[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(-max(log2(Gf[, 
                i])), 0), main = "log2(Fg)", visu = TRUE)
        tmp <- v2m(-log2(Gb[, i]), Nsr = maNsr(obj), Nsc = maNsc(obj), 
            Ngc = maNgc(obj), Ngr = maNgr(obj), color.lim = c(-max(log2(Gb[, 
                i])), 0), main = "log2(Bg)", visu = TRUE)
        tmp <- v2m(-log2(Gf[, i] - Gb[, i]), Nsr = maNsr(obj), 
            Nsc = maNsc(obj), Ngc = maNgc(obj), Ngr = maNgr(obj), 
            color.lim = c(-max(log2(Gf[, i] - Gb[, i])), 0), 
            main = "log2(Fg-Bg)", visu = TRUE)
        cat("Pause. Press <Enter> to continue...")
        readline()
        invisible()
    }
}
##############################################################################


