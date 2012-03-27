anovaspatial <- function (obj, index, xN = 5, yN = 5, visu = FALSE) 
{
    ### DATA PREPARATION
    M <- v2m(maM(obj)[, index], Nsr = maNsr(obj), Nsc = maNsc(obj), 
        Ngr = maNgr(obj), Ngc = maNgc(obj))
    tmp <- 0
    for (ii in 1:xN) {
        tmp <- c(tmp, rep(ii, floor(dim(M)[[1]]/xN)))
    }
    tmp <- tmp[-1]
    tmp <- c(tmp, rep(ii + 1, dim(M)[[1]] - length(tmp)))
    tmp2 <- 0
    for (ii in 1:yN) {
        tmp2 <- c(tmp2, rep(tmp + (ii - 1) * max(tmp[1:dim(M)[[1]]]), 
            floor(dim(M)[[2]]/yN)))
    }
    tmp2 <- tmp2[-1]
    tmp2 <- c(tmp2, rep(tmp + (ii - 1) * max(tmp[1:dim(M)[[1]]]), 
        dim(M)[[2]] - length(tmp2)/dim(M)[[1]]))
    block <- factor(tmp2)
    ### ANOVA
    s <- summary(ano <- lm(as.vector(M) ~ factor(block) - 1))
    ### VISUALISATION 
    if (visu) {
        ptmp <- tmp2
        pvalues <- s[[4]][, 4]
        for (ii in 1:max(tmp2)) {
            ptmp[tmp2 == ii] <- -log10(pvalues[[ii]])
        }
        mat <- matrix(1:2, ncol = 2, nrow = 1, byrow = TRUE)
        l <- layout(mat, widths = c(3, 1))
        colo <- rgb((1:100)/100, green = 0, blue = 0)
        image(1:dim(M)[[2]], 1:dim(M)[[1]], t(matrix(ptmp, ncol = dim(M)[[2]])), 
            zlim = c(0, max(ptmp)), main = "Significance based on t-test", 
            xlab = "X", ylab = "Y", col = colo)
        par(xaxt = "n")
        par(cex.lab = 1.1)
        par(font = 4)
        par(cex.axis = 1)
        seqo <- seq(0, max(ptmp), by = 0.02)
        image(c(0, 1), seqo, t(matrix(c(1:length(seqo)))), col = colo, 
            xlab = "", ylab = "-log10(p-value)")
        par(yaxt = "s")
        par(xaxt = "s")
    }
    s
}
########################################################################



