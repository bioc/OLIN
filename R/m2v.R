m2v <- function (M, Ngc, Ngr, Nsc, Nsr, visu = FALSE, color.lim = c(-1, 
    1), xlab = "Columns", ylab = "Rows", ...) 
{
    smatrix <- M
    #### VISUALISATION 
    if (visu) {
        colo <- c(rgb(0, (100:0)/100, 0), rgb(0, 0, 0), rgb((1:100)/100, 
            g = 0, b = 0))
        stmp <- smatrix
        stmp[stmp < color.lim[1]] <- color.lim[1]
        stmp[stmp > color.lim[2]] <- color.lim[2]
        image(1:dim(smatrix)[[2]], 1:dim(smatrix)[[1]], t(stmp), 
            col = colo, zlim = color.lim, xlab = xlab, ylab = ylab, 
            ...)
    }
    ### CONVERSION FROM MATRIX TO VECTOR
    tmp <- c(0)
    for (j in 1:Ngr) {
        for (i in 1:Ngc) {
            a <- (i - 1) * Nsc + 1
            b <- i * Nsc
            c <- (j - 1) * Nsr + 1
            d <- j * Nsr
            tmp2 <- as.vector(t(smatrix[c:d, a:b]))
            tmp <- c(tmp, tmp2)
        }
    }
    tmp <- tmp[-1]
}
##############################################################################


