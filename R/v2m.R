v2m <- function (V, Ngc, Ngr, Nsc, Nsr, visu = FALSE, color.lim = c(-1, 
    1), xlab = "Columns", ylab = "Rows", ...) 
{
    ### CONVERSION FROM VECTOR TO MATRIX
    tmp <- matrix(nrow = Nsr)
    for (s in 1:(Ngc * Ngr)) {
        tmpmatrix <- matrix(V[((s - 1) * Nsr * Nsc + 1):(s * 
            Nsr * Nsc)], nrow = Nsr, ncol = Nsc, byrow = TRUE)
        tmp <- cbind(tmp, tmpmatrix)
    }
    tmp <- tmp[, -1]
    cutx <- (Nsc * Ngc)
    smatrix <- tmp[, 1:cutx]
    for (i in 2:Ngr) {
        smatrix <- rbind(smatrix, tmp[, ((i - 1) * cutx + 1):(i * 
            cutx)])
    }
    ### VISUALISATION 
    if (visu) {
        colo <- c(rgb(0, (100:0)/100, 0), rgb(0, 0, 0), rgb((1:100)/100, 
            g = 0, b = 0))
        smatrixtmp <- smatrix
        smatrixtmp[smatrixtmp < color.lim[1]] <- color.lim[1]
        smatrixtmp[smatrixtmp > color.lim[2]] <- color.lim[2]
        image(1:dim(smatrix)[[2]], 1:dim(smatrix)[[1]], t(smatrixtmp), 
            col = colo, zlim = color.lim, xlab = xlab, ylab = ylab, 
            ...)
    }
    smatrix
}
#######################################################################

