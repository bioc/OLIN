mxy.plot <- function (V, Ngc, Ngr, Nsc, Nsr, color.lim = c(-1, 1), xlab = "Columns", 
    ylab = "Rows", ...) 
{
    ### CONVERSION OF VECTOR TO MATRIX
    tmp <- matrix(nrow = Nsr)
    for (s in 1:(Ngc * Ngr)) {
        tmpmatrix <- matrix(V[((s - 1) * Nsr * Nsc + 1):(s * 
            Nsr * Nsc)], nrow = Nsr, ncol = Nsc, byrow = TRUE)
        tmp <- cbind(tmp, tmpmatrix)
    }
    tmp <- tmp[, -1]
    cutx <- (Nsc * Ngc)
    smatrix <- tmp[, 1:cutx]
    if (Ngr > 1){ 
    	for (i in 2:Ngr) {
            smatrix <- rbind(smatrix, tmp[, ((i - 1) * cutx + 1):(i * 
                             cutx)])
        }
    }
    ### VISUALISATION 
    colo <- c(rgb(0, (100:0)/100, 0), rgb(0, 0, 0), rgb((1:100)/100, 
        g = 0, b = 0))
    smatrixtmp <- smatrix
    smatrixtmp[smatrixtmp < color.lim[1]] <- color.lim[1]
    smatrixtmp[smatrixtmp > color.lim[2]] <- color.lim[2]
    mat <- matrix(1:2, ncol = 2, nrow = 1, byrow = TRUE)
    l <- layout(mat, width = c(5, 1))
    par(xaxt = "s")
    par(yaxt = "s")
    par(mar = c(4, 4, 5, 2))
    image(1:dim(smatrix)[[2]], 1:dim(smatrix)[[1]], t(smatrixtmp), 
        col = colo, zlim = color.lim, xlab = xlab, ylab = ylab, 
        ...)
    colorbar.mxy(color.lim = color.lim, ylablim = FALSE)
}
############################################################################



