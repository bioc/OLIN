mxy2.plot <- function (V, X, Y, Ngc, Ngr, Nsc, Nsr, color.lim = c(-1, 1), 
    xlab = "X", ylab = "Y", ...) 
{
    opo <- par(no.readonly = TRUE)

    #### DETERMINING PHYSICAL LAYOUT 
    tempx <- integer(1)
    temp1 <- c(1:Nsc)
    for (s in 1:(Ngc - 1)) {
        temp1 <- c(temp1, (s * Nsc * Nsr + 1):(Nsc * (s * Nsr + 
            1)))
    }
    printdiff <- (X[2] - X[1])/2
    for (l in 1:Ngc) {
        tempx[(l - 1) * (Nsc + 1) + 1] <- X[temp1[1 + Nsc * (l - 
            1)]] - printdiff
        for (k in 2:Nsc) {
            tempx[(l - 1) * (Nsc + 1) + k] <- 0.5 * (X[temp1[(k - 
                1) + Nsc * (l - 1)]] + X[temp1[k + Nsc * (l - 
                1)]])
        }
        tempx[l * (Nsc + 1)] <- X[temp1[l * Nsc]] + printdiff
    }
    tempx <- c(0, tempx, tempx[length(tempx)] + tempx[1])
    temp2 <- seq(1, Nsc * Nsr, Nsc)
    for (s in 1:Ngr) {
        temp2 <- c(temp2, seq(1 + s * Ngc * Nsr * Nsc, (s * Ngc + 
            1) * Nsr * Nsc, Nsc))
    }
    printdiff <- 0.5 * (Y[temp2[2]] - Y[temp2[1]])
    tempy <- c(0)
    for (l in 1:Ngr) {
        tempy[(l - 1) * (Nsr + 1) + 1] <- Y[temp2[1 + Nsr * (l - 
            1)]] - printdiff
        for (k in 2:Nsr) {
            tempy[(l - 1) * (Nsr + 1) + k] <- 0.5 * (Y[temp2[(k - 
                1) + Nsr * (l - 1)]] + Y[temp2[k + Nsr * (l - 
	                1)]])
        }
        tempy[l * (Nsr + 1)] <- Y[temp2[l * Nsr]] + printdiff
    }
    tempy <- c(0, tempy, tempy[length(tempy)] + tempy[1])
    gapmatrix <- matrix(data = 0, nrow = Nsr)
    temp <- gapmatrix
    for (s in 1:(Ngc * Ngr)) {
        bmatrix <- matrix(V[((s - 1) * Nsr * Nsc + 1):(s * Nsr * 
            Nsc)], nrow = Nsr, ncol = Nsc, byrow = TRUE)
        temp <- cbind(temp, gapmatrix, bmatrix)
    }
    temp <- temp[, -1]
    tempdim <- dim(temp)
    temp <- rbind(matrix(0, ncol = tempdim[2]), temp)
    cutx <- (Nsc * Ngc) + Ngc
    smatrix <- temp[, 1:cutx]
    if (Ngr > 1){  
    	for (i in 2:Ngr) {
        	smatrix <- rbind(smatrix, temp[, ((i - 1) * cutx + 1):(i * 
            	cutx)])
    	}
    }
    smatrixdim <- dim(smatrix)
    smatrix <- rbind(smatrix, matrix(0, ncol = smatrixdim[2]))
    smatrix <- cbind(smatrix, matrix(0, nrow = (smatrixdim[1] + 
        1)))

    #### VISUALISATION 
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
    image(tempx, tempy, t(smatrixtmp), col = colo, zlim = color.lim, 
        xlab = xlab, ylab = ylab, ...)
    colorbar.mxy(color.lim = color.lim, ylablim = FALSE)
    par(opo)
}
#########################################################################


