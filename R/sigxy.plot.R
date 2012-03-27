sigxy.plot <- function (Sp, Sn, color.lim = c(-3, 3), ...) 
{
    opo <- par(no.readonly = TRUE)
    col.sig <- c(rgb(0, (100:0)/100, 0), rgb(0, 0, 0), rgb((1:100)/100, 
        green = 0, blue = 0))
   
    ### FUSING SIGNIFICANCE VECTORS TO MATRIX 
    Mtmp1 <- -log10(Sp)
    Mtmp2 <- log10(Sn)
    for (ii in 1:dim(Mtmp1)[[1]]) {
        for (jj in 1:dim(Mtmp1)[[2]]) {
            if (!is.na(Mtmp1[ii, jj])) {
                if (abs(Mtmp1[ii, jj]) < abs(Mtmp2[ii, jj])) {
                  Mtmp1[ii, jj] <- Mtmp2[ii, jj]
                }
            }
        }
    }
    #### THRESHOLDING 
    Mtmp1[!is.na(Mtmp1) & Mtmp1 < color.lim[1]] <- color.lim[1]
    Mtmp1[!is.na(Mtmp1) & Mtmp1 > color.lim[2]] <- color.lim[2]
   
    #### VISUALISATION
    mat <- matrix(1:2, ncol = 2, nrow = 1, byrow = TRUE)
    l <- layout(mat, widths = c(5.5, 1))
    image(1:dim(Mtmp1)[[2]], 1:dim(Mtmp1)[[1]], t(Mtmp1), xlab = "X", 
        ylab = "Y", col = col.sig, zlim = color.lim, ...)
    colorbar.sig(color.lim)
    par(opo)
}
###############################################################################


