colorbar.mxy.abs <- function (color.lim, color = "red", ylab = "", ylablim = FALSE) 
{
    par(xaxt = "n")
    par(yaxt = "n")
    par(mar = c(4, 1, 5, 3))

    ### COLOR PALETTE 
    if (color == "green") {
        col <- c(rgb(0, 0, 0), rgb(green = (1:100)/100, red= 0, blue = 0))
    }
    else {
        col <- c(rgb(0, 0, 0), rgb((1:100)/100, green = 0, blue = 0))
    }
    ###  PLOTTING
    seqo <- seq(color.lim[1], color.lim[2], length = 200)
    image(c(0, 1), seqo, t(matrix(c(1:length(seqo)))), col = col, 
        xlab = "", ylab = ylab)
    par(yaxt = "s")

    ### AXIS LABELLING 
    if (ylablim) {
        ylim1 <- round(color.lim[1], 2)
        if (ylim1 < min(seqo)) {
            ylim1 <- ylim1 + 0.02
        }
        ylim2 <- round(color.lim[2], 2)
        if (ylim2 > max(seqo)) {
            ylim2 <- ylim2 - 0.02
        }
        ylimlabel1 <- as.character(abs(ylim1))
        ylimlabel2 <- as.character(ylim2)
        axis(4, labels = c(paste("- ", ylimlabel1, sep = ""), 
            "0", ylimlabel2), at = c(ylim1, 0, ylim2), srt = 90)
        par(xaxt = "s")
    }
    else {
        axis(4)
    }
}
####################################################################################



