colorbar.sig <- function (color.lim = c(-3, 3)) 
{
    opo <- par(no.readonly = TRUE)
    par(mar = c(4, 1, 5, 3))
    colo <- c(rgb(0, (100:0)/100, 0), rgb(0, 0, 0), rgb((1:100)/100, 
        g = 0, b = 0))
    ticks <- seq(color.lim[1], color.lim[2], 1)
    labels <- -(abs(ticks))
    labels <- as.character(labels)
    par(xaxt = "n")
    par(yaxt = "n")
    par(cex.lab = 1.45)
    par(font = 4)
    par(cex.axis = 1)
    seqo <- seq(color.lim[1], color.lim[2], by = 0.02)
    image(c(0, 1), c(seq(color.lim[1], 0, by = 0.02), seq(0.02, 
        color.lim[2], by = 0.02)), t(matrix(c(1:length(seqo)))), 
        col = colo, xlab = "", ylab = "log10", font.axis = 2)
    par(yaxt = "s")
    axis(2, at = ticks, labels = labels)
    par(opo)
}

##########################################################################


