sigint.plot2 <- function (object, Sp, Sn, ylim=c(-3,-3), ...) 
{
    A <- maA(object)
    M <- maM(object)
    opo <- par(no.readonly = TRUE)
    ylim[2] <- -ylim[2]
    par(mfrow = c(2, 1))
    plot(A, M, main = "MA plot", ...)
    ticks <- seq(ylim[1], ylim[2], 1)
    labels <- -(abs(ticks))
    labels <- as.character(labels)
    par(yaxt = "n")
    lfp <- log10(Sp)
    lfn <- log10(Sn)
    lfp[lfp < -ylim[2]] <- -ylim[2]
    lfn[lfn < ylim[1]] <- ylim[1]
    plot(A, -lfp, ylab = "log10", ylim = ylim, col = "red", main = "Significance")
    points(A, lfn, col = "green")
    par(yaxt = "s")
    axis(2, at = ticks, labels = labels)
    par(opo)
}

################################################################################



