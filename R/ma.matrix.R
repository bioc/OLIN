ma.matrix <- function (X, av = "median", delta = 2, edgeNA = FALSE) 
{
    Xav <- matrix(NA, nrow = dim(X)[[1]], ncol = dim(X)[[2]])
    if (av == "mean") {
        average <- mean
    }
    else {
        average <- median
    }
    #### SLIDING WINDOW 
    for (j in 1:dim(X)[[1]]) {
        for (k in 1:dim(X)[[2]]) {
            a <- (j - delta)
            c <- (j + delta)
            b <- (k - delta)
            d <- (k + delta)
            if (a < 1) {
                a <- 1
                c <- 2 * delta + 1
            }
            if (b < 1) {
                b <- 1
                d <- 2 * delta + 1
            }
            if (c > dim(X)[[1]]) {
                a <- dim(X)[[1]] - 2 * delta
                c <- dim(X)[[1]]
            }
            if (d > dim(X)[[2]]) {
                b <- dim(X)[[2]] - 2 * delta
                d <- dim(X)[[2]]
            }
            Xav[j, k] <- average(X[a:c, b:d], na.rm = TRUE)
        }
    }
 
    ### TREATMENT OF EDGES
    if (edgeNA) {
        Xav[1:(delta), ] <- NA
        Xav[, 1:(delta)] <- NA
        Xav[, (dim(Xav)[[2]] - delta + 1):dim(Xav)[[2]]] <- NA
        Xav[(dim(Xav)[[1]] - delta + 1):dim(Xav)[[1]], ] <- NA
    }
    Xav
}
############################################################################


