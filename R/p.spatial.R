p.spatial <- function (X, delta = 2, N = -1, av = "median", p.adjust.method = "none") 
{
    if (N < 0) {
        N <- 100 * dim(X)[[1]] * dim(X)[[2]]
    }
    XavP <- double(N)

    #### GENERATING EMPIRICAL DISTRIBUTION 
    if (av == "mean") {
        for (i in 1:N) {
            XavP[i] <- mean(sample(as.vector(X), (delta + 1)^2))
        }
    }
    if (av == "median") {
        for (i in 1:N) {
            XavP[i] <- median(sample(as.vector(X), (delta + 1)^2))
        }
    }
    
    #### STATS FOR ORIGINAL DATA
    Xav <- as.vector(ma.matrix(X, delta = delta, av = av))
    Xav.l <- length(Xav)
   
    #### DETERMINING SIGNIFICANCE
    pP <- double(length = length(as.vector(X))) + NA
    XavP.l <- length(XavP)
    for (i in 1:Xav.l) {
        pP[i] <- sum(XavP >= Xav[i], na.rm = TRUE)/XavP.l
    }
    nP <- double(length = length(as.vector(X))) + NA
    for (i in 1:Xav.l) {
        nP[i] <- sum(XavP <= Xav[i], na.rm = TRUE)/XavP.l
    }

    #### ADJUSTMENTS
    pP[pP == 0] <- 1/N
    nP[nP == 0] <- 1/N
    pP.adjust <- p.adjust(pP, method = p.adjust.method)
    nP.adjust <- p.adjust(nP, method = p.adjust.method)
    pP.adjust <- matrix(pP.adjust, ncol = dim(X)[[2]])
    nP.adjust <- matrix(nP.adjust, ncol = dim(X)[[2]])
    list(Pp = pP.adjust, Pn = nP.adjust)
}
#########################################################################


