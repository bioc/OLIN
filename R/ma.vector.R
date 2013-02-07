ma.vector <- function (A, M, av = "median", delta = 50) 
{
    Ao <- A[order(A)]
    Mo <- M[order(A)]
    index <- c(1:length(A))
    Io <- index[order(A)]
    Maverage <- double(length(A))
    if (av == "mean") {
        for (i in (delta + 1):(length(A) - delta)) {
            Maverage[i] <- mean(Mo[c((i - delta):(i - 1), (i + 
                1):(i + delta))], na.rm = TRUE)
        }
    }
    if (av == "median") {
        for (i in (delta + 1):(length(A) - delta)) {
            Maverage[i] <- median(Mo[c((i - delta):(i - 1), (i + 
                1):(i + delta))], na.rm = TRUE)
        }
    }
    Maverage[c(1:(delta + 1), (length(Maverage) - delta):length(Maverage))] <- NA
    Maverage[is.na(Mo)] <- NA
    Maverage[order(Io)]
}
##########################################################################




