anovaint <- function (obj, index, N = 10) 
{
    A <- maA(obj)[, index]
    M <- maM(obj)[, index]
    Ao <- A[order(A)]
    Ao <- Ao[!is.nan(Ao)]
    Mo <- M[order(A)]
    Mo <- Mo[!is.nan(Mo)]
    intensityint <- gl(N, ceiling(length(Mo)/N), length(Mo))
    s <- summary(ano <- lm(Mo ~ intensityint - 1))
}
##############################################################



