anovaplate <- function (obj, index) 
{
    M <- maM(obj)[, index]
    plate <- maPlate(obj)
    s <- summary(ano <- lm(M ~ plate - 1))
}
##############################################################



