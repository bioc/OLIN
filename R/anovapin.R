anovapin <- function (obj, index) 
{
    M <- maM(obj)[, index]
    pin <- gl(maNgc(obj)* maNgr(obj), maNsc(obj)*maNsr(obj))
    s <- summary(ano <- lm(M ~ pin - 1))
}
##############################################################



