backgroundCorrect2 <- function(object,method="subtract", offset=0){
 require("limma") || stop("Bioconductor package limma needed for this application ")
 require("convert") ||   stop("Bioconductor package convert needed for this application")

 RG <- as(object,"RGList")

 RGb <- backgroundCorrect(RG, method=method, offset=offset, verbose=TRUE)

 object <- as(RGb,"marrayRaw")
 object
} 
