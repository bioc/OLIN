.First.lib <- function (libname, pkgname, where) 
{
    require("methods") || stop("R-package methods needed")
    require("stats") ||   stop("R-package modreg needed")
    require("locfit") ||  stop("R-package locfit needed")
    require("Biobase") || stop("Bioconductor-package Biobase needed")
    require("marray") ||  stop("Bioconductor-package marray needed")
}



