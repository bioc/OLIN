sig.mask <- function(object,Sp,Sn,thrp,thrn){

if (!(class(object)=="marrayRaw") & !(class(object)=="marrayNorm")){
  stop("Object should be of class marrayRaw or marrayNorm")
}

for (i in 1:dim(maM(object))[[2]]){

if (class(object)=="marrayRaw"){
 maRf(object)[Sp[[i]] < thrp[i],i]  <- NA
 maGf(object)[Sp[[i]] < thrp[i],i]  <- NA
 maRb(object)[Sp[[i]] < thrp[i],i]  <- NA
 maGb(object)[Sp[[i]] < thrp[i],i]  <- NA

 maRf(object)[Sn[[i]] < thrn[i],i]  <- NA
 maGf(object)[Sn[[i]] < thrn[i],i]  <- NA
 maRb(object)[Sn[[i]] < thrn[i],i]  <- NA
 maGb(object)[Sn[[i]] < thrn[i],i]  <- NA
}

if (class(object)=="marrayNorm"){
 maM(object)[Sp < thrp[i],i]  <- NA
 maM(object)[Sn < thrn[i],i]  <- NA
}
}
object
}


