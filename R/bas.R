bas <- function(obj,mode="var"){


### BETWEEN ARRAY SCALING BASED ON VARIANCE
if (mode=="var"){
 
 dvar <- apply(maM(obj),2,var,na=TRUE)
 gmdvar <- exp(sum(log(dvar))/length(dvar))
 s <- sqrt(gmdvar/dvar)
 
 M <- t(t(maM(obj)) * s)
 maM(obj) <- M

 obj 
} else{

### BETWEEN ARRAY SCALING BASED ON MAD

if (mode=="mad"){

for (i in 1:5){
 dmad <- apply(maM(obj),2,mad,na=TRUE)
  gmdmad <- exp(sum(log(dmad))/length(dmad))
  s <- sqrt(gmdmad/dmad)
 
 
 M <- t(t(maM(obj)) * s)
 maM(obj) <- M

 obj 
 }
obj
}

### BETWEEN ARRAY SCALING BASED ON QQ-NORMALISATION
  else {if (mode=="qq"){

    Mr <- apply(maM(obj),2,rank)
    Ms <- apply(apply(maM(obj),2,sort,na.last=TRUE),1,mean,na.rm=TRUE)

    tmp <- Ms 

    for (i in 1:dim(maM(obj))[2]){
       Ms <- cbind(Ms,tmp);
    }

    Ms <- Ms[,-1] 

    M <- Ms[Mr[,1],1]

    for (i in 1:dim(maM(obj))[2]){
      M <- cbind(M,Ms[Mr[,i],i]);
    }
     M <- M[,-1]
 
     maM(obj) <- M
     obj
    ## qqnormalisation
    } else {
cat("NO SCALING PERFORMED!\n")
 }
}
}
}









