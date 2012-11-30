setGeneric("getVolumeUD", function(x,...){standardGeneric("getVolumeUD")})
setMethod("getVolumeUD", 
          signature=c(x="Raster"),
          definition=function(x,...){
            transf <- function(nr){
              rank <- (1:length(values(nr)))[rank(values(nr))]
              values(nr) <- 1 - cumsum(sort(values(nr)))[rank]
              return(nr)
            }            
            if(length(c(x,...))==1) {return(transf(x))} else {return(lapply(list(x, ...), transf))}
            })
# setMethod("getVolumeUD", 
#           signature=c(x="RasterStack"),
#           definition=function(x,...){
#           return(getVolumeUD(unstack(x)))
#             })
# leroydbbmm
# abs(mean(diff(yFromRow(leroydbbmm, 1:10))))*abs(mean(diff(xFromCol(leroydbbmm, 1:10))))*sum(values(newRaster)<=0.95) / (1e6)
