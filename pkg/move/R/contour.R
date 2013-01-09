setGeneric("contour", function(x, ...) standardGeneric("contour"))
setMethod(f = "contour", 
          signature = c(x = ".UD"), 
          definition = function(x, ...) {  
		  x<-getVolumeUD(x)
            callNextMethod()
          })


setMethod(f = "contour", 
          signature = c(x = ".UDStack"), 
          definition = function(x, ...){  
            par(mfrow=1:ceiling(sqrt(length(split(x)))))
            lapply(split(x), contour, ...) 
          })
