setGeneric("contour", function(x, ...) standardGeneric("contour"))
setMethod(f = "contour", 
          signature = c(x = ".UD"), 
          definition = function(x, ...) {  
            ## enter nlevel for the number of levels, or levels for the correct levels!!
            newRaster <- x
            # raster2contour(x, ...)
            rank <- (1:length(values(newRaster)))[rank(values(newRaster))]
            values(newRaster) <- 1 - cumsum(sort(values(newRaster)))[rank]
            x <- newRaster
            callNextMethod()
          })


setMethod(f = "contour", 
          signature = c(x = ".UDStack"), 
          definition = function(x, ...){  
            par(mfrow=1:ceiling(sqrt(length(split(x)))))
            lapply(split(x), contour, ...) 
          })
