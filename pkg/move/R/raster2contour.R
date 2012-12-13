### Contour to SpatialLinesDataFrame conversion if
### (!isGeneric('outerProbability')){
setGeneric("raster2contour", function(x, ...) {standardGeneric("raster2contour")})  
# }
setMethod(f = "raster2contour", 
          signature = c(x = "RasterLayer"),
          definition = function(x, ...) {
            rank <- (1:length(values(x)))[rank(values(x))]
            values(x) <- 1 - cumsum(sort(values(x)))[rank]
            rasterToContour(x, ...)
          })
