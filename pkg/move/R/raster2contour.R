# return the contour as SLDF object: plot=F, google=F if (plot==F && google==F
# && track==F){ raster2contour(x,...) return(newRaster) } else { browser() plot
# the contour line: plot=T, google=F if (plot==T && google==F){ contour(x =
# newRaster, add = add, ...)  } add track to contour+google map: plot=T,
# google=T if (google==T){ print(newRaster) if (class(y)!='Move'){stop('y must
# be the corresponding Move object to the DBBMM object')} if
# (grepl(pattern='longlat', x=proj4string(y))==FALSE){stop('Use a Move object
# with longlat projection method')} require(RgoogleMaps) lon <-
# coordinates(y)[,1] lat <- coordinates(y)[,2] lonRange <-
# c(min(range(lon))-abs(diff(range(lon))*x@ext[1]),
# max(range(lon))+abs(diff(range(lon))*x@ext[2])) latRange <-
# c(min(range(lat))-abs(diff(range(lat))*x@ext[3]),
# max(range(lat))+abs(diff(range(lat))*x@ext[4])) MyMap <-
# GetMap.bbox(lonR=lonRange, latR=latRange) # MyMap <-
# GetMap.bbox(lonR=range(coordinates(y)[ ,1]), latR=range(coordinates(y)[ ,2]))
# rst1 <- raster2contour(x, ...)  sldf <-
# spTransform.SpatialLinesDataFrame(rst1, CRSobj=CRS('+proj=longlat')) add <- F
# for (i in 1:length(sldf@lines)){ for (j in 1:length(sldf@lines[[i]]@Lines)){
# sldflon <- sldf@lines[[i]]@Lines[[j]]@coords[,1] sldflat <-
# sldf@lines[[i]]@Lines[[j]]@coords[,2] PlotOnStaticMap(MyMap=MyMap,
# lon=sldflon, lat=sldflat, FUN=lines,add=add, col=col, lwd=lwd) add <- T } }
# #add a track: track=T if (track==T) { PlotOnStaticMap(MyMap=MyMap,
# lon=coordinates(y)[,1], lat=coordinates(y)[,2], add=TRUE, FUN=lines,
# lwd=llwd, col=lcol) } file.remove(paste(getwd(),'MyTile.png',sep='/'))
# file.remove(paste(getwd(),'MyTile.png.rda',sep='/')) } }})

### Contour to SpatialLinesDataFrame conversion if
### (!isGeneric('outerProbability')){
setGeneric("raster2contour", function(x, ...) {standardGeneric("raster2contour")})  #bart kami do we want to skip maxpixels? if not than there would be no reason to have a different function name to rasterToContour
# }
setMethod(f = "raster2contour", 
          signature = c(x = "RasterLayer"),
          #signature = c(x = "Raster"),
          definition = function(x, ...) {
            rank <- (1:length(values(x)))[rank(values(x))]
            values(x) <- 1 - cumsum(sort(values(x)))[rank]
            rasterToContour(x, ...)
          })
