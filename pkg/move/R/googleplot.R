setGeneric("googleplot", function(obj, ...){standardGeneric("googleplot")})
setMethod(f = "googleplot", 
          signature = c(obj=".MoveTrack"), 
          function(obj, maptype="terrain",...){
            require(RgoogleMaps)
            lat <-coordinates(obj)[ ,2] 
            lon <- coordinates(obj)[ ,1]
            MyMap <- GetMap.bbox(lonR=range(coordinates(obj)[ ,1]), latR=range(coordinates(obj)[ ,2]), maptype=maptype)
            PlotOnStaticMap(MyMap=MyMap, lon=lon, lat=lat, FUN=lines, ...)
            file.remove(paste(getwd(),"MyTile.png",sep="/"))
            file.remove(paste(getwd(),"MyTile.png.rda",sep="/"))
          })
