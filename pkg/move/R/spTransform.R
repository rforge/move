
###Redifining spTransform, because it changes the class of the object to SpatialPointsDataFrame 
setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "missing"), 
          function(x, center=FALSE, ...){
            spTransform(x=x, center=center, CRSobj="+proj=aeqd")
          })

setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "character"), 
          function(x, CRSobj, center=FALSE, ...){
            spTransform(x=x, CRSobj=CRS(CRSobj), center=center)
          })

setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "CRS"), 
          function(x, CRSobj, center=FALSE, ...){
            if (center){ 
              mid.range.lon <- (max(coordinates(x)[ ,1])+min(coordinates(x)[ ,1]))/2
              mid.range.lat  <- (max(coordinates(x)[ ,2])+min(coordinates(x)[ ,2]))/2
              CRSobj <- CRS(paste(CRSobj@projargs," +lon_0=",mid.range.lon," +lat_0=", mid.range.lat, sep=""))
            } 
            
            coordsnew <- spTransform.SpatialPointsDataFrame(x=x, CRSobj=CRSobj)
            x <- new(class(x), coordsnew,x )
            return(x)
          })

# if (!isGeneric("SpatialLines")) {
# setGeneric("SpatialLines", function(LinesList) standardGeneric("SpatialLines"))
# }
# 
# #transform Move's spatialpoints to spatiallines
#  setMethod("SpatialLines", "Move", function(LinesList){
#            xy <- (coordinates(LinesList))
#            xyLine <- Line(xy)
#            if (length(LinesList@animal)!=0){id <- LinesList@animal} else {id <- "noID"}
#            #xyLines <- Lines(list(xyLine), ID=LinesList@animal)
#            xyLines <- Lines(list(xyLine), ID="a")
#            #return(SpatialLines(list(xyLines), proj4string=CRS("+proj=aeqd +ellps=WGS84")))
#            return(SpatialLines(list(xyLines), proj4string=CRS(proj4string(LinesList))))
#            }
#            )
