setGeneric("seglength", function(x){standardGeneric("seglength")})
setMethod("seglength", 
          signature="SpatialPointsDataFrame",
          definition=function(x){  
            #if (!grepl("longlat",proj4string(x))) x <- spTransform(x, CRSobj="+proj=longlat")
            track <- coordinates(x)
            segM <- cbind(as.data.frame(track)[-nrow(track),], as.data.frame(track)[-1,])
            #if (grepl("longlat",proj4string(x))) {
            if(isLonLat(x)){
              #if proj is long lat the segment lengths are calculated as kilometers on a great circle
              Dists <- as.numeric(apply(segM, 1, function(segM) spDistsN1(as.matrix(t(segM[1:2])), as.matrix(t(segM[3:4])), longlat=T)))
            } else {
              #if proj is not longlat the segmentlength are calculate as (p^2+^2)^1/2
              Dists <- sqrt(rowSums((coordinates(x)[-n.locs(x),]-coordinates(x)[-1,])^2))
            }
            return(Dists)
          })
