setGeneric("lineMidpoint", function(object){standardGeneric("lineMidpoint")})
setMethod(f = "lineMidpoint",
          signature="SpatialPointsDataFrame",
          definition=function(object){
            track <- coordinates(object)
            if (!grepl("longlat",proj4string(object))) stop(paste("Coordinates need longlat projection! Projection is:",proj4string(object)))
            if (nrow(track)<2) { ##make the point at the coordinate
              mid <- t(coordinates(track))
            } else {
              if (nrow(track)==2) { ##make the point at the center of the line
                mid <- (coordinates(track)[2,]-coordinates(track)[1,])*.5+coordinates(track)[1,]
              }
              if (nrow(track)>2){
                dists <- seglength(object)
                
                totalDist <- sum(dists)
                cumsum <- cumsum(dists)
                lcum <- list(cumsum)
                
                min <- which(cumsum(dists)>0.5*sum(dists))[1]
                prop <- (cumsum(dists)[min]-sum(dists)/2)/cumsum(dists)[min]
                mid <- (coordinates(track)[min+1,]-coordinates(track)[min,])*prop+coordinates(track)[min,]
              }
            }
            midSP <- SpatialPoints(coords=t(as.data.frame(x=mid, row.names=NULL)), proj4string=CRS("+proj=longlat"))
            #return(midSP)
          })
