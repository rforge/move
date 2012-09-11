setGeneric("distanceSummary", function(x){standardGeneric("distanceSummary")})
setMethod("distanceSummary", 
          signature=".MoveTrackSingle",
          definition=function(x){ 
            track <- coordinates(x)
            if(nrow(track)>2){
              Dists <- seglength(x)*1000 #vector of all distances in meters
              df <- data.frame(TravDist=sum(Dists))    #travel distance in meters
              df$MaxDist <- max(Dists) #largest distance
              df$MinDist <- min(Dists) #shortest distance
              if (grepl("longlat",proj4string(x))) {
                df$FarthDist <- max(spDistsN1(pts=track,pt=track[1,],longlat=T))*1000
              } else {
                df$FarthDist <- max(spDistsN1(pts=track,pt=track[1,],longlat=F))
              } #farthest distance from the start in meters
              #df$FarthDist <- max(spDistsN1(pts=track,pt=track[1,],longlat=grepl(proj4string(x),"longlat")))*1000 
              df$AverDist <- mean(Dists)    #mean distance between relocations in meters
              df$SDDist <- sd(Dists)      #standard deviation of distances between relocations
              if (grepl("longlat",proj4string(x))) {
                df$SEDist <- max(as.numeric(spDistsN1(pts=t(as.matrix(track[n.locs(x),])),pt=track[1,],longlat=T))*1000)
              } else {
                df$SEDist <- max(as.numeric(spDistsN1(pts=t(as.matrix(track[n.locs(x),])),pt=track[1,],longlat=F)))
              } #start to end straight distance in meters
              return(df)} else {NA}#{warning("Two or less locations.")}
          })

setMethod("distanceSummary", 
          signature=".MoveTrackStack", 
          definition=function(x){
            lst <- lapply(split(x), distanceSummary)
            return(lst)
          })


#setGeneric("distance")#, function(x){standardGeneric("distance")})
setMethod("distance", 
          signature=".MoveTrackSingle",
          definition=function(x){ 
            track <- coordinates(x)
            if(nrow(track)>2){
              Dists <- seglength(x)*1000 #vector of all distances in meters
              } else {Dists <- NA}#{warning("Two or less locations.")}
            return(Dists)
          })

setMethod("distance", 
          signature=".MoveTrackStack", 
          definition=function(x){
            lst <- lapply(split(x), distance)
            return(lst)
          })
