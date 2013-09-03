setGeneric("angleSummary", function(x){standardGeneric("angleSummary")})
setMethod("angleSummary", 
          signature=".MoveTrackSingle",
          definition=function(x){
            if(nrow(coordinates(x))>=3){
              if (!require(circular)) 
                stop("You need to install the circular package to proceed") #var.circular
              if (!grepl("longlat",proj4string(x))) x <- spTransform(x, CRSobj="+proj=longlat")
              tAzimuth <- bearing(coordinates(x)[-n.locs(x), ], coordinates(x)[-1, ])
              df <- data.frame(AverAzimuth=as.numeric(mean.circular(circular(tAzimuth,units="degrees"),na.rm=T)))
              df$VarAzimuth <- as.numeric(var(circular(tAzimuth,units="degrees"),na.rm=T) )
              df$SEAzimuth <- bearing(round(coordinates(x)[1,],5), round(coordinates(x)[nrow(coordinates(x)),],5))  #start to end straight Azimuth
              return(df)} else {NA}
          })

setMethod("angleSummary", 
          signature=".MoveTrackStack", 
          definition=function(x){
            lst <- lapply(split(x), angleSummary)
            return(lst)
          })


setGeneric("angle", function(x){standardGeneric("angle")})
setMethod("angle", 
          signature=".MoveTrackSingle",
          definition=function(x){
            if(nrow(coordinates(x))>=3){
              if (!isLonLat(x)) x <- spTransform(x, CRSobj="+proj=longlat")
              tAzimuth <- geosphere::bearing(coordinates(x)[-n.locs(x), ], coordinates(x)[-1, ])
              return(tAzimuth)} else {NA}
          })

setMethod("angle", 
          signature=".MoveTrackStack", 
          definition=function(x){
            lst <- lapply(split(x), angle)
            return(lst)
          })
