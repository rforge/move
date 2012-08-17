setGeneric("summary")
setMethod(f = "summary", 
          signature = c(object = ".UD"), 
          definition = function(object) {
            return(list(Raster_proj=object@crs@projargs, Raster_ext=object@extent, Raster_max_val=maxValue(object), Raster_min_val=minValue(object)))
          })

setMethod("summary", 
          signature=".UDStack", 
          definition=function(object){
            lst <- lapply(split(object), summary)
            names(lst) <- rep(rownames(as.character(dbbmm@DBMvar$individual.local.identifier[1])), length(lst))
            return(lst)
          })

setMethod("summary", 
          signature=".MoveTrackSingle", 
          definition=function(object){
            if (any(grepl('maptools', installed.packages()))) require(maptools) else stop("You need to install the maptools package to proceed") #angle
            if (any(grepl('circular', installed.packages()))) require(circular) else stop("You need to install the maptools package to proceed") #angle
            if (!grepl("longlat",proj4string(object))) {
              object <- spTransform(object, CRSobj="+proj=longlat")
              warning("\n The projeciton of the object was changed to \"longlat\" within this function!")}            
            lst <- list()
            lst <- list(distance(object), time(object), speed(object), angle(object))
            names(lst) <- rep(rownames(object@idData), length(lst))
            return(lst)
          })

setMethod("summary", 
          signature=".MoveTrackStack", 
          definition=function(object){
            lst <- lapply(split(object), summary)
            return(lst)
          })

setGeneric("distance", function(x,values=FALSE){standardGeneric("distance")})
setMethod("distance", 
          signature=".MoveTrackSingle",
          definition=function(x,values){ 
            track <- coordinates(x)
            if(nrow(track)>2){
              Dists <- seglength(x) #vector of all distances in meters
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
              if(values) return(Dists) else return(df)
            } else {NA}#{warning("Two or less locations.")}
          })

setMethod("distance", 
          signature=".MoveTrackStack", 
          definition=function(x,values){
            lst <- lapply(split(x), distance,values=values)
            return(lst)
          })

setGeneric("time", function(x){standardGeneric("time")})
setMethod("time", 
          signature=".MoveTrackSingle",
          definition=function(x){           
            date <- timestamps(x)
            TimeDiff <- time.lag(x) #time differences in minutes
            #             out <- boxplot(seglength(x)/(as.numeric(TimeDiff+0.0000001)), range=10, plot=F)$out ##marco there must be an easier way than this
            #             if(length(out)!=0) x@idData$out <- out else x@idData$out <- NA
            #             x@idData$outliners <- length(out[out > median(out)])>0   #check whether there are strong outliers in speed
            df <- data.frame(Duration=difftime(date[length(date)], date[1], units="hours")) #total duration of the track in hours
            df$AverDur <- mean(TimeDiff)       #mean time difference between relocations
            df$SDDur <- sd(TimeDiff)           #standard deviation of time differences between relocations
            df$dupl <- any((TimeDiff)<(1/3600))            #check whether any two relocations are closer than a second to each other
            df$multseason <-  any(TimeDiff > (24*30))      #check whether any two subsequent relocations are more than one month apart
            return(df)
          })

setMethod("time", 
          signature=".MoveTrackStack", 
          definition=function(x){
            lst <- lapply(split(x), time)
            return(lst)
          })

setGeneric("speed", function(x,values=FALSE){standardGeneric("speed")})
setMethod("speed", 
          signature=".MoveTrackSingle",
          definition=function(x,values){
            if(length(seglength(x)>0)){
              Speed <- (seglength(x)*1000)/time.lag(x) #meter per min
              df  <- data.frame(AverSpeed=mean(Speed, na.rm=TRUE))
              df$VarSpeed <- var(Speed, na.rm=T)
              df$MaxSpeed <- max(Speed)
              if(values) return(Speed) else return(df)} else {NA}
          })

setMethod("speed", 
          signature=".MoveTrackStack", 
          definition=function(x,values){
            lst <- lapply(split(x), speed,values=values)
            return(lst)
          })


setGeneric("angle", function(x,values=FALSE){standardGeneric("angle")})
setMethod("angle", 
          signature=".MoveTrackSingle",
          definition=function(x,values){
            if(nrow(coordinates(x))>=3){
              if (any(grepl('maptools', installed.packages()))) require(maptools) else stop("You need to install the maptools package to proceed") #trackAzimuth
              if (any(grepl('circular', installed.packages()))) require(circular) else stop("You need to install the maptools package to proceed") #var.circular
              if (!grepl("longlat",proj4string(x))) x <- spTransform(x, CRSobj="+proj=longlat")
              tAzimuth <- trackAzimuth(coordinates(x))
              df <- data.frame(AverAzimuth=as.numeric(mean.circular(circular(tAzimuth,units="degrees"),na.rm=T)))
              df$VarAzimuth <- as.numeric(var(circular(tAzimuth,units="degrees"),na.rm=T) )
              df$SEAzimuth <- bearing(round(coordinates(x)[1,],5), round(coordinates(x)[nrow(coordinates(x)),],5))  #start to end straight Azimuth
              if(values) return(tAzimuth) else return(df)} else {NA}
          })

setMethod("angle", 
          signature=".MoveTrackStack", 
          definition=function(x,values){
            lst <- lapply(split(x), angle, values=values)
            return(lst)
          })