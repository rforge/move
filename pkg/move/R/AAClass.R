setClassUnion(".OptionalPOSIXct", c("POSIXct","NULL"))

setClass(Class = ".MoveGeneral",
         representation = representation(
           dateCreation = "POSIXct",
           study = "character",
           citation = "character",
           license = "character"),
         prototype = prototype(
           dateCreation = Sys.time(),
           study = as.character(),
           citation = as.character(),
           license = as.character()),
         validity = function(object){
           #if(length(object@study)>1)
           #		stop("Study has length unequal to 0 or 1")
           if(length(object@citation)>1)
             stop("Citation has length unequal to 0 or 1")
           if(length(object@license)>1)
             stop("License has length unequal to 0 or 1")
           return(TRUE)
         }
         )

setClass(Class = ".MoveTrack",contains=c("SpatialPointsDataFrame"),
         representation = representation(
           timestamps = "POSIXct",
           idData = "data.frame",
           sensor="factor"),
         prototype = prototype(
           timestamps = as.POSIXct(NA),
           idData = data.frame(),
           sensor=factor()),
         validity = function(object){
           if(length(object@timestamps)!=nrow(object@coords))
             stop("Number of timestamps does not match the number of coordinates")
           if(length(object@sensor)!=nrow(object@coords))
             stop("Number of sensors observations does not match the number of coordinates")
           return(TRUE)
         }
         )


setClass(Class = ".MoveTrackSingle",contains=c(".MoveTrack"), ##why are no missed fixes stored for a MoveStack?
         representation = representation (
           timesMissedFixes = ".OptionalPOSIXct"),
         #  burstID = "factor", ##to indicate the bursts of a single track
         #  burstSPDF = "SpatialPointsDataFrame"),
         prototype = prototype(
           timesMissedFixes = NULL),
         # burstID = as.factor(NA)),
         validity = function(object){
           if(any(object@timestamps!=sort(object@timestamps)))
             stop("The dataset includes unsorted time stamps")
           if (any(dups<-duplicated(data.frame(format(object@timestamps,"%Y %m %d %H %M %OS3"), object@sensor))))
             stop("The dataset includes double timestamps (first one:",object@timestamps[dups][1],")")
           if(nrow(object@idData)>1)
             stop("There are more than 1 row stored in the idData")
           #if (length(object@burstID)!=nrow(coordinates(object)))
           #stop("The length of burst IDs is not equal to the number of locations")
           return(TRUE)
         }
         )

setClass(Class = "Move", contains=c(".MoveTrackSingle",".MoveGeneral"),
         representation = representation (
           ),
         prototype = prototype(
           ),
         validity = function(object){
           return(TRUE)
         }
         )


setAs(".MoveTrack","data.frame", function(from){return(data.frame(data.frame(from), sensor=from@sensor, timestamps=from@timestamps))})



setClass(Class = ".MoveTrackStack", contains = c(".MoveTrack"),
         representation = representation(
           trackId = "factor"),
         prototype = prototype(
           trackId = factor()),
         validity = function(object){
           if(length(object@trackId)!=nrow(object@coords))
             stop("Number of trackId does not match the number of coordinates")
           if(any(tmp<-duplicated(cbind(object@timestamps, object@trackId, object@sensor))))
             stop("The data set includes double timestamps per ID (first one:", object@trackId[tmp][1]," ",object@sensor[tmp][1]," ",object@timestamps[tmp][1], ")")
           if(any(unlist(lapply(tapply(object@timestamps,object@trackId, order),diff))!=1))
             stop("Not ordered timestamps per individual")
           return(TRUE)
         }
         )

setAs( ".MoveTrackStack","data.frame", function(from){ return(data.frame(as(as(from,".MoveTrack"),"data.frame"), trackId=from@trackId))})

setClass(Class = "MoveStack", contains = c(".MoveTrackStack",".MoveGeneral"),
         representation = representation(),
         prototype = prototype(),
         validity = function(object){
           if(length(unique(object@trackId))!=nrow(object@idData))
             stop("Not same number of IDs and rows in dataframe per ID")
           if(any(sort(as.character(unique(object@trackId)))!=sort(unique(rownames(object@idData))))){
             stop("No match between rownames in idData and ids along track")} 
           return(TRUE)
         }
         )


# Defining the class of the Dynamic Brownian Motion Variance object
# setClass(Class = 'DBMvar', representation = representation ( means = 'array',
# in.windows = 'array', interest = 'logical', break.list = 'ANY') )


setClass(Class = "dBMvarianceTmp", 
         representation = representation(
           window.size = "numeric", 
           margin = "numeric", 
           means = "numeric", 
           in.windows = "numeric", 
           interest = "logical", 
           break.list = "numeric"), 
         prototype = prototype(
           window.size = numeric(), 
           margin = numeric(), 
           means = numeric(), 
           in.windows = numeric(), 
           interest = logical(), 
           break.list = numeric()), 
         validity = function(object) {
           if (length(unique(c(length(object@means), length(object@in.windows), length(object@interest)))) != 1) 
             stop("Length does not match")
           if (length(object@margin) != 1) 
             stop("Margin length not 1")
           if (length(object@window.size) != 1) 
             stop("Window size length not 1")
           return(TRUE)
         })

setClass(Class = "dBMvariance", contains = c(".MoveTrackSingle", "dBMvarianceTmp"), 
         validity = function(object) {
           if (length(object@means) != nrow(object@coords)) 
             stop("Number of coordinates does not match the number of means")
           return(TRUE)
         })


setClass(Class = "dBMvarianceStack", contains = c(".MoveTrackStack", "dBMvarianceTmp"), 
         validity = function(object) {
           if (length(object@means) != nrow(object@coords)) 
             stop("Number of coordinates does not match the number of means")
           return(TRUE)
         })


setClass(Class = ".UDStack", contains = c("RasterStack"), 
         representation = representation(method = "character"), 
         prototype = prototype(
           method = as.character()), 
         validity = function(object) {
           if (!all(apply(values(object), MARGIN = 2, FUN = function(X) isTRUE(all.equal(sum(X), 1, check.attributes=F))))) 
             stop("One or more of the used rasters are not a UD (sum is not equal to 1)")
         })

setClass(Class = ".UD", contains = c("RasterLayer"), 
         representation = representation(method = "character"), 
         prototype = prototype(
           method = as.character()), 
         validity = function(object) {
           if (!isTRUE(all.equal(tmp<-sum(values((object))), 1))) 
             stop("The used raster is not a UD (sum unequal to 1), sum is: ", sprintf("%.15f",tmp))
           return(TRUE)
         })

### Defining the class of the Brownian Bridge Movement Model object
setClass(Class = "DBBMMStack", contains = c(".UDStack"), 
         representation = representation(
           DBMvar = "dBMvarianceStack", 
           ext = "numeric"), 
         prototype = prototype(
           ext = as.numeric()), 
         validity = function(object) {
           if (!all(unique(object@DBMvar@trackId) == object@layernames)) 
             stop("The layer names of the raster objects do not match the trackIDs of the DBMvarStack.")
         })

setClass(Class = "DBBMM", contains = c(".UD"), 
         representation = representation(
           DBMvar = "dBMvariance", 
           ext = "numeric"), 
         prototype = prototype(ext = as.numeric()))

setClass(Class = ".MoveTrackSingleBurst", contains = c(".MoveTrackSingle"), 
         representation = representation(
           burstId = "factor"), 
         prototype = prototype(
           burstId = factor()), 
    validity = function(object) {
	    if(length(object@burstId)!=(length(object@timestamps)-1))
		    stop("Burst ids need to be one shorter than rest since it is a segment property")
        return(TRUE)
      })

setClass(Class = "MoveBurst", contains = c(".MoveTrackSingleBurst", ".MoveGeneral"), 
    validity = function(object) {
        return(TRUE)
    })

# See if this validity check needs to go into this class why was it commented ?
# validity = function(object){ if(is.na(outerProbability(object))){ stop('The
# used extent is too large. Choose a smaller value for ext!')# when did this
# occure Marco?  } else { if (outerProbability > .01){ warning('outer
# probability: ', outerProbability,' The used extent is too small. Choose an
# extent which includes more of the probabilities.') } } }
