setGeneric("summary")
setMethod(f = "summary", 
          signature = c(object = ".UD"), 
          definition = function(object) {
            return(list(Raster_proj=object@crs@projargs, Raster_ext=object@extent, Raster_max_val=maxValue(object), Raster_min_val=minValue(object)))
          })

setMethod("summary", 
          signature=".UDStack", 
          definition=function(object){
            lst <- lapply(split(object), summary) ##marco here is a problem with the layernames and the split function
            #names(lst) <- rep(rownames(as.character(object@DBMvar$individual.local.identifier[1])), length(lst))
            return(lst)
          })

setMethod("summary", 
          signature=".MoveTrackSingle", 
          definition=function(object){
            if (!require(circular)) 
              stop("You need to install the circular package to proceed") #angle
            if (!grepl("longlat",proj4string(object))) {
              object <- spTransform(object, CRSobj="+proj=longlat")
              warning("\n The projeciton of the object was changed to \"longlat\" within this function!")}            
            lst <- list()
            lst <- list(distanceSummary(object), timeSummary(object), speedSummary(object), angleSummary(object))
            names(lst) <- rep(rownames(object@idData), length(lst))
            return(lst)
          })

setMethod("summary", 
          signature=".MoveTrackStack", 
          definition=function(object){
            lst <- lapply(split(object), summary)
            return(lst)
          })
