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
		  	    if (any(dups<-duplicated(data.frame(object@timestamps, object@sensor))))
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
