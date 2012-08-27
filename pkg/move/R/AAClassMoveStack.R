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

setClass(Class = "MoveStack", contains = c(".MoveGeneral",".MoveTrackStack"),
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

