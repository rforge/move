### Show Method for the data object Move
#setGeneric("show"), function(object) standardGeneric("show")) ##marco we need our own show function, because raster uses just cat which does not allow line breaks!!
setMethod("show", ".MoveTrack", function(object){
	  #  print(as(x,"SpatialPointsDataFrame"))
	  getMethod("print","Spatial")(object,fill=T) 
	  timeRange <- range(object@timestamps)
	  #cat("\ntimestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))),fill=T)  
	  cat("\ntimestamps  :", timeRange, sep=", ", capture.output(round(difftime(timeRange[2],timeRange[1]))),fill=T)
	  #cat("\nsensors     :",paste(as.character(unique(object@sensor)), collapse=", "),fill=T)
	  cat("\nsensors     :", as.character(unique(object@sensor)), sep=", ",fill=T)
	  #cat("\nindiv. data :", paste(collapse=", ", names(object@idData)),fill=T)
	  cat("\nindiv. data :", sep=", ", names(object@idData),fill=T)
})
setMethod("show", "dBMvarianceTmp", function(object){
	  cat("\nmargin      :", object@margin)
	  cat("\nwindow size :", object@window.size)# bart have to add means usw
})
setMethod("show", ".MoveTrackSingle", function(object){
	  callNextMethod()
	  try(silent=TRUE, if(length(object@timesMissedFixes)>=1)
	      cat("\nmissed fixes :", length(object@timesMissedFixes),fill=T) )
})
setMethod("show", ".MoveTrackStack", function(object){
	  callNextMethod()
	  #cat("\nindividuals :",paste(as.character(unique(object@trackId)), collapse=", "),fill=T)
	  cat("\nindividuals :",as.character(unique(object@trackId), sep=", "),fill=T)
})
setMethod("show", "dBMvariance", function(object){
	  callNextMethod()
	  show(as(object,"dBMvarianceTmp"))
})
setMethod("show", "Move", function(object){
	  callNextMethod()
	  show(as(object,".MoveGeneral"))
})
setMethod("show", "MoveBurst", function(object){
	  callNextMethod()
	  show(as(object,".MoveGeneral"))
})
setMethod("show", "MoveStack", function(object){
	  callNextMethod()
	  show(as(object,".MoveGeneral"))
})
setMethod("show",".MoveTrackSingleBurst", function(object){
	  callNextMethod()
	  #cat("\nbursts      :",paste(as.character(unique(object@burstId)), collapse=", "),fill=T)
	  cat("\nbursts      :",as.character(unique(object@burstId)), sep=", ",fill=T)
})
setMethod("show",".MoveGeneral", function(object){
	  if(length(object@license)!=0)
		  cat("\nlicense      :",object@license,fill=T)
	  if(length(object@citation)!=0)
		  cat("\ncitation     :",object@citation,fill=T)
	  if(length(object@study)!=0)
		  cat("\nstudy name   :",object@study,fill=T)
	  cat("\ndate created:",format(object@dateCreation),fill=T)

})
#	 
#	 setMethod("show", "MoveStack", function(object){
#	   print(object) 
#	 })
