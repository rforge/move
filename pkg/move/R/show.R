### Show Method for the data object Move
setMethod("show", ".MoveTrack", function(object){
	  #  print(as(x,"SpatialPointsDataFrame"))
	  getMethod("print","Spatial")(object) 
	  timeRange <- range(object@timestamps)
	  cat("timestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))), " (start...end, duration) \n")  
	  cat("sensors     :",paste(as.character(unique(object@sensor)), collapse=", "),"\n")
	  cat("indiv. data :", paste(collapse=", ", names(object@idData)),"\n")
})
setMethod("show", "dBMvarianceTmp", function(object){
	  cat("margin      :", object@margin)
	  cat("window size :", object@window.size)# bart have to add means usw
})
setMethod("show", ".MoveTrackSingle", function(object){
	  callNextMethod()
	  try(silent=TRUE, if(length(object@timesMissedFixes)>=1)
	      cat("missed fixes :", length(object@timesMissedFixes),"\n") )
})
setMethod("show", ".MoveTrackStack", function(object){
	  callNextMethod()
	  cat("individuals :",paste(as.character(unique(object@trackId)), collapse=", "),"\n")
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
	  cat("bursts      :",paste(as.character(unique(object@burstId)), collapse=", "),"\n")
})
setMethod("show",".MoveGeneral", function(object){
	  if(length(object@license)!=0)
		  cat("license      :",object@license,"\n")
	  if(length(object@citation)!=0)
		  cat("citation     :",object@citation,"\n")
	  if(length(object@study)!=0)
		  cat("study name   :",object@study,"\n")
	  cat("date created:",format(object@dateCreation),"\n")

})
#	 
#	 setMethod("show", "MoveStack", function(object){
#	   print(object) 
#	 })
