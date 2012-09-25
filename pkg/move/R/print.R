###Print function for a Move and MoveStack object
setGeneric("print")
setMethod("print", ".MoveTrack", function(x){
	  #  print(as(x,"SpatialPointsDataFrame"))
	  getMethod("print","Spatial")(x) 
	  timeRange <- range(x@timestamps)
	  cat("timestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))), " (start...end, duration) \n")  
	  cat("sensors     :",paste(as.character(unique(x@sensor)), collapse=", "),"\n")
	  cat("indiv. data :", paste(collapse=", ", names(x@idData)),"\n")
})
setMethod("print", "dBMvarianceTmp", function(x){
	  cat("margin      :", x@margin,'\n')
	  cat("window size :", x@window.size,'\n')# bart have to add means usw
})
setMethod("print", ".MoveTrackSingle", function(x){
	  callNextMethod()
	  try(silent=TRUE, if(length(x@timesMissedFixes)>=1)
	      cat("missed fixes:", length(x@timesMissedFixes),"\n") )
})
setMethod("print", ".MoveTrackStack", function(x){
	  callNextMethod()
	  cat("individuals :",paste(as.character(unique(x@trackId)), collapse=", "),"\n")
})
setMethod("print", "dBMvariance", function(x){
	  callNextMethod()
	  print(as(x,"dBMvarianceTmp"))
})
setMethod("print", "Move", function(x){
	  callNextMethod()
	  print(as(x,".MoveGeneral"))
})
setMethod("print", "MoveBurst", function(x){
	  callNextMethod()
	  print(as(x,".MoveGeneral"))
})
setMethod("print", "MoveStack", function(x){
	  callNextMethod()
	  print(as(x,".MoveGeneral"))
})
setMethod("print",".MoveTrackSingleBurst", function(x){
	  callNextMethod()
	  cat("bursts      :",paste(as.character(unique(x@burstId)), collapse=", "),"\n")
})
setMethod("print",".MoveGeneral", function(x){
	  if(length(x@license)!=0)
		  cat("license      :",x@license,"\n")
	  if(length(x@citation)!=0)
		  cat("citation     :",x@citation,"\n")
	  if(length(x@study)!=0)
		  cat("study name   :",x@study,"\n")
	  cat("date created:",format(x@dateCreation),"\n")

})
# setMethod("print",".MoveTrackStack",function(x){
#           callNextMethod(x)
#           if (exists("study.name",x@idData)==TRUE){
#             cat("study name  :",levels(x@idData$study.name),"\n",...)}
#           if (exists("individual.taxon.canonical.name", where=x@idData)==TRUE){
#             cat("species     :",as.character(unique(x@idData$individual.taxon.canonical.name)),"\n",...)}
#           cat("no. of indiv:",nlevels(x@trackId),"\n",...)
#           cat("indiv. ids  :",paste(levels(x@trackId),collapse=", "),"\n",...)
#           pp <- split(x@coords,x@trackId)
#           cat("no. of fixes:",unlist(lapply(pp,length)),"\n",...)
#           }
#           )
# 
# setMethod("print","MoveStack",
#           function(x){
#             callNextMethod(x)
#             #if (exists("sensor.type", where=x@idData)==TRUE){
#             #  cat("sensor type :",levels(x@idData$sensor.type),"\n",...)}
#             maxItems <- 10  
#             items <- ncol(x@idData)
#             if (items > maxItems) { 
#               coln <- colnames(x@idData)
#               coln <- c(coln[1:maxItems], '...')
#             } else {coln <- colnames(x@idData)}
#               cat("indiv. attr.:", paste(coln, collapse=", "), "\n",...)
#           }
#           )

#		setMethod("print",".MoveTrackSingle", function(x,...){
#		            #callNextMethod(x)
#		            cat("Class        :", class(x),...)
#		            try(cat("\nname         :", rownames(x@idData),...),silent=T)
#		            cat("\nnfeatures    :", nrow(coordinates(x)),...)
#		            cat("\nextent       :", c(extent(x)@xmin, extent(x)@xmax, extent(x)@ymin, extent(x)@ymax),...)
#		            cat("\ncoord.ref    :", proj4string(x),...)
#		            cat("\nndatacols    :", ncol(x@data),...)
#		            #cat("variables    :", paste(colnames(x@data),collapse=", "), "\n",...)
#		            cat("\nvariables    :", colnames(x@data), sep=", ",...)
#		            timeRange <- range(x@timestamps)
#		            #cat("timestamps   :",paste(timeRange, collapse="..."),"\n",...)
#		            cat("\ntimestamps   :", timeRange, sep=", ",...)
#		            cat("\nduration     :", capture.output(round(difftime(timeRange[2],timeRange[1]))), ...)
#		            try(silent=TRUE, if(length(x@timesMissedFixes)>=1)
#		              cat("\nmissed fixes :", length(x@timesMissedFixes)) )
#		          }
#		          )
#		
#		# setMethod("print", ".MoveTrack", function(x){
#		#   print(as(x[x@trackId==ID,],"SpatialPointsDataFrame"))
#		#   callNextMethod(x)
#		#   timeRange <- range(x@timestamps)
#		#   cat("timestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))), " (start...end, duration) \n",...)  
#		# }       
#		# )
#		
#		##Print function for a Move and MoveStack object
#		setMethod("print",".MoveTrackStack",function(x,...){
#		  #callNextMethod(x)
#		  cat("Class        :", class(x),...)
#		  cat("\nnfeatures    :", length(coordinates(x)[,1]),...)
#		  cat("\nextent       :", c(extent(x)@xmin, extent(x)@xmax, extent(x)@ymin, extent(x)@ymax),...)
#		  cat("\ncoord.ref    :", proj4string(x),...)
#		  cat("\nndatacols    :", ncol(x@data),...)
#		  #cat("variables    :", paste(colnames(x@data),collapse=", "), "\n",...)
#		  cat("\nvariables    :", colnames(x@data), sep=", ", ...)
#		  if (exists("study.name",x@idData)==TRUE){
#		    cat("\nstudy name   :",levels(x@idData$study.name),...)}
#		  if (exists("individual.taxon.canonical.name", where=x@idData)==TRUE){
#		    cat("\nspecies      :",as.character(unique(x@idData$individual.taxon.canonical.name)),...)}
#		  cat("\nno. of indiv :",nlevels(x@trackId),...)
#		  #cat("indiv. ids   :",paste(levels(x@trackId),collapse=", "),"\n",...)
#		  cat("\nindiv. ids   :", levels(x@trackId),sep=", ", ...)
#		  pp <- split(x@coords[,1],x@trackId)
#		  cat("\nno. of fixes :",unlist(lapply(pp,length)),...)
#		})
#		
#		setMethod("print","MoveStack", function(x,...){
#		            callNextMethod(x)
#		            try(silent=TRUE, if(length(x@timesMissedFixes)>1)
#		              cat("\nmissed fixes  :", length(x@timesMissedFixes)) )
#		            if (exists("sensor.type", where=x@idData)==TRUE){
#		              cat("\nsensor type  :",levels(x@idData$sensor.type),...)}
#		            maxItems <- 10  
#		            items <- ncol(x@idData)
#		            if (items > maxItems) { 
#		              coln <- colnames(x@idData)
#		              coln <- c(coln[1:maxItems], '...')
#		            } else {coln <- colnames(x@idData)}
#		            #cat("indiv. attr. :", paste(coln, collapse=", "), "\n",...)
#		            cat("\nindiv. attr. :", coln, sep=", ",...)
#		            timeRange <- range(x@timestamps)
#		            #cat("timestamps   :",paste(timeRange, collapse="..."),"\n",...)
#		            cat("\ntimestamps   :",timeRange, sep=", ",...)
#		            cat("\nduration     :", capture.output(round(difftime(timeRange[2],timeRange[1]))),...)
#		          })
