setClass(Class = ".MoveTrackStack", contains = c(".MoveTrack"),
	       representation = representation(trackId = "factor"),
	       prototype = prototype(trackId = factor()),
	       validity = function(object){
    			if(length(object@trackId)!=nrow(object@coords))
    				stop("Number of trackId does not match the number of coordinates")
    		  	if(any(duplicated(cbind(object@timestamps, object@trackId))))
    				stop("The data set includes double timestamps per ID")
    			if(any(unlist(lapply(tapply(object@timestamps,object@trackId, order),diff))!=1))
    				stop("Not ordered timestamps per individual")
    			return(TRUE)
    	 	}
        )

setClass(Class = "MoveStack", contains = c(".MoveGeneral",".MoveTrackStack"),
       	 representation = representation(idData = "data.frame"),
       	 prototype = prototype(idData = data.frame()),
      	 validity = function(object){
    			if(length(unique(object@trackId))!=nrow(object@idData))
    				stop("Not same number of IDs and rows in dataframe per ID")
    			if(any(sort(as.character(unique(object@trackId)))!=sort(unique(rownames(object@idData)))))
			{browser()
				stop("No match between rownames in idData and ids along track")}
    			return(TRUE)
    		}
        )
setMethod('[', signature(x="MoveStack"),definition=function(x,i,j,drop){
	  new('MoveStack', as(x, "SpatialPointsDataFrame")[i,], 
	      trackId=droplevels(x@trackId[i]),
	      idData=x@idData[as.character(unique(x@trackId[i])),],
	      timestamps=x@timestamps[i])})
	      



#setGeneric("move", function(x, y, time, data, proj, ...) standardGeneric("move"))
setGeneric("moveStack", function(x, y, time, data, proj, ...) standardGeneric("moveStack"))
setMethod(f="moveStack", 
	        signature=c(x="character"), 
	        definition = function(x, proj){
		        df <- read.csv(x, header=TRUE, sep=",", dec=".")
		        #check whether data are really from movebank
		        if (!all(c("timestamp","location.long", "location.lat","study.timezone","study.local.timestamp","sensor.type","individual.local.identifier","individual.taxon.canonical.name")%in%colnames(df)))
		        stop("The specified file does not seem to be from Movebank. Please use the alternative import function.")
      		df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC") 
      		df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format="%Y-%m-%d %H:%M:%OS"))            
      		missedFixes <- df[(is.na(df$location.long)|is.na(df$location.lat)), ]
      		df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ]
            
      		# p is a vector with unique variables of the individual
      		p <- unlist(lapply(lapply(lapply(lapply(apply(df, 2, tapply, df$individual.local.identifier, unique), lapply, length),unlist),'==',1),all))
            
      		tmp <- SpatialPointsDataFrame(
      		      	coords = cbind(df$location.long,df$location.lat),
      		      	data = data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long","timestamp","individual.local.identifier", names(p)[p])]]), 
      		      	proj4string = CRS("+proj=longlat +ellps=WGS84"), # proj is not used here
      		      	match.ID = TRUE)
      		idData <- df[!duplicated(df$individual.local.identifier),names(p)[p]]
      		rownames(idData) <- idData$individual.local.identifier
      		
          res <- new("MoveStack", 
      		        tmp, 
      		        idData = idData,
      		        timestamps = df$timestamp, 
      		        trackId = df$individual.local.identifier
      		        )
            
      		return(res)
      	  }
      	  )


setGeneric("citation", function(obj) standardGeneric("citation"))
setMethod("citation", ".MoveGeneral", function(obj){
  return(obj@citation)
}
)

##Print function for a Move and MoveStack object
setGeneric("print")
setMethod("print",".MoveTrackStack",function(x){
          callNextMethod(x)
          if (exists("study.name",x@idData)==TRUE){
            cat("study name  :",levels(x@idData$study.name),"\n")}
          if (exists("individual.taxon.canonical.name", where=x@idData)==TRUE){
            cat("species     :",as.character(unique(x@idData$individual.taxon.canonical.name)),"\n")}
          cat("no. of indiv:",nlevels(x@trackId),"\n")
          cat("indiv. ids  :",paste(levels(x@trackId),collapse=", "),"\n")
          pp <- split(x@coords,x@trackId)
          cat("no. of fixes:",unlist(lapply(pp,length)),"\n")
          }
          )

setMethod("print","MoveStack",
          function(x){
            callNextMethod(x)
            if (exists("sensor.type", where=x@idData)==TRUE){
              cat("sensor type :",levels(x@idData$sensor.type),"\n")}
            maxItems <- 10  
            items <- ncol(x@idData)
            if (items > maxItems) { 
              coln <- colnames(x@idData)
              coln <- c(coln[1:maxItems], '...')
            } else {coln <- colnames(x@idData)}
              cat("indiv. attr.:", paste(coln, collapse=", "), "\n")
          }
          )
setMethod("show", "MoveStack", function(object){
  print(object) 
}         
)
