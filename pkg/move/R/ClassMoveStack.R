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


setMethod("[", signature(x="MoveStack"),definition=function(x,i){
	  new("MoveStack", as(x, "SpatialPointsDataFrame")[i,], 
	      trackId=droplevels(x@trackId[i]),
	      idData=x@idData[as.character(unique(x@trackId[i])),],
	      timestamps=x@timestamps[i])})
	      

setMethod(f = "plot", ##bart marco find a more decent way to plot MoveStacks
          signature = c(x="MoveStack", y="missing"), 
          function(x, google=FALSE, maptype="terrain",...){
            unstackedMove <- split(x)
            indiv <- length(unique(x@trackId))
            lines(unstackedMove[[1]], xlim=range(coordinates(x)[,1]), ylim=range(coordinates(x)[,2]) )
            #x@trackId <- factor(x@trackId, labels=c(1:length(unique(x@trackId))))
            l <- split(as.data.frame(coordinates(x)),cumsum(c(0,abs(diff(as.numeric(x@trackId))))))
            lapply(unstackedMove, FUN=lines, col=c(rgb(runif(indiv),runif(indiv),runif(indiv))), add=T, xlim=range(coordinates(x)[,1]), ylim=range(coordinates(x)[,2] ))            
            ##there is no google implemented for MoveStacks marco
          }
          )


setGeneric("moveStack", function(x, proj) standardGeneric("moveStack"))
setMethod(f = "moveStack", 
          signature = c(x="list"),
          definition = function(x){
            if (any((as.character(lapply(x, class)))!="Move")) 
              stop("One or more objects in the list are not from class Move")
            if (any(as.character(lapply(x, function(y) attr(slot(y, "timestamps"), "tzone")) )!="UTC"))
              stop("One or more objects in the list have no UTC timestamps")
            if (length(unique(as.character(lapply(x,proj4string))))!=1)
              stop("One or more objects in the have differnt projections. All projections have to be the same")
            animal <- unlist(lapply(x, slot, name="animal"))
            length <- lapply(lapply(x, coordinates), nrow)
            coords <- do.call(rbind, lapply(x, coordinates))
            colnames(coords) <- c("location.long", "location.lat")
            allData <- lapply(x, function(y) slot(y, "data"))
            allColumns <- unique(unlist(sapply(allData, names)))
            DATA <- do.call("rbind", lapply(allData, FUN = function(entry) {
              missingColumns <- allColumns[which(!allColumns %in% names(entry))]
              entry[, missingColumns] <- NA
              entry})) #thanks to: Karl Ove Hufthammer
            
            tmp <- SpatialPointsDataFrame(
              coords = coords,
              data = DATA, 
              proj4string = CRS(proj4string(x[[1]])),
              match.ID = TRUE)
            
            idData <- data.frame(row.names=animal, 
                                 individual.taxon.canonical.name=unlist(lapply(x, slot, "species")),
                                 study.name=unlist(lapply(x, slot, "study")))
            res <- new("MoveStack", 
                       tmp, 
                       idData = idData,
                       timestamps = as.POSIXct(do.call(rbind, (lapply(x, function(y) {as.data.frame(y@timestamps)})))[,1], tz="UTC"), #timezone?
                       trackId = as.factor(rep(animal, length)),
                       study = c("A study"))
            return(res)
          })

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


###create a list of Move objects from a Move Stack (hand over additional arguments!)
setGeneric("split") ##check whether this is necessary or screws up the original method marco
setMethod(f = "split",
          signature = c(x="MoveStack", f="missing"),
          definition = function(x, f, ...){
            moveList <- list()
            for (ID in unique(x@trackId)) {
              moveObj <- new(Class="Move", 
                             animal=ID,
                             species=as.character(x@idData[ID, "individual.taxon.canonical.name"]),
                             timestamps=x@timestamps[x@trackId==ID],
                             data=x@data[x@trackId==ID,],
                             coords.nrs=x@coords.nrs,
                             coords=x@coords[x@trackId==ID,],
                             bbox=as.matrix(x@bbox),
                             proj4string=x@proj4string,
                             dateCreation=x@dateCreation,
                             study=as.character(x@idData[ID,"study.name"]),
                             citation=x@citation)
              moveList[[ID]]  <- moveObj
            }
            return(moveList)
          }) ###there is a warning issued like: In min(x) : no non-missing arguments to min; returning Inf;; this is due to the data that are coerced by the show function for 'min values' and 'max values'


setGeneric("citation", function(obj) standardGeneric("citation"))
setMethod("citation", ".MoveGeneral", function(obj){
  return(obj@citation)
})


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
