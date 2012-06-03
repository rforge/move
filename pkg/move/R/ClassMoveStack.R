setClass(Class=".MoveTrackStack", contains=c(".MoveTrack"),
	 representation=representation( 
				       trackId="factor"
				       ),
	 prototype=prototype(trackId=factor()),
	 validity=function(object){
			if(length(object@trackId)!=nrow(object@coords))
				stop("Number of trackId does not match the number of coordinates")
		  	if(any(duplicated(cbind(object@timestamps, object@trackId))))
				stop("The data set includes double timestamps per id")
			if(any(unlist(lapply(tapply(object@timestamps,object@trackId, order),diff))!=1))
				stop("Not ordered timestamps per individual")
			return(TRUE)
	 	}
	 )
setClass(Class="MoveStack", contains=c(".MoveGeneral",".MoveTrackStack"),
	representation=representation(idData="data.frame"),
	prototype=prototype(idData=data.frame()),
	validity=function(object){
			if(length(unique(object@trackId))!=nrow(object@idData))
				stop("Not same number of ids and rows in dataframe per id")
			if(any(sort(levels(object@trackId))!=sort(unique(rownames(object@idData)))))
				stop("No match between rownames in idData and ids along track")
			return(TRUE)
		}
	)
setGeneric("citation", function(obj) standardGeneric("citation"))
setMethod("citation", ".MoveGeneral", function(obj){
            return(obj@citation)
          }
          )
#setGeneric("move", function(x, y, time, data, proj, ...) standardGeneric("move"))
setGeneric("moveStack", function(x, y, time, data, proj, ...) standardGeneric("moveStack"))
setMethod(f="moveStack", 
	  signature=c(x="character"), 
	  definition = function(x, proj){
		df <- read.csv(x, header=TRUE, sep=",", dec=".")
		#check whether data are really from movebank
		if (!all(c("timestamp","location.long", "location.lat","study.timezone","study.local.timestamp","sensor.type","individual.local.identifier","individual.taxon.canonical.name")%in%colnames(df)))
		        stop("The entered file does not seem to be from Movebank. Please use the alternative import function.")
		df$timestamp <-
		        as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC") 
		df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format="%Y-%m-%d %H:%M:%OS"))            
		missedFixes<-df[(is.na(df$location.long)|is.na(df$location.lat)), ]
		df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ]
		# p is a vector of with variables ar unique per individual
		p<-unlist(lapply(lapply(lapply(lapply(apply(df, 2, tapply, df$individual.local.identifier, unique), lapply, length),unlist),'==',1),all))
		tmp <- SpatialPointsDataFrame(
		      	coords = cbind(df$location.long,df$location.lat),
		      	data = data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long","timestamp","individual.local.identifier", names(p)[p])]]), 
		      	proj4string = CRS("+proj=longlat +ellps=WGS84"), # proj is not used here
		      	match.ID = TRUE)
		idData<-df[!duplicated(df$individual.local.identifier),names(p)[p]]
		rownames(idData)<-idData$individual.local.identifier
		res <- new("MoveStack", 
		        tmp, 
		        idData=idData,
		        timestamps=df$timestamp, 
		        trackId=df$individual.local.identifier
		        )
		return(res)
	  }
	  )
#data <- move("../inst/extdata/leroy.csv",proj=CRS("+proj=longlat"))
#sta<-moveStack("~/Downloads/Oilbirds.csv")
#tmp<-new("MoveStack")
## look citation now works for both without defining it twice
#citation(data)
#citation(tmp)
## Look now we dont have to define a spTransform function or coordinates function all sp functions magically work! Well magick it finds the class inheritance
#head(coordinates(data))
#head(coordinates(spTransform(data, CRS("+proj=aeqd"))))
#head(coordinates(sta))
#head(coordinates(spTransform(sta, CRS("+proj=aeqd"))))
