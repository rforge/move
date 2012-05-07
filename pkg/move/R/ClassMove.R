# require(sp, quietly=TRUE)  ## Including necessary classes for move package
# require(raster, quietly=TRUE)
# require(RgoogleMaps, quietly=TRUE)
# require(rgdal, quietly=TRUE)
# require(geosphere, quietly=TRUE)
# require(methods, quietly=TRUE)



###Defining the class of the Move
setClass(Class = "Move",
         representation = representation (
           sdf = "SpatialPointsDataFrame",     
            animal = "character",   #animal name
            species = "character",
            dateCreation = "POSIXct", #time stamp data creation ##use for the date POSIXct
            study = "character",
            citation = "character",
            license = "character",
            timesMissedFixes="POSIXct")
)


## Making move a generic funtion
#if (!isGeneric("move")) {
	setGeneric("move", function(x, y, time, data, proj)
		standardGeneric("move"))
#}

## Defining the funcitoin move

##Reading from a .csv file
setMethod(f="move", 
          signature=c(x="character"), 
          definition = function(x, proj){
            #check wheter rgdal is installed
            if (any(.packages(all=T)=="rgdal")==FALSE){stop("You need the 'rgdl' package to be installed. \n You may use: \n setRepositories(ind=1:2) \n install.packages('rgdal') \n")} else {}
            
            #check whether data are really from movebank
            
            #stop("The entered file does not seem to be from Movebank.")
            
		        df <- read.csv(x, header=TRUE, sep=",", dec=".")
		        df$timestamp <- as.POSIXct(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%S", tz="UTC") ## NOTE: GMT is is default
            df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format = "%Y-%m-%d %H:%M:%OSn"))            
            
            res <- new("Move")
            #save omitted NA timestamps
		        res@timesMissedFixes <- df[(is.na(df$location.long)|is.na(df$location.lat)), "timestamp"]
            #omitting NAs
		        df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ]
            
		        #if (proj@projargs=="+proj=longlat"){
		          tmp <- SpatialPointsDataFrame(
		            coords = cbind(df$location.long,df$location.lat),
		            data = data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long")]]), ## incude all data from the data.frame except the coordinats (which are already stored in coords)
		            proj4string = CRS("+proj=longlat +ellps=WGS84"), 
		            match.ID = TRUE)
		        #} else {stop("No valid CRS object entered")}
		        
		        res@sdf <- tmp
            
            if (length(levels(df$individual.local.identifier))==1) {
              res@animal[1] <- as.character(df$individual.local.identifier[1])} else{stop("More than one animal detected")}
            if (length(levels(df$individual.taxon.canonical.name))==1) {
              res@species[1] <- as.character(df$individual.taxon.canonical.name[1])} else{stop("More than one species defined")}
		        if (length(levels(df$study.name))==1) {
              res@study[1] <- as.character(df$study.name[1])} else{stop("More than one study detected")}            
		        
		        ###validity check for sorted time stamps
		        if (any(df$timestamp!=sort(df$timestamp))){stop("\n Error:The data set includes unsorted time stamps")} else {}
		        ###validity check for double time stamps
		        if (any(duplicated(df$timestamp))){stop("\n Error: the data set includes double time stamps")} else {}
            ###validity check for double locations
		        if (any(duplicated(as.numeric(coordinates(res@sdf))))){cat("\n WARNING: The data file includes double locations \n")} else {}
            
            return(res)
          }
)

#if non-movebank data are used, coordinates (x,y), time and the data frame must be defined
setMethod(f="move",
          signature=c(x="numeric"),#,y="numeric",time="factor",data="data.frame"),
          definition = function(x,y,time,data,proj){
            #check wheter rgdal is installed
            if (any(.packages(all=T)=="rgdal")==FALSE){stop("You need the 'rgdl' package to be installed. \n You may use: \n setRepositories(ind=1:2) \n install.packages('rgdal') \n")} else {}
            
            df <- data
            df$timestamp <- time
            #check for valid POSIXct timestamp
            if (grepl("POSIXct", class(df$timestamp))[1]==FALSE) {stop("\n The timestamps need to be transformed to a POSIXct class.")} else {}
            df$location.long <- x
            df$location.lat <- y
            
            res <- new("Move")
            res@timesMissedFixes <- df[(is.na(df$location.long)|is.na(df$location.lat)), "timestamp"] #save omitted NA timestamps
            df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ] #omitting NAsa
           
            tmp <- SpatialPointsDataFrame(
              coords = cbind(df$location.long, df$location.lat),
              data = df,#(df[names(df)[!names(df)%in%c("location.lat", "location.long")]]),
              proj4string = proj, 
              match.ID = TRUE)
            
            res@sdf <- tmp
            
#             if (length(levels(df$individual.local.identifier))==1) {
#               res@animal[1] <- as.character(df$individual.local.identifier[1])} else{stop("More than one animal detected")}
  
            ###validity check for sorted time stamps
            if (any(df$timestamp!=sort(df$timestamp))){stop("\n Error:The data set includes unsorted time stamps")} else {}
            ###validity check for double time stamps
            if (any(duplicated(df$timestamp))){stop("\n Error: the data set includes double time stamps")} else {}
            ###validity check for double locations
            if (any(duplicated(as.numeric(coordinates(res@sdf))))){cat("\n WARNING: The data file includes double locations \n")} else {}
            
            return(res)
          }
          )


###Checking if projection method was set for move function
###if not, object is not created and process stops, returning error message
# setMethod(f="move",
#           signature=c(x="character", proj="missing"),
#           definition = function(x){return(stop("Projection method missing"))}
#           )
# 
# setMethod(f="move",
#           signature=c(x="character", proj="character"),
#           definition = function(x,proj){return(move(x=x,proj=CRS(proj)))}
#           )


###extract coordinates from Move
setMethod("coordinates", "Move", function(obj, ...){
            return(coordinates(obj@sdf, ...))
          }
          )


###extract number of locations from Move
#if (!isGeneric("n.locs")) {
setGeneric("n.locs", function(obj) standardGeneric("n.locs"))
#}

setMethod("n.locs", "Move", function(obj){
            return(length(coordinates(obj)[ ,1]))
          }
          )


###extract time.lag from Move
#if (!isGeneric("time.lag")) {
setGeneric("time.lag", function(x, ...) standardGeneric("time.lag"))
#}

setMethod("time.lag", "Move", function(x, ...){
            return(c(as.numeric(diff(x@sdf@data$timestamp)),0)) #calculates the time differences between locations one less than locations! we need a more elegant way than just adding a zero 
          }
          )


###extract projection from Move
setMethod("proj4string", "Move", function(obj){
            return(proj4string(obj@sdf))
          }
          )

###extract the sdf data.frame from Move
setMethod("as.data.frame", "Move", function(x,...){
          return(as.data.frame(x@sdf,...))
          }
          )

if (!isGeneric("SpatialLines")) {
setGeneric("SpatialLines", function(LinesList) standardGeneric("SpatialLines"))
}

#transform Move's spatialpoints to spatiallines
 setMethod("SpatialLines", "Move", function(LinesList){
           xy <- (coordinates(LinesList))
           xyLine <- Line(xy)
           if (length(LinesList@animal)!=0){id <- LinesList@animal} else {id <- "noID"}
           #xyLines <- Lines(list(xyLine), ID=LinesList@animal)
           xyLines <- Lines(list(xyLine), ID="a")
           #return(SpatialLines(list(xyLines), proj4string=CRS("+proj=aeqd +ellps=WGS84")))
           return(SpatialLines(list(xyLines), proj4string=CRS(proj4string(LinesList))))
           }
           )


###plotting 
setGeneric("points")
setMethod("points", "Move", function(x,add=FALSE,...){
          if (add==FALSE) {plot(coordinates(x), type="p", ...)}
          else {points(coordinates(x), type="p", ...)}
          }          
          )

setGeneric("lines")
setMethod("lines", "Move", function(x,add=FALSE,...){
          if (add==FALSE) {plot(coordinates(x), type="l", ...)}
          else {lines(coordinates(x), type="l", ...)}
          }          
          )

setGeneric("plot")
setMethod("plot", "Move", function(x, google=FALSE,...){
            if (google==FALSE){
              plot(coordinates(x), type="p", ...)#creates points
              lines(x, add=TRUE, ...)
            } else {
              require(RgoogleMaps)
              obj <- x
              lat <-coordinates(obj)[ ,2] 
              lon <- coordinates(obj)[ ,1]
              center <- c(mean(lat), mean(lon))
              zoom <- min(MaxZoom(range(lat), range(lon)))
              
              MyMap <- GetMap(center=center, zoom=zoom, destfile=paste(getwd(),"MyTile.png",sep="/"), ...)
              
              PlotOnStaticMap(destfile=paste(getwd(),"MyTile.png",sep="/"), lon=lon, lat=lat, FUN=lines)
              PlotOnStaticMap(MyMap=MyMap, add=TRUE, FUN=lines, lwd=2, lty=5)
              file.remove(paste(getwd(),"MyTile.png",sep="/"))
              file.remove(paste(getwd(),"MyTile.png.rda",sep="/"))
            }
          }
          )


setMethod(f = "spTransform", 
          signature = c(x = "Move", CRSobj = "missing"), 
          function(x, center=FALSE, ...){
            if (center==TRUE){
              mid.range.lon <- (max(coordinates(x)[ ,1])+min(coordinates(x)[ ,1]))/2
              mid.range.lat  <- (max(coordinates(x)[ ,2])+min(coordinates(x)[ ,2]))/2
              crsexpr <- paste("+proj=aeqd +lon_0=",mid.range.lon," +lat_0=", mid.range.lat, sep="")
            } else {
              crsexpr <- "+proj=aeqd"
            }
            coordsnew <- spTransform.SpatialPointsDataFrame(x= x@sdf, CRSobj=CRS(crsexpr))
            x@sdf <- coordsnew
            return(x)
          }
          )


setMethod(f = "spTransform", 
          signature = c(x = "Move", CRSobj = "character"), 
          function(x, CRSobj, center=FALSE){
            if (center==TRUE){
              mid.range.lon <- (max(coordinates(x)[ ,1])+min(coordinates(x)[ ,1]))/2
              mid.range.lat  <- (max(coordinates(x)[ ,2])+min(coordinates(x)[ ,2]))/2
              crsexpr <- paste(CRSobj," +lon_0=",mid.range.lon," +lat_0=", mid.range.lat, sep="")
            } else {
              crsexpr <- CRSobj
            }
            
            coordsnew <- spTransform.SpatialPointsDataFrame(x= x@sdf, CRSobj=CRS(crsexpr))
            x@sdf <- coordsnew
            return(x)
          }
          )




### Show Method for the data object Move
setMethod("show", "Move", function(object){
            cat("******* Class Move, show method ******* \n")
            Animal   <- object@animal
            Species  <- object@species
            Receiver <- object@sdf@data$sensor.type[1]
            Proj     <- proj4string(object)
            nPoints  <- length(coordinates(object)[ ,1])
            #DateCreation <- c(object@dateCreation)
            Study   <- c(object@study)
            df <- try(data.frame(Animal, Species, nPoints, Receiver, Study, check.rows=FALSE), silent=TRUE)
            print(df)            
            cat("***** Projection method             ***** \n")
            print(Proj)            
            cat("***** Coordinates                   ***** \n")
            print(coordinates(object)[1:3, ])            
            cat("***** Spatial Data Frame data       ***** \n")
            print(object@sdf@data[1:3, ])            
            cat("***** End Spatial Data Frame data   ***** \n")
            cat("***** There were: ", length(object@timesMissedFixes), "fixes omitted due to NA location \n")
            cat("***** How to cite the dataset \n")
            cat(object@citation, "\n")
            cat("***** Usage of the data underlies the following license \n")
            cat(object@license, "\n")
            cat("******* End show (Move) ******* \n") 
            }
          )

### Summary of a Move object
# setMethod("summary", "Move", function(object){
#           return(list(object@sdf[1:5, ], paste("Omitted locations: ", length(object@timesMissedFixes))))
#           }
#           )