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
      			if(length(object@study)>1)
      				stop("Study has length unequal to 0 or 1")
      			if(length(object@citation)>1)
      				stop("Citation has length unequal to 0 or 1")
      			if(length(object@license)>1)
      				stop("License has length unequal to 0 or 1")
      			return(TRUE)
    		 }
    	   )

setClass(Class = ".MoveTrack",contains=c("SpatialPointsDataFrame"),
	       representation = representation(
				   timestamps = "POSIXct"),
	       prototype = prototype(
           timestamps = as.POSIXct(NA)),
      	 validity = function(object){
      			if(length(object@timestamps)!=nrow(object@coords))
      				stop("Number of timestamps does not match the number of coordinates")
      			return(TRUE)
	 	    }
	      )

setClass(Class = ".MoveTrackSingle",contains=c(".MoveTrack"), ##why are no missed fixes stored for a MoveStack?
	       representation = representation (
					  timesMissedFixes = ".OptionalPOSIXct"),
	       prototype = prototype(
            timesMissedFixes = NULL),
	       validity = function(object){
		  	    if(any(object@timestamps!=sort(object@timestamps)))
				      stop("The dataset includes unsorted time stamps")
		  	    if (any(duplicated(object@timestamps)))
				      stop("The dataset includes double timestamps")
			      return(TRUE)
	 	    }
	      )

setClass(Class = "Move", contains=c(".MoveTrackSingle",".MoveGeneral"),
       	 representation = representation (
      		 animal = "character",
      		 species = "character"),# marco maybe make something like id data instead of this rather specific implementation
      	 prototype = prototype(
           animal = as.character(),
      		 species = as.character()),
      	 validity = function(object){
      			if(length(object@species)>1)
      				stop("Species has length unequal to 0 or 1")
      			if(length(object@animal)>1)
      				stop("Animal has length unequal to 0 or 1")
      			return(TRUE)
      	 }
      	 )

setClass(Class= "MoveBurst", contains=c("Move"),
		representation=representation(
				bursts="factor"),
		prototype=prototype(
				bursts=factor()),
		validity=function(object){
			if(length(object@bursts)!=length(object@timestamps))
				stop("length of bursts does not match")
			return(TRUE)})


## Making move a generic funtion
#if (!isGeneric("move")) {
	setGeneric("move", function(x, y, time, data, proj, ...) standardGeneric("move"))
#}

### Defining the funcitoin move
##Reading from a .csv file
setMethod(f = "move", 
      	  signature = c(x="character"), # marco maybe also make these this fucntion work with files with multiple ids by combining moveStack and move functions all into move functions
      	  definition = function(x, proj){
      		#check wheter rgdal is installed
      		#if (!any(.packages(all=T)=="rgdal")){stop("You need the 'rgdal' package to be installed. \n You may use: \n setRepositories(ind=1:2) \n install.packages('rgdal') \n")} else {}
		  if(!file.exists(x))
			  stop("x should be a file on disk but it cant be found")
      		df <- read.csv(x, header=TRUE, sep=",", dec=".")
      		#check whether data are really from movebank
      		if (!all(c("timestamp", "location.long",  "location.lat", "study.timezone", "study.local.timestamp", "sensor.type", "individual.local.identifier", "individual.taxon.canonical.name")%in%colnames(df)))
      		        stop("The entered file does not seem to be from Movebank. Please use the alternative import function.")
      		df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC") 
      		df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format="%Y-%m-%d %H:%M:%OS"))            
      		missedFixes<- df[(is.na(df$location.long)|is.na(df$location.lat)), ]$timestamp
      		df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ]
          
      		tmp <- SpatialPointsDataFrame(
      		      coords = cbind(df$location.long,df$location.lat),
      		      data = data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long","timestamp")]]), 
      		      proj4string = CRS("+proj=longlat +ellps=WGS84"), # proj (function argument ) is not used here Marco
      		      match.ID = TRUE)
          
      		res <- new("Move", 
      		      timestamps = df$timestamp, 
      		      tmp, 
      		      study = levels(df$study.name), 
      		      species = levels(df$individual.taxon.canonical.name), 
      		      animal = levels(df$individual.local.identifier),
      		      timesMissedFixes = missedFixes)
      		return(res)
      	  }
      	  )

#if non-movebank data are used, coordinates (x,y), time and the data frame must be defined
setMethod(f="move",
          signature=c(x="numeric"),#,y="numeric",time="factor",data="data.frame"),
          definition = function(x,y,time,data,proj){
            #check wheter rgdal is installed
            #if (any(.packages(all=T)=="rgdal")==FALSE){stop("You need the 'rgdl' package to be installed. \n You may use: \n setRepositories(ind=1:2) \n install.packages('rgdal') \n")} else {}
            
            df <- data
            df$location.long <- x
            df$location.lat <- y
            if(any(is.na(df$location.long))==TRUE) warning("There were NA locations detected.")
            #res@timesMissedFixes <- df[(is.na(df$location.long)|is.na(df$location.lat)), "timestamp"] #save omitted NA timestamps
            df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ] #omitting NAsa
           
            tmp <- SpatialPointsDataFrame(
              coords = cbind(df$location.long, df$location.lat),
              data = df,#(df[names(df)[!names(df)%in%c("location.lat", "location.long")]]),
              proj4string = proj, 
              match.ID = TRUE)
            
            res <- new("Move",
                       tmp,
                       timestamps=time)
            return(res)
          }
          )


###This function is called by getMovebankData with
###move(x=trackDF, y=studyDF, animal=animalName)
setMethod(f="move",
          signature=c(x="data.frame", y="data.frame"),
          definition = function(x,y, animal,...){
            #check wheter rgdal is installed
            #if (any(.packages(all=T)=="rgdal")==FALSE){stop("You need the 'rgdl' package to be installed. \n You may use: \n setRepositories(ind=1:2) \n install.packages('rgdal') \n")} else {}

            df <- x
            df$timestamp <-  as.POSIXct(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%S", tz="UTC")
            df <- df[!(is.na(df$location_long)|is.na(df$location_lat)), ] #omitting NAsa
            #check for valid POSIXct timestamp
            if (grepl("POSIXct", class(df$timestamp))[1]==FALSE) {stop("\n The timestamps need to be transformed to a POSIXct class.")} else {}
#            df$location.long <- x
#            df$location.lat <- y
            
            tmp <- SpatialPointsDataFrame(
              coords = cbind(df$location_long, df$location_lat),
              data = df,#(df[names(df)[!names(df)%in%c("location.lat", "location.long")]]),
              proj4string = CRS("+proj=longlat +ellps=WGS84"), 
              match.ID = TRUE)
            
            res <- new("Move",
                       tmp)
            
            res@timesMissedFixes <- df[(is.na(df$location_long)|is.na(df$location_lat)), "timestamp"] #save omitted NA timestamps
            res@animal <- animal
            res@species <- ""
            res@study <- as.character(y$name)
            res@citation <- as.character(y$citation)
            res@license <- as.character(y$license_terms)            
            return(res)
          })

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
#setMethod("coordinates", "Move", function(obj, ...){
#            return(coordinates(obj@sdf, ...))
#          }
#          )


###extract number of locations from Move
if (!isGeneric("n.locs")) 
{
	setGeneric("n.locs", function(obj) standardGeneric("n.locs"))
}

setMethod("n.locs", "Move", function(obj){
            return(length(coordinates(obj)[ ,1]))
          }
          )


###extract time.lag from Move
if (!isGeneric("time.lag")) {setGeneric("time.lag", function(x, ...) standardGeneric("time.lag"))}
setMethod("time.lag", "Move", function(x, ...){
            return(as.numeric(diff(x@timestamps),...)) #calculates the time differences between locations one less than locations! we need a more elegant way than just adding a zero 
          }
          )

###Redifining spTransform, because it changes the class of the object to SpatialPointsDataFrame 
setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "missing"), 
          function(x, center=FALSE, ...){
            spTransform(x=x, center=center, CRSobj="+proj=aeqd")
          }
)
setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "character"), 
          function(x, CRSobj, center=FALSE){
		  spTransform(x=x, CRSobj=CRS(CRSobj), center=center)
	  })

setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "CRS"), 
          function(x, CRSobj, center=FALSE){
            if (center){
              mid.range.lon <- (max(coordinates(x)[ ,1])+min(coordinates(x)[ ,1]))/2
              mid.range.lat  <- (max(coordinates(x)[ ,2])+min(coordinates(x)[ ,2]))/2
              CRSobj <- CRS(paste(as.character(CRSobj)," +lon_0=",mid.range.lon," +lat_0=", mid.range.lat, sep=""))
            } 
            
            coordsnew <- spTransform.SpatialPointsDataFrame(x=x, CRSobj=CRSobj)
            x <- new(class(x), coordsnew,x )
            return(x)
          }
)


# if (!isGeneric("SpatialLines")) {
# setGeneric("SpatialLines", function(LinesList) standardGeneric("SpatialLines"))
# }
# 
# #transform Move's spatialpoints to spatiallines
#  setMethod("SpatialLines", "Move", function(LinesList){
#            xy <- (coordinates(LinesList))
#            xyLine <- Line(xy)
#            if (length(LinesList@animal)!=0){id <- LinesList@animal} else {id <- "noID"}
#            #xyLines <- Lines(list(xyLine), ID=LinesList@animal)
#            xyLines <- Lines(list(xyLine), ID="a")
#            #return(SpatialLines(list(xyLines), proj4string=CRS("+proj=aeqd +ellps=WGS84")))
#            return(SpatialLines(list(xyLines), proj4string=CRS(proj4string(LinesList))))
#            }
#            )


###plotting 
#setGeneric("points")
#setMethod("points", "Move", function(x,add=FALSE,...){
#          if (add==FALSE) {plot(coordinates(x), type="p", ...)}
#          else {points(coordinates(x), type="p", ...)}
#          }          
#          )
#


###MARCO change all plot functions to plot, default is points and lines, user can also plot p or l OR add p and l for Move AND MoveStacks
setGeneric("lines") ##need to be different -- 
setMethod("lines", "Move", function(x,add=FALSE,...){
          if (add==FALSE) {plot(coordinates(x), type="l", ...)}
          else {lines(coordinates(x), type="l", ...)}
          }          
          )

setGeneric("plot") ###is not working properly!! returns that google is not a graphic parameter
setMethod(f = "plot", 
          signature = c(x="Move", y="missing"), 
          function(x, google=FALSE, maptype="terrain",...){
            if (google==FALSE){
              plot(coordinates(x), type="p", ...)#creates points
              lines(x, add=TRUE, ...)
            } else {
              if (grepl("longlat",proj4string(x)) == FALSE) {stop("\n The projeciton of the coordinates needs to be \"longlat\" to be plotted on a google map. \n")} else {}
              googleplot(obj=x,...)
            }
          }
          )


setGeneric("googleplot", function(obj, ...){standardGeneric("googleplot")})
setMethod(f = "googleplot", 
          signature = c(obj="Move"), 
          function(obj, maptype="terrain",...){
              require(RgoogleMaps)
              lat <-coordinates(obj)[ ,2] 
              lon <- coordinates(obj)[ ,1]
              MyMap <- GetMap.bbox(lonR=range(coordinates(obj)[ ,1]), latR=range(coordinates(obj)[ ,2]), maptype=maptype)
              PlotOnStaticMap(MyMap=MyMap, lon=lon, lat=lat, FUN=lines, ...)
              file.remove(paste(getwd(),"MyTile.png",sep="/"))
              file.remove(paste(getwd(),"MyTile.png.rda",sep="/"))
              })
# 
# setMethod(f = "plot",
#           signature = c(x="MoveStack"),
#           definition = function(x, google=FALSE, maptype="terrain", ...){
#             moveList <- split(test2)
#             plot(moveList[[1]], ...)
#             for (i in 2:length(moveList)){
#               points(moveList[[i]], add=TRUE, ...)
#               lines(moveList[[i]], add=TRUE, ...)
#             }
#           }
#           )
#plot(test2)

###Print function for a Move and MoveStack object
# setGeneric("print")
# setMethod("print",".MoveTrackStack",function(x){
#           callNextMethod(x)
#           if (exists("study.name",x@idData)==TRUE){
#             cat("study name  :",levels(x@idData$study.name),"\n")}
#           if (exists("individual.taxon.canonical.name", where=x@idData)==TRUE){
#             cat("species     :",as.character(unique(x@idData$individual.taxon.canonical.name)),"\n")}
#           cat("no. of indiv:",nlevels(x@trackId),"\n")
#           cat("indiv. ids  :",paste(levels(x@trackId),collapse=", "),"\n")
#           pp <- split(x@coords,x@trackId)
#           cat("no. of fixes:",unlist(lapply(pp,length)),"\n")
#           }
#           )

# setMethod("print","MoveStack",
#           function(x){
#             callNextMethod(x)
#             if (exists("sensor.type", where=x@idData)==TRUE){
#               cat("sensor type :",levels(x@idData$sensor.type),"\n")}
#             maxItems <- 10  
#             items <- ncol(x@idData)
#             if (items > maxItems) { 
#               coln <- colnames(x@idData)
#               coln <- c(coln[1:maxItems], '...')
#             } else {coln <- colnames(x@idData)}
#               cat("indiv. attr.:", paste(coln, collapse=", "), "\n")
#           }
#           )

setMethod("print",".MoveTrackSingle",
          function(x){
            callNextMethod(x)
            try(silent=TRUE, if(length(x@timesMissedFixes)>1)
              cat("missed fixes:", length(x@timesMissedFixes)) )
          }
          )

setMethod("print", ".MoveTrack", function(x){
  #print(as(x[x@trackId==ID,],"SpatialPointsDataFrame"))
  callNextMethod(x)
  timeRange <- range(x@timestamps)
  cat("timestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))), " (start...end, duration) \n")  
}       
)




### Show Method for the data object Move
setMethod("show", "Move", function(object){
            print(object)
            }
          )
# setMethod("show", "MoveStack", function(object){
#   print(object) 
# }         
# )


### Summary of a Move object
setGeneric("summary")
setMethod("summary", "Move", function(object){
    
    require(adehabitat,quietly=T)
    require(circular,quietly=T)
    require(gpclib,quietly=T)
  
    track <- as.data.frame(coordinates(object))
    date <- object@timestamps
    animalID <- as.data.frame(object@data$individual.local.identifier[1])
    names(animalID)  <- "animalID"
    tagID  <- as.data.frame(object@data$tag.local.identifier[1])
    names(tagID) <- "tagID" 
    species <- object@species # Marco why not use object@species here?
    trackProj <- proj4string(object)    
    #check whether trac is in long/lat
    if (grepl("longlat",proj4string(object)) == FALSE) {stop("\n The projeciton of the coordinates needs to be \"longlat\". \n")}else{}
    
    trackTraj <- as.ltraj(as.data.frame(coordinates(object)), date=date, id=animalID)
    
      
      tempSpecies <- object@species  #species name # Marco why not use object@species here?
      tempReloc <- nrow(track)    #number of relocations
      tempStart <- as.character(min(date)) #start date and time of tracking
      tempEnd <- as.character(max(date))  # end date and time of tracking
      tempRelpd <- NA  #relocations per day
      tempSEDist <- NA  #start to end straight distance
      tempTravDist <- NA    #travel distance
      tempMaxDist <- NA
      tempMinDist <- NA
      temp.dt <- NA
      oult <- NA
      dupl <- NA
      multseason <- NA
      tempMaxDist <- NA  # farthest distance from the start
      tempAverDist <- NA   #mean distance between relocations
      tempSDDist <- NA   #standard deviation of distances between relocations
      temp.Dur <- NA  # total duration of the track in hours
      tempAverDur <- NA    #mean time difference between relocations
      tempSDDur <- NA      #standard deviation of time differences between relocations
      tempAverSpeed <- NA #mean speed in meters per second derived from segment length / time diff
      tempMaxSpeed <- NA #max speed in meters per second
      tempVarSpeed <- NA #variance in speed in meters per second
      tempAngles <- NA #contains absolute angle in radians with zero=pi/2 (angle between each move and north)
      tempAverAngles <- NA #mean absolute direction
      tempRhoAngles <- NA #mean resultant length of absolute direction
      tempVarAngles <- NA #one minus the mean resultant length divided by the sample size of a vector of circular data
      temp.SEAngleAngle <- NA #start to end angle
      tempFPTcoeff <- NA #the coefficient of log(first_passage_time)=a*log(radius) + b
      tempFPTintercept <- NA #the intercept of the above fpt equation.
      tempHBrown <- NA #the Brownian motion variance estimated from the trajectory
      tempMCPArea <- NA #area of the MCP
    
    if((max(floor(as.numeric(julian(date))))-min(floor(as.numeric(julian(date)))))>0)
      {
        tempRelpd <- nrow(track) / (max(floor(as.numeric(julian(date)))) - min(floor(as.numeric(julian(date)))))  #relocations per day
      }else{
        tempRelpd <- NA
      }
    
      tempSEDist <- distHaversine(p1 = track[1, ], p2=track[nrow(track), ])  #start to end straight distance in meters
      tempDists <- distHaversine(p1 = track[-nrow(track), ], p2=track[-1, ]) #vector of all distances in meters
      tempTravDist <- sum(distHaversine(p1 = track[-nrow(track), ], p2=track[-1, ]))    #travel distance in meters
      tempMaxDist <- max(tempDists) #largest distance
      tempMinDist <- min(tempDists) #shortest distance
      tempFarthDist <- max(spDistsN1(pts=coordinates(object),pt=coordinates(object)[1,],longlat=TRUE))*1000 #farthest distance from the start in meters
      tempAverDist <- mean(tempDists)    #mean distance between relocations in meters
      tempSDDist <- sd(tempDists)      #standard deviation of distances between relocations
      
      tempTimeDiff <- difftime(time2=date[-length(date)], time1=date[-1]) #time differences in minutes
      out <- boxplot(tempDists/(as.numeric(tempTimeDiff+0.0000001)), range=10, plot=F)$out  #unit: meters per minute
      outliners <- length(out[out > median(out)])>0   #check whether there are strong outliers in speed
      dupl <- any((tempTimeDiff)<(1/3600))            #check whether any two relocations are closer than a second to each other
      multseason <-  any(tempTimeDiff > (24*30))      #check whether any two subsequent relocations are more than one month apart
      
      tempDur <- difftime(date[length(date)], date[1], units="hours") #total duration of the track in hours
      tempAverDur <- mean(tempTimeDiff)       #mean time difference between relocations
      tempSDDur <- sd(tempTimeDiff)           #standard deviation of time differences between relocations
      
      tempSpeed <- as.numeric(tempDists)/as.numeric(tempTimeDiff)
      tempAverSpeed <- mean(tempSpeed, na.rm=TRUE)
      tempMaxSpeed <- max(tempSpeed)          #in meters per minute
      tempVarSpeed <- var(tempSpeed, na.rm=T)
      
      options(warn=-1)
      tempAngles <- as.circular(bearing(p1 = track[-nrow(track), ], p2=track[-1, ]), units="degree", zero=pi/2, rotation="clock")
      tempAverAngles <- summary(tempAngles)[2]  #mean direction
      tempRhoAngles <- summary(tempAngles)[3]  #mean resultant length
      tempVarAngles <- var(tempAngles)  #one minus the mean resultant length divided by the sample size of a vector of circular data  ????
      tempSEAngle <- bearing(round(track[1,],5), round(track[nrow(track),],5))  #start to end straight angle
      options(warn=0)
      
#       FPTSeq <- seq(max(tempDists*1000)/10000,max(tempDists*1000), length=10000)   ### ???? what happens here ¿¿¿¿
#       trackFPT <- fpt(trackTraj, FPTSeq, units="seconds")
#       FPT <- as.data.frame(trackFPT[[1]][,names(trackFPT[[1]])[colSums(!is.na(trackFPT[[1]]))>=12]])
#       if(ncol(FPT)>0)
#       {
#         colSumFPT <- colSums(FPT, na.rm=T) / colSums(!is.na(FPT))
#         radii <- attr(trackFPT, "radii")[1:length(colSumFPT)]
#         lmFPT <- lm(log(colSumFPT)~log(radii))
#         tempFPTcoeff <- lmFPT[1]$coefficients[2]
#         tempFPTintercept <- lmFPT[1]$coefficients[1]
#       }else{
        tempFPTcoeff <- NA
        tempFPTintercept <- NA
#      }
    
      tempHBrown <- hbrown(trackTraj)
    
      if(nrow(track)>4)
      {#area of the MCP
        tempMCPArea <- mcp.area(track, percent=100, id=rep(as.numeric(tagID),length.out=length(track[,1])), unin="m", unout="m2",plotit=FALSE)  ###???? what area ¿¿¿¿
      }else{
        tempMCPArea <- NA
      }
      names(tempMCPArea)  <- "MCPArea"

         
  
          
          
    tempRes <- as.data.frame(cbind(animalID, tagID, tempSpecies, tempRelpd, tempSEDist, tempTravDist, tempMaxDist, tempMinDist, tempFarthDist, tempAverDist, tempSDDist, outliners, dupl, multseason, tempDur, tempAverDur, tempSDDur, tempAverSpeed, tempMaxSpeed, tempVarSpeed, tempAverAngles, tempRhoAngles, tempVarAngles, tempSEAngle, tempFPTcoeff, tempFPTintercept, tempHBrown, tempMCPArea))
    results <- tempRes
    
  cat("*** Move Object Summary *** \n")
  print(results)
  cat("\nProjection is    : ",proj4string(object),"\n")
  cat("Omitted locations:  ", length(object@timesMissedFixes))
}
)
# 
#			#Find below the functions to plot a "centroid" point on the line of a certain line segment
			data <- read.csv("~/Downloads/beh_movebank.csv", header=T, sep=";", dec=".")
			data <- data[order(time=as.POSIXct(x=data$time, format="%Y-%m-%d %H:%M:%S",tz="UTC")),]
			test <- move(x=data$xi, y=data$yi,time=as.POSIXct(x=data$time, format="%Y-%m-%d %H:%M:%S",tz="UTC"), data=data, proj=CRS("+proj=longlat"))
			trackb <- new("MoveBurst", bursts=as.factor(data$id_line_beh), test)
			#l <- as.list(split(as.data.frame(coordinates(trackb)),cumsum(c(0,abs(diff(as.numeric(trackb@data$beh_code)))))))
			#midLST <- list()
			#midLST <- lapply(X=ll, FUN=lineMidpoint)
#			
#			
#			
#			
#			require(move)
#			##add classfication for color and size marco
setGeneric("plotBursts", function(object, by, plot=TRUE, col, class,...){standardGeneric("plotBursts")})



setMethod(f = "plotBursts", 
          signature = c(object="MoveBurst", by="integer"),
          definition = function(object, by, plot=TRUE, add=FALSE,...){
#            require("classInt")
            fixes <- nrow(coordinates(object))
            states <- unique(by)
            col <- rgb(runif(states),runif(states),runif(states)) #make color changeable
            l <- as.list(split(data.frame(coordinates(object)),cumsum(c(0,abs(diff(as.numeric(by)))))))
            coll <- as.list(split(data.frame(by),cumsum(c(0,abs(diff(as.numeric(by)))))))
            ll <- lapply(l, FUN=SpatialPoints, proj4string=CRS("+proj=longlat"))
            midLST <- lapply(X=ll, FUN=lineMidpoint)
            colLST <- lapply(lapply(coll, unique), function(x) return(col[as.numeric(x)]))
            sizeLST <- lapply(lapply(ll, length), FUN= function(x) (x/nrow(coordinates(object))))#
            df <- cbind(as.data.frame(do.call(rbind, midLST)), 
                        as.data.frame(do.call(rbind, colLST)), 
                        as.data.frame(do.call(rbind, sizeLST)))
            colnames(df) <- c("x","y","color", "size")
            spdf  <- SpatialPointsDataFrame(coords=df[,1:2],data=df[,3:4], proj4string=CRS("+proj=longlat"))
            
            if (plot){
              if(!add)
                plot(coordinates(object), type="l")
              apply(df, 1, function(x,...){points(x['x'], x['y'], cex=as.numeric(x['size']), col=x['color'],...)}, ...)
                  #points(coordinates(spdf), col=spdf@data$color, cex=spdf@data$size, pch=19)
            #lapply(midLST, FUN=points,  pch=19)#cex=sizeLST[[1]], col=colLST[[1]],
            } else {}
            if(plot)
              return(invisible(spdf))
            else return(spdf)#return(spdf)
          })

#cut
#sort(unique(cut(size, breaks=5)))


setGeneric("lineMidpoint", function(object){standardGeneric("lineMidpoint")})
setMethod(f = "lineMidpoint",
          signature="SpatialPoints",
          definition=function(object){
            track <- coordinates(object)
            
            if (nrow(track)<2) { ##make the point at the coordinate
              mid <- t(coordinates(track))
            } else {
              if (nrow(track)==2) { ##make the point at the center of the line
                mid <- (coordinates(track)[2,]-coordinates(track)[1,])*.5+coordinates(track)[1,]
              }
              
              if (nrow(track)>2){
                 dreck <- cbind(as.data.frame(track)[-nrow(track),], as.data.frame(track)[-1,])
                 names(dreck) <- c("X1", "Y1", "X2", "Y2")
                 seglength <- function(dreck)
                 {
                   spDistsN1(as.matrix(t(dreck[1:2])), as.matrix(t(dreck[3:4])), longlat=FALSE)
                 }
                dists <- apply(dreck, 1, seglength)
                
                totalDist <- sum(dists)
                cumsum <- cumsum(dists)
                lcum <- list(cumsum)
                
                min <- which(cumsum(dists)>0.5*sum(dists))[1]
                prop <- (cumsum(dists)[min]-sum(dists)/2)/cumsum(dists)[min]
                mid <- (coordinates(track)[min+1,]-coordinates(track)[min,])*prop+coordinates(track)[min,]
              }
            }
            midSP <- SpatialPoints(coords=t(as.data.frame(mid)), proj4string=CRS("+proj=longlat"))
            })
#head(plotBursts(trackb, by=trackb@data$beh_code, plot=F))

