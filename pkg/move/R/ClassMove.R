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
				   sensor="factor"),
	       prototype = prototype(
           timestamps = as.POSIXct(NA),
	   sensor=factor()),
      	 validity = function(object){
      			if(length(object@timestamps)!=nrow(object@coords))
      				stop("Number of timestamps does not match the number of coordinates")
      			if(length(object@sensor)!=nrow(object@coords))
      				stop("Number of sensors observations does not match the number of coordinates")
      			return(TRUE)
	 	    }
	      )

# setClass(Class = ".MoveTrackBurst",
#          representation = representation (
#            burstID = "factor", ##to indicate the bursts of a single track
#            burstSPDF = "SpatialPointsDataFrame"),
#          prototype = prototype(
#            burstID = as.factor(NA)),
#          validity = function(object){
#            if (length(object@burstID)!=nrow(coordinates(object)))
#              stop("The length of burst IDs is not equal to the number of locations")
#            return(TRUE)
#          }
#          )

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
            #if (length(object@burstID)!=nrow(coordinates(object)))
              #stop("The length of burst IDs is not equal to the number of locations")
			      return(TRUE)
	 	    }
	      )

setClass(Class = "Move", contains=c(".MoveTrackSingle",".MoveGeneral"),
       	 representation = representation (
       	   idData = "data.frame"),
      	 prototype = prototype(
           idData = data.frame()),
      	 validity = function(object){
      			if(nrow(object@idData)>1)
              stop("There are more than 1 row stored in the idData")
      			return(TRUE)
      	 }
      	 )


## Making move a generic funtion
setGeneric("move", function(x, y, time, data, proj, ...) standardGeneric("move"))
setMethod(f = "move", 
      	  signature = c(x="character",y='missing',time='missing', data='missing', proj='missing'), # marco maybe also make these this fucntion work with files with multiple ids by combining moveStack and move functions all into move functions
      	  definition = function(x){
		  if(!file.exists(x))
			  stop("x should be a file on disk but it cant be found")
      		df <- read.csv(x, header=TRUE, sep=",", dec=".")
      		#check whether data are really from movebank
      		if (!all(c("timestamp", "location.long",  "location.lat", "study.timezone", "study.local.timestamp", "sensor.type", "individual.local.identifier", "individual.taxon.canonical.name")%in%colnames(df)))
      		        stop("The entered file does not seem to be from Movebank. Please use the alternative import function.")
		if(any(dups<-duplicated(apply(df[,names(df)!="event.id"], 1, paste, collapse="__")))){
			warning("Exact duplicate records removed (n=",sum(dups),") (movebank allows them but the move package cant deal with them)")
		       df<-df[!dups,]
	       }	       
      		df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC") 
	       if(any(tapply(df$sensor.type, df$individual.local.identifier, length)!=1)){
		       df<-df[with(df, order(individual.local.identifier, timestamp)), ]

	       }
	       proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

      		try(df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format="%Y-%m-%d %H:%M:%OS")),silent=T)
      		.move(df=df, proj=proj)
      	  }
      	  )

#setMethod(f="move",
#          signature=c(x='ANY', y='ANY', time='ANY', data='ANY', proj='ANY',sensor='missing', animal='ANY'),
#          definition = function(x, y, time, data, proj,sensor, animal, ...){
#		  move(x=x, y=y, time=time, data=data, proj=proj,sensor='unknown', animal=animal,...)
#	  }
#	  )
#setMethod(f="move",
#          signature=c(x='ANY', y='ANY', time='ANY', data='ANY', proj='ANY',sensor='character', animal='ANY'),
#          definition = function(x, y, time, data, proj,sensor, animal, ...){
#		  move(x=x, y=y, time=time, data=data, proj=proj,sensor=factor(sensor), animal=animal,...)
#	  }
#	  )
#setMethod(f="move",
#          signature=c(x='ANY', y='ANY', time='ANY', data='ANY', proj='ANY',sensor='ANY', animal='character'),
#          definition = function(x, y, time, data, proj,sensor, animal, ...){
#		  move(x=x, y=y, time=time, data=data, proj=proj,sensor=sensor, animal=factor(animal),...)
#	  }
#	  )
#setMethod(f="move",
#          signature=c(x='ANY', y='ANY', time='ANY', data='ANY', proj='ANY',sensor='ANY', animal='missing'),
#          definition = function(x, y, time, data, proj,sensor, animal, ...){
#		  move(x=x, y=y, time=time, data=data, proj=proj,sensor=sensor, animal='unknown',...)
#	  }
#	  )
#if non-Movebank data are used, table is new defined 
setMethod(f="move",
          signature=c(x="numeric", y="numeric", time="POSIXct", data="missing", proj="CRS"),
          definition = function(x,y,time,data,proj, ...){
		  data<-data.frame(x,y,time)
		  move(x=x,y=y,time=time,proj=proj,data=data,...)
          }
          )
setMethod(f="move",
          signature=c(x="numeric", y="numeric", time="POSIXct", data="data.frame", proj="CRS"),
          definition = function(x,y,time,data,proj,sensor='unknown',animal='unnamed', ...){
            data$location.long <- x
            data$location.lat <- y
            data$timestamp <- time
            data$individual.local.identifier <- animal
	    data$sensor.type<-sensor
            .move(df=data, proj=proj)
          }
          )

setGeneric(".move", function(df, proj) standardGeneric(".move"))
setMethod(f = ".move", 
          signature = c(df="data.frame", proj="CRS"), 
          definition = function(df, proj){
#            df <- x[['df']]
#            proj <- x[[2]]
            if(any(is.na(df$location.long))==TRUE) warning("There were NA locations detected and omitted.")
            missedFixes<- df[(is.na(df$location.long)|is.na(df$location.lat)), ]$timestamp
            df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ]
#	    sensor<-df$sensor.type
 #             df <- df[,names(df)!="sensor.type"]

            
	    if(length(unique(df$individual.local.identifier))>1 & any(unique(as.character(df$individual.local.identifier))==""))
	    {# this is not so elegant from me (bart) since this function also gets used by non movebank data
		    warning("omitting locations that have and empty local identifier (n=",sum(tmp<-as.character(df$individual.local.identifier)==""),") most likely the tag was not deployed") 
		    df<-df[!tmp,]
		    df$individual.local.identifier<-factor(df$individual.local.identifier)

	    }
            ids <- as.list(as.character(unique(df$individual.local.identifier)))


#            if (length(ids)>1){
# i changed this becuause it gave a error when some were unique per id and others not              IDDATA <- lapply(ids, FUN= function(id) {df[df$individual.local.identifier==id, apply(df[df$individual.local.identifier==id,], MARGIN=2, FUN=function(y) {length(unique(y))==1}) ][1,]})
#			      browser()
#			      uniquePerID<-apply(df, MARGIN=2, function(x,y){all(tapply(x,y,function(x){length(unique(x))})==1)}, y=df$individual.local.identifier)
#              idData <- df[!duplicated(df$individual.local.identifier), names(uniquePerID[uniquePerID])]
#            } else {            
#              idData <- df[1, unlist(lapply(lapply(apply(df, 2, unique), length), '==', 1))]
#            }
	    #this function should both work for one and multiple individuals
	      uniquePerID<-apply(df, MARGIN=2, function(x,y){all(tapply(x,y,function(x){length(unique(x))})==1)}, y=df$individual.local.identifier)
			      uniquePerID["sensor.type"]<-FALSE
	      idData<-subset(df, select=names(uniquePerID[uniquePerID]), !duplicated(individual.local.identifier))
	      if(length(names(idData))!=1)# dont shorten it because we need something
	     idData<-subset(idData, select=names(idData)!="individual.local.identifier")

              rownames(idData) <- unique(df$individual.local.identifier)
              

              data <- data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long","timestamp", colnames(idData))]])
              if (ncol(data)==0) data <- data.frame(data, empty=NA)
              
            tmp <- SpatialPointsDataFrame(
                    coords = cbind(df$location.long,df$location.lat),
                    data = data, 
                    proj4string = proj,#CRS("+proj=longlat +ellps=WGS84"), # proj (function argument ) is not used here Marco
                    match.ID = TRUE)
            if (length(ids)==1){
              res <- new("Move", 
                         timestamps = df$timestamp, 
			                   sensor = factor(df$sensor),
                         tmp, 
                         idData = idData,
                         timesMissedFixes = missedFixes)
              } else {
               res <- new("MoveStack", 
                 	        tmp, 
                 	        idData = idData,
			                    sensor = factor(df$sensor),
               		        timestamps = df$timestamp, 
               		        trackId = factor(df$individual.local.identifier))}
            return(res)
          })




###extract number of locations from Move
if (!isGeneric("n.locs")) 
{
	setGeneric("n.locs", function(obj) standardGeneric("n.locs"))
}

setMethod("n.locs", "SpatialPointsDataFrame", function(obj){
            return(length(coordinates(obj)[ ,1]))
          }
          )


###extract time.lag from Move
if (!isGeneric("time.lag")) {setGeneric("time.lag", function(x, ...) standardGeneric("time.lag"))}
setMethod("time.lag", ".MoveTrackSingle", function(x, ...){
#            return(difftime(time2=x@timestamps[-length(x@timestamps)], x@timestamps[-1],...)) #calculates the time differences between locations one less than locations! we need a more elegant way than just adding a zero 
	                return(as.numeric(diff(x@timestamps),...)) #calculates the time differences between locations one less than locations! we need a more elegant way than just adding a zero 

          }
          )

###Redifining spTransform, because it changes the class of the object to SpatialPointsDataFrame 
setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "missing"), 
          function(x, center=FALSE, ...){
            spTransform(x=x, center=center, CRSobj="+proj=aeqd")
          })

setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "character"), 
          function(x, CRSobj, center=FALSE, ...){
		  spTransform(x=x, CRSobj=CRS(CRSobj), center=center)
	  })

setMethod(f = "spTransform", 
          signature = c(x = ".MoveTrack", CRSobj = "CRS"), 
          function(x, CRSobj, center=FALSE, ...){
            if (center){ 
              mid.range.lon <- (max(coordinates(x)[ ,1])+min(coordinates(x)[ ,1]))/2
              mid.range.lat  <- (max(coordinates(x)[ ,2])+min(coordinates(x)[ ,2]))/2
              CRSobj <- CRS(paste(CRSobj@projargs," +lon_0=",mid.range.lon," +lat_0=", mid.range.lat, sep=""))
            } 
            
            coordsnew <- spTransform.SpatialPointsDataFrame(x=x, CRSobj=CRSobj)
            x <- new(class(x), coordsnew,x )
            return(x)
          })

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

# Marco i think this function needs to be uncommented since it would be nice to be able to plot points for move objects
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
          function(x, y, ...){
            plot(coordinates(x), type="p", ...)#creates points
            lines(x, add=TRUE, ...)
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

###Print function for a Move and MoveStack object
setGeneric("print")
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
# 
# setMethod("print","MoveStack",
#           function(x){
#             callNextMethod(x)
#             #if (exists("sensor.type", where=x@idData)==TRUE){
#             #  cat("sensor type :",levels(x@idData$sensor.type),"\n")}
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
            #callNextMethod(x)
            cat("Class        :", class(x),"\n")
            try(cat("name         :", rownames(x@idData),"\n"),silent=T)
            cat("nfeatures    :", nrow(coordinates(x)),"\n")
            cat("extent       :", c(extent(x)@xmin, extent(x)@xmax, extent(x)@ymin, extent(x)@ymax),"\n")
            cat("coord.ref    :", proj4string(x),"\n")
            cat("ndatacols    :", ncol(x@data),"\n")
            cat("variables    :", paste(colnames(x@data),collapse=", "), "\n")
            timeRange <- range(x@timestamps)
            cat("timestamps   :",paste(timeRange, collapse="..."),"\n")
            cat("duration     :", capture.output(round(difftime(timeRange[2],timeRange[1]))), "\n")
            try(silent=TRUE, if(length(x@timesMissedFixes)>=1)
            cat("missed fixes :", length(x@timesMissedFixes)) )
          }
          )

# setMethod("print", ".MoveTrack", function(x){
#   print(as(x[x@trackId==ID,],"SpatialPointsDataFrame"))
#   callNextMethod(x)
#   timeRange <- range(x@timestamps)
#   cat("timestamps  :",paste(timeRange, collapse="..."),capture.output(round(difftime(timeRange[2],timeRange[1]))), " (start...end, duration) \n")  
# }       
# )




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
    animalID <- rownames(object@idData) #as.data.frame(object@data$individual.local.identifier[1])
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




#			#Find below the functions to plot a "centroid" point on the line of a certain line segment
setGeneric("burstTrack", function(object, by, breaks=5, sizeFUN="relTime"){standardGeneric("burstTrack")}) 
# setMethod(f = "burstTrack", 
#           signature = c(object="MoveStack", by="integer"),
#           definition = function(object, by, breaks, sizeFUN){
#             moveUnstacked <- split(x=object) #split MoveStack into individual Move objects
#             spdfLST <- as.list(lapply(moveUnstacked, FUN=burstTrack, by=by, breaks=breaks, sizeFUN=sizeFUN))                             
#           })##function was not tested yet


setMethod(f = "burstTrack", 
          signature = c(object="Move", by="numeric"),
          definition = function(object, by, breaks, sizeFUN){
            fixes <- nrow(coordinates(object))
            totalDur <- difftime(object@timestamps[fixes], object@timestamps[1], units="mins") #duration in MIN
            if (length(by)!=fixes)
              stop("The length of burst IDs is not equal to the number of locations")
              starts <- c(1,which(diff(x=as.numeric(by))!=0))
              stops  <- c(which(diff(x=as.numeric(by))!=0),length(as.numeric(by)))
            l  <- apply(data.frame(Starts=starts, Stops=stops), 1, function(x) data.frame(coordinates(object),object@timestamps)[x[1]:x[2],])
            ll <- lapply(l, function(x) { SpatialPointsDataFrame(coords=data.frame(x[1:2]), 
                                                                 proj4string=CRS("+proj=longlat"), 
                                                                 data=data.frame(timestamps=x$object.timestamps))})
            midLST <- lapply(X=ll, FUN=lineMidpoint)
            
            col <- rgb(runif(unique(by)),runif(unique(by)),runif(unique(by))) #make color changeable
            coll <- as.list(split(data.frame(by),cumsum(c(0,abs(diff(as.numeric(by)))))))
            colLST <- lapply(lapply(coll, unique), function(x) return(col[as.numeric(x)]))
            #sizeLST <- lapply(lapply(ll, length), FUN= function(x) (x/nrow(coordinates(object))))
            
            if (class(sizeFUN)!="function") {
              sizeLST <- lapply(lapply(ll, function(y) {diff.POSIXt(c(y@data$timestamps[nrow(y)], time2=y@data$timestamps[1]), units="mins")} ), FUN= function(x) (as.numeric(x)/as.numeric(totalDur))) 
              } else {FUN <- match.fun(sizeFUN)
                      sizeLST  <- FUN(ll)}
            sizes  <- as.numeric(cut(unlist(sizeLST), breaks=breaks))/max(as.numeric(cut(unlist(sizeLST), breaks=breaks))) 
            df <- cbind(as.data.frame(do.call(rbind, colLST)),
                        sizes, 
                        as.data.frame(do.call(rbind, midLST)))
            colnames(df) <- c("color", "size","x","y")
            spdf <- SpatialPointsDataFrame(coords=do.call(rbind, midLST),data=df[,1:2], proj4string=CRS("+proj=longlat"))    
            return(spdf)
          })

#			##add classfication for color and size marco
setGeneric("plotBursts", function(object, add=TRUE, ...){standardGeneric("plotBursts")})
setMethod(f = "plotBursts", 
          signature = c(object="list"),
          definition = function(object, add, ...){
            lapply(object, FUN=plotBursts, add=add, ...)
          })

setMethod(f = "plotBursts", 
          signature = c(object="SpatialPointsDataFrame"),
          definition = function(object, add, ...){
            if (add==FALSE) 
              plot(coordinates(object), type="l", ...)
            #if(!add)
              df <- data.frame(color=object@data$color, size=object@data$size, coordinates=coordinates(object))
              apply(df, MARGIN=1, function(x,...){points(x=x[3], y=x[4], cex=as.numeric(x['size']), col=x['color'],...)}, ...)
          })
#             } else {}
#             if(plot) {return(invisible(spdf))}
#             else {return(spdf)}
#spdf <- plotBursts(trackb, data$beh_code, breaks=10, pch=19)
#head(plotBursts(trackb, by=trackb@data$beh_code, plot=F))



setGeneric("lineMidpoint", function(object){standardGeneric("lineMidpoint")})
setMethod(f = "lineMidpoint",
          signature="SpatialPointsDataFrame",
          definition=function(object){
            track <- coordinates(object)
            #should we include a check for projection bart
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
            midSP <- SpatialPoints(coords=t(as.data.frame(x=mid, row.names=NULL)), proj4string=CRS("+proj=longlat"))
            })

