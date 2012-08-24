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
				   idData = "data.frame",
				   sensor="factor"),
	       prototype = prototype(
           timestamps = as.POSIXct(NA),
	   idData = data.frame(),
	         sensor=factor()),
      	 validity = function(object){
      			if(length(object@timestamps)!=nrow(object@coords))
      				stop("Number of timestamps does not match the number of coordinates")
      			if(length(object@sensor)!=nrow(object@coords))
      				stop("Number of sensors observations does not match the number of coordinates")
      			return(TRUE)
	 	    }
	      )


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
      			if(nrow(object@idData)>1)
              stop("There are more than 1 row stored in the idData")
            #if (length(object@burstID)!=nrow(coordinates(object)))
              #stop("The length of burst IDs is not equal to the number of locations")
			      return(TRUE)
	 	    }
	      )

setClass(Class = "Move", contains=c(".MoveTrackSingle",".MoveGeneral"),
       	 representation = representation (
       	   ),
      	 prototype = prototype(
           ),
      	 validity = function(object){
      			return(TRUE)
      	 }
      	 )


 setAs(".MoveTrack","data.frame", function(from){return(data.frame(data.frame(from), sensor=from@sensor, timestamps=from@timestamps))})

setMethod("[", signature(x=".MoveTrack"), function(x, i, j, ...) {
       if(!missing(i)){
       	 x@timestamps <- x@timestamps[i]
       	 x@sensor <- x@sensor[i]
       	}else{i<-T}
       if(missing(j))
       	j<-T
	  if(class(i)=="character")
		  stop("Not sure if these methods work for class character")
	  if(class(j)=="character")
		  stop("Not sure if these methods work for class character")
       	 callNextMethod(x=x,i=i,j=j,...)
})


setGeneric("move", function(x, y, time, data, proj, ...) standardGeneric("move"))
setMethod(f = "move", 
      	  signature = c(x="character",y='missing',time='missing', data='missing', proj='missing'), 
      	  definition = function(x){
		  if(!file.exists(x))
			  stop("x should be a file on disk but it cant be found")
      		df <- read.csv(x, header=TRUE, sep=",", dec=".")
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
		  df$sensor<-df$sensor.type 
		  df <- df[,names(df)!="sensor.type"]

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
	          data$sensor <- sensor 
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
            #df$sensor<-df$sensor.type 
            #if(is.null(df$sensor.type)) df$sensor <- rep(NA, nrow(df)) else df$sensor<-df$sensor.type
            #df <- df[,names(df)!="sensor.type"]

	    if(length(unique(df$individual.local.identifier))>1 & any(unique(as.character(df$individual.local.identifier))==""))
	    {# this is not so elegant from me (bart) since this function also gets used by non movebank data
  		    warning("omitting locations that have and empty local identifier (n=",sum(tmp<-as.character(df$individual.local.identifier)==""),") most likely the tag was not deployed") 
  		    df<-df[!tmp,]
  		    df$individual.local.identifier<-factor(df$individual.local.identifier)
        }
        ids <- as.list(as.character(unique(df$individual.local.identifier)))
	    #this function should both work for one and multiple individuals
        uniquePerID<-apply(df, MARGIN=2, function(x,y){all(tapply(x,y,function(x){length(unique(x))})==1)}, y=factor(df$individual.local.identifier))
			  uniquePerID["sensor"]<-FALSE
	      idData<-subset(df, select=names(uniquePerID[uniquePerID]), !duplicated(individual.local.identifier))
	      if(length(names(idData))!=1)# dont shorten it because we need something
	        idData<-subset(idData, select=names(idData)!="individual.local.identifier")
            
        if(length(unique(idData$citation))==1) citation <- as.character(unique(idData$citation)) else citation <- character()
        if(length(unique(idData$citation))>1) {
          warning("There were more than one citation for this study found! Only using the first.")
          citation <- as.character(unique(idData$citation))[1]}
        #idData <- idData[,names(idData)!="citation"]
        rownames(idData) <- unique(df$individual.local.identifier)
        data <- data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long","timestamp", colnames(idData))]])
        if (ncol(data)==0) data <- data.frame(data, empty=NA)
        tmp <- SpatialPointsDataFrame(
                coords = cbind(df$location.long,df$location.lat),
                data = data, 
                proj4string = proj,
                match.ID = TRUE)
        if (length(ids)==1){
          res <- new("Move", 
                     timestamps = df$timestamp, 
	                   sensor = factor(df$sensor),
                     tmp, 
                     citation = citation,
                     idData = idData,
                     timesMissedFixes = missedFixes)
          } else {
           res <- new("MoveStack", 
             	        tmp, 
             	        idData = idData,
	                    sensor = factor(df$sensor),
           		        timestamps = df$timestamp, 
                      citation = citation,
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
          })


###extract time.lag from Move
if (!isGeneric("time.lag")) {setGeneric("time.lag", function(x, ...) standardGeneric("time.lag"))}
setMethod("time.lag", ".MoveTrackSingle", function(x, ...){
	        return(as.numeric(diff(x@timestamps),...)) #one less than locations! we need a more elegant way than just adding a zero 
          })

  
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

###plotting 
setGeneric("plot") 
setMethod(f = "plot", 
          signature = c(x=".MoveTrackSingle", y="missing"), 
          function(x, y, ...){
            plot(coordinates(x), type="p", ...)
            #points(x, add=FALSE, ...)
            lines(x, ...)
          })


setGeneric("points")
setMethod("points", ".MoveTrackSingle", function(x,add=TRUE,...){
         if (add==FALSE) {rm(add)
                          plot(coordinates(x), type="p", ...)}
         else {points(coordinates(x), type="p", ...)}
         })

###MARCO change all plot functions to plot, default is points and lines, user can also plot p or l OR add p and l for Move AND MoveStacks
setGeneric("lines") ##need to be different -- 
setMethod("lines", ".MoveTrackSingle", function(x,add=TRUE,...){
          if (add==FALSE) {rm(add)
                           plot(coordinates(x), type="l", ...)}
          else {lines(coordinates(x), type="l", ...)}
          })


setGeneric("googleplot", function(obj, ...){standardGeneric("googleplot")})
setMethod(f = "googleplot", 
          signature = c(obj=".MoveTrack"), 
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
            })
# setMethod("show", "MoveStack", function(object){
#   print(object) 
# }         
# )




#			#Find below the functions to plot a "centroid" point on the line of a certain line segment
setGeneric("burstTrack", function(object, by, breaks=5, sizeFUN="relTime"){standardGeneric("burstTrack")}) 
# setMethod(f = "burstTrack", 
#           signature = c(object="MoveStack", by="integer"),
#           definition = function(object, by, breaks, sizeFUN){
#             moveUnstacked <- split(x=object) #split MoveStack into individual Move objects
#             spdfLST <- as.list(lapply(moveUnstacked, FUN=burstTrack, by=by, breaks=breaks, sizeFUN=sizeFUN))                             
#           })##function was not tested yet


setMethod(f = "burstTrack", 
          signature = c(object=".MoveTrackSingle", by="numeric"),
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
                      sizeLST  <- FUN(ll, object)}
            sizes  <- as.numeric(cut(unlist(sizeLST), breaks=breaks))/max(as.numeric(cut(unlist(sizeLST), breaks=breaks))) 
            df <- cbind(as.data.frame(do.call(rbind, colLST)),
                        sizes, 
                        #as.data.frame(do.call(rbind, midLST)))
                        data.frame(do.call(rbind, midLST)))
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
            if (!grepl("longlat",proj4string(object))) stop(paste("Coordinates need longlat projection! Projection is:",proj4string(object)))
            if (nrow(track)<2) { ##make the point at the coordinate
              mid <- t(coordinates(track))
            } else {
              if (nrow(track)==2) { ##make the point at the center of the line
                mid <- (coordinates(track)[2,]-coordinates(track)[1,])*.5+coordinates(track)[1,]
              }
              if (nrow(track)>2){
                # dreck <- cbind(as.data.frame(track)[-nrow(track),], as.data.frame(track)[-1,])
                # names(dreck) <- c("X1", "Y1", "X2", "Y2")
                # segmentlength <- function(dreck)
                # {
                #   spDistsN1(as.matrix(t(dreck[1:2])), as.matrix(t(dreck[3:4])), longlat=FALSE)
                # }
                #dists <- apply(dreck, 1, segmentlength)
                dists <- seglength(object)
                
                totalDist <- sum(dists)
                cumsum <- cumsum(dists)
                lcum <- list(cumsum)
                
                min <- which(cumsum(dists)>0.5*sum(dists))[1]
                prop <- (cumsum(dists)[min]-sum(dists)/2)/cumsum(dists)[min]
                mid <- (coordinates(track)[min+1,]-coordinates(track)[min,])*prop+coordinates(track)[min,]
              }
            }
            midSP <- SpatialPoints(coords=t(as.data.frame(x=mid, row.names=NULL)), proj4string=CRS("+proj=longlat"))
            #return(midSP)
            })

setGeneric("timestamps", function(this) standardGeneric("timestamps"))
setMethod("timestamps", ".MoveTrack",
   function(this) {
      this@timestamps
   })

setMethod("timestamps", ".MoveTrackSingle",
          function(this) {
            this@timestamps
          })

setGeneric("timestamps<-", function(this, value) standardGeneric("timestamps<-"))
setReplaceMethod("timestamps", ".MoveTrack",
   function(this, value) {
     if (length(value)!=length(this@timestamps)) 
       stop(paste("The number of timestamps does not match the original number of timestamps! (",length(value),":",length(this@timestamps),")"))
    this@timestamps <- value
    this
   })



setGeneric("citation", function(obj) standardGeneric("citation"))
setMethod("citation", ".MoveGeneral", function(obj){
                  return(obj@citation)
                })

setGeneric("citation<-", function(obj, value) standardGeneric("citation<-"))
setReplaceMethod("citation", ".MoveGeneral", function(obj, value){
                   if (length(value)!=1) {
                     value <- as.character(value[1])
                     warning("There were more than one citation entered. Only using the first element!")
                   }
                   obj@citation <- value
                   obj
                 })
