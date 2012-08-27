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
