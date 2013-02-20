setGeneric("move", function(x, y, time, data, proj=NA, ...) standardGeneric("move"))
setMethod(f = "move", 
	  signature = c(x="character",y='missing',time='missing', data='missing', proj='missing'), 
	  definition = function(x){
		  if(!file.exists(x))
			  stop("x should be a file on disk but it cant be found")
		  df <- read.csv(x, header=TRUE, sep=",", dec=".")

		  if (!all(c("timestamp", "location.long",  "location.lat", "study.timezone", "study.local.timestamp", "sensor.type", "individual.local.identifier", "individual.taxon.canonical.name")%in%colnames(df)))
			  stop("The entered file does not seem to be from Movebank. Please use the alternative import function.")

		  if(any(dups<-duplicated( do.call('paste',c(df[duplicated(df$timestamp)|duplicated(df$timestamp, fromLast=T),names(df)!="event.id"], list(sep="__")))))){#first find atleast the ones where the timestamp (factor) is duplicated
			  warning("Exact duplicate records removed (n=",sum(dups),") (movebank allows them but the move package can't deal with them)")
			  df <- df[!duplicated( do.call('paste',c(df[,names(df)!="event.id"], list(sep="__")))),]# cant use dups here since it that uses the optimization of only looking at timestamps first
		  }

		  df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC") # need to make character out of it to ensure milli seconds are considerd
		  df$study.local.timestamp <- as.POSIXct(strptime(df$study.local.timestamp, format="%Y-%m-%d %H:%M:%OS"))

		  if(any(tapply(df$sensor.type, df$individual.local.identifier, length)!=1)){
			  df <- df[with(df, order(df$individual.local.identifier, timestamp)), ]  
		  }
		  df$individual.local.identifier<-as.factor( df$individual.local.identifier)
		  levels(df$individual.local.identifier) <- raster:::.goodNames(levels(factor(df$individual.local.identifier))) #changing names to 'goodNames' skipping spaces

		  if("visible" %in% colnames(df))
		  {
			  v<-df$visible=='false'
		  }else{
			  v<-F
	  	  }
		  unUsed<-is.na(df$location.long)|is.na(df$location.lat)|v| is.na(df$individual.local.identifier)| df$individual.local.identifier==''
		  sensor<-df$sensor.type
		  timestamps<-df$timestamp
		  individual.local.identifier<-df$individual.local.identifier
		  uniquePerID <- unlist(lapply(df,  function(x,y){all(tapply(x,y,function(x){length(unique(x))})==1)}, y=factor(df$individual.local.identifier)))
		  idData <- subset(df, select=names(uniquePerID[uniquePerID]), !duplicated(df$individual.local.identifier))
		  rownames(idData)<-idData$individual.local.identifier
		  df<-df[,!(names(df)%in%unique(c('sensor.type','timestamps', colnames(idData))))]
		  unUsedDf<-df[unUsed,]
		  df<-df[!unUsed,]
		  coordinates(df)<- ~location.long+location.lat

		  proj4string(df)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

		  track<-new('.MoveTrack', df, timestamps=timestamps[!unUsed], sensor=sensor[!unUsed], idData=idData)

		  unUsedRecords<-new('.unUsedRecords', dataUnUsedRecords=unUsedDf, timestampsUnUsedRecords=timestamps[unUsed], sensorUnUsedRecords=sensor[unUsed])
		  if(nrow(idData)!=1){
			  unUsedRecords<-new('.unUsedRecordsStack', unUsedRecords, trackIdUnUsedRecords=individual.local.identifier[unUsed])
			  return(new('MoveStack', track, unUsedRecords, trackId=individual.local.identifier[!unUsed]))
		  }else{
			  return(new('Move',track, unUsedRecords))
		  }
	  }
	  )

#if non-Movebank data are used, table is new defined 
setMethod(f="move",
	  signature=c(x="numeric", y="numeric", time="POSIXct", data="missing", proj="ANY"),
	  definition = function(x,y,time,data,proj, ...){
		  data<-data.frame(x,y,time)
		  move(x=x,y=y,time=time,proj=proj,data=data,...)
	  }
	  )
setMethod(f="move",
	  signature=c(x="numeric", y="numeric", time="POSIXct", data="data.frame", proj="ANY"),
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
	  signature = c(df="data.frame", proj="ANY"), 
	  definition = function(df, proj){

		  if(any(is.na(df$location.long))){ 
			  warning("There were NA locations detected and omitted. Currently they are not stored in unusedrecords")
			  df <- df[!(is.na(df$location.long)|is.na(df$location.lat)), ]
		  }
		  df$individual.local.identifier<-factor(df$individual.local.identifier)
		  levels(df$individual.local.identifier) <- raster:::.goodNames(levels(factor(df$individual.local.identifier))) #changing names to 'goodNames' skipping spaces
		
		  if(length(unique(df$individual.local.identifier))>1 & any(unique(as.character(df$individual.local.identifier))==""))
		  {
			  warning("Omitting locations that have an empty local identifier (n=",sum(tmp<-as.character(df$individual.local.identifier)==""),"). Most likely the tag was not deployed") 
			  df <- df[!tmp,]
			  df$individual.local.identifier <- factor(df$individual.local.identifier)
		  }
		  ids <- as.list(as.character(unique(df$individual.local.identifier)))
		  uniquePerID <- unlist(lapply(df,  function(x,y){all(tapply(x,y,function(x){length(unique(x))})==1)}, y=factor(df$individual.local.identifier)))
		  uniquePerID["sensor"] <- FALSE
		  idData <- subset(df, select=names(uniquePerID[uniquePerID]), !duplicated(df$individual.local.identifier))

		  if(length(names(idData))!=1)# dont shorten it because we need something
			  idData<-subset(idData, select=names(idData)!="individual.local.identifier")

		  if(length(unique(idData$citation))>1) 
		  {
			  warning("There were more than one citation for this study found! Only using the first.")
			  citations <- as.character(unique(idData$citation))[1]
		  }

		  if(length(unique(idData$citation))==1) 
		  {citations <- as.character(unique(idData$citation))} else {citations <- character()}

		  rownames(idData) <- unique(df$individual.local.identifier)
		  data <- data.frame(df[names(df)[!names(df)%in%c("location.lat", "location.long","timestamp", colnames(idData))]])

		  if (ncol(data)==0) data <- data.frame(data, empty=NA)

		  if(!is(proj,"CRS")) proj <- CRS(proj)
		  tmp <- SpatialPointsDataFrame(
						coords = cbind(df$location.long,df$location.lat),
						data = data, 
						proj4string = proj,
						match.ID = TRUE)
		  df$sensor<-factor(df$sensor)

		  if (length(ids)==1){
			  res <- new("Move", 
				     timestamps = df$timestamp, 
				     sensor = df$sensor,
				     sensorUnUsedRecords=factor(levels=levels(df$sensor)),
				     tmp, 
				     citation = citations,
				     idData = idData
				     )
		  } else {
trackId<-factor(df$individual.local.identifier)
			  res <- new("MoveStack", 
				     tmp, 
				     idData = idData,
				     sensor = df$sensor,
				     sensorUnUsedRecords=factor(levels=levels(df$sensor)),
				     timestamps = df$timestamp, 
				     citation = citations,
				     trackId = trackId,
				     trackIdUnUsedRecords=factor(levels=levels(trackId)))}
		  return(res)
	  })
