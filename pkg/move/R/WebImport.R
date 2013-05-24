## Class to login without using RCurl
setClass(Class = "MovebankLogin",
	 representation = representation(username = "character", password = "character", rcurl = "logical"),
	 prototype = prototype(username = as.character(), password = as.character()),
	 validity = function(object){
		 if(nchar(object@username)==0 || nchar(object@password==0))
			 return(TRUE)
	 }
	 )
## Browsing Movebank data base
setGeneric("movebankLogin", function(username, password,...) standardGeneric("movebankLogin"))
setMethod(f="movebankLogin", 
	  signature=c(username="character", password="character"), 
	  definition = function(username, password){
		  if(any(grepl("RCurl", installed.packages()))) {rcurl <- TRUE} else {rcurl <- FALSE}
		  if (!rcurl) warning("You are using an unsecure connection via http. To use https install RCurl.")
		  return(new("MovebankLogin", username=username, password=password, rcurl=rcurl))
	  })

setMethod(f="movebankLogin", 
	  signature=c(username="character", password="missing"),
	  definition = function(username, password){

		  pwd<-readline("password:")
		  return(movebankLogin(username=username, password=pwd))
	  })

setMethod(f="movebankLogin", 
	  signature=c(username="missing", password="missing"),
	  definition = function(username, password){

		  user<-readline("username:")
		  return(movebankLogin(username=user))
	  })


##construct URLs and download from Movebank
setGeneric("getMovebank", function(entity_type, login,...) standardGeneric("getMovebank"))
setMethod(f="getMovebank", 
	  signature=c(entity_type="character", login="MovebankLogin"), 
	  definition = function(entity_type, login, ...){
		  tmp <- list(...)
		  url <- paste("://www.movebank.org/movebank/service/direct-read?entity_type=",entity_type  ,sep="")
		  try(if(any(names(tmp)=="id")&class(unlist(tmp['id']))=="character") 
		      tmp['id'] <- getMovebankID(unlist(tmp['id']), login) ,silent=T)
		  try(if(any(names(tmp)=="study_id")&class(unlist(tmp['study_id']))=="character") 
		      tmp['study_id'] <- getMovebankID(unlist(tmp['study_id']), login) ,silent=T)
		  try(if(any(names(tmp)=="tag_study_id")&class(unlist(tmp['tag_study_id']))=="character") 
		      tmp['tag_study_id'] <- getMovebankID(unlist(tmp['tag_study_id']), login) ,silent=T)
		  if(length(tmp)!=0){
			  tmp <- lapply(tmp, paste, collapse='%2C')
			  url <- paste(url, sep="&",paste(names(tmp),tmp, collapse="&", sep="="))
		  }
		  if (login@rcurl){
			  require(RCurl)
			  curl  <- getCurlHandle()
			  curlSetOpt( .opts = list(httpheader = c(user = login@username, password = login@password),verbose=FALSE), curl=curl)
			  url <- paste("https", url, sep="")  
			  web <- getURL(url, curl=curl, verbose=F, .encoding="UTF-8")
			  if(grepl(pattern="The requested download may contain copyrighted material", x=web)) stop("You need a permission to access this data set. Go to www.movebank.org and accept the license terms when downloading the data set (you only have to do this once per data set).")
			  data <- read.csv(textConnection(web))
		  } else {
			  url <- paste(paste("http",url, sep=""), sep="&",paste("user=",login@username,"&password=",login@password, sep=""))
			  data <- read.csv(url, header=T, sep=",", as.is=T)
		  }
		  if(grepl(pattern="You.may.only.download.it.if.you.agree.with.the.terms", x=names(data)[1])) stop("You need a permission to access this data set. Go to www.movebank.org and accept the license terms when downloading the data set (you only have to do this once per data set).")
		  if (grepl(pattern="X.html..head..title.Apache.Tomcat", capture.output(data)[1])) stop("It looks like you are not allowed to download this data set. Or there is a sensor for which no attributes are available.")
		  if (grepl(pattern="are.not.available.for.download", capture.output(data)[1])) stop("You have no permission to download this data set.")
		  if (any(grepl(pattern="503 Service Temporarily Unavailable", unlist(head(data))))) stop("Movebank is (temporarily) unavailable")
		  return(data)
	  })

setMethod(f="getMovebank", 
	  signature=c(entity_type="character", login="missing"), 
	  definition = function(entity_type, login, ...){
		  d<-movebankLogin()
		  getMovebank(entity_type=entity_type, login=d,...)
	  })


#names of the studies
setGeneric("searchMovebankStudies", function(x,login) standardGeneric("searchMovebankStudies"))
setMethod(f="searchMovebankStudies", 
	  signature=c(x="character",login="MovebankLogin"), 
	  definition = function(x,login){  
		  data <- getMovebank("study", login, sort="name", attributes="id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
		  res <- c(data$name[grepl(x,data$name,useBytes=TRUE)])
		  #    names(res) <- paste("##### Results for ",x,"#####")
		  if(length(res)>0){return(res)}else{"No study matches your search criteria"}
	  })

setMethod(f="searchMovebankStudies", 
	  signature=c(x="character",login="missing"), 
	  definition = function(x,login){  
		  login=movebankLogin()
		  searchMovebankStudies(x=x,login=login)
	  })



#get all study names
setGeneric("getMovebankStudies", function(login) standardGeneric("getMovebankStudies"))
setMethod(f="getMovebankStudies", 
	  signature=c(login="missing"), 
	  definition = function(login){
		  login <- movebankLogin()
		  getMovebankStudies(login=login)
	  })

setMethod(f="getMovebankStudies", 
	  signature=c(login="MovebankLogin"), 
	  definition = function(login){
		  data <- getMovebank("study", login, sort="name", attributes="id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
		  return(data$name)
	  })


#names of the sensors
setGeneric("getMovebankSensors", function(study, login) standardGeneric("getMovebankSensors"))
setMethod(f="getMovebankSensors", 
	  signature=c(study="ANY",login="missing"), 
	  definition = function(study,login){
		  login <- movebankLogin()
		  getMovebankSensors(study=study, login=login)
	  })
setMethod(f="getMovebankSensors", 
	  signature=c(study="missing",login="missing"), 
	  definition = function(study,login){
		  login <- movebankLogin()
		  getMovebankSensors(login=login)        
	  })

setMethod(f="getMovebankSensors", 
	  signature=c(study="missing",login="MovebankLogin"), 
	  definition = function(study,login){
		  data <- getMovebank("tag_type", login)
		  #cat("##### LIST OF ALL SENSOR TYPES IN MOVEBANK #####\n")
		  return(data)
	  })

setMethod(f="getMovebankSensors", 
	  signature=c(study="ANY",login="MovebankLogin"), 
	  definition = function(study,login){      
		  data <- getMovebank("sensor", login, tag_study_id=study)
		  return(data)
	  })



setGeneric("getMovebankSensorsAttributes", function(study, login) standardGeneric("getMovebankSensorsAttributes"))
setMethod(f="getMovebankSensorsAttributes", 
	  signature=c(study="ANY",login="MovebankLogin"), 
	  definition = function(study,login){
		  data <- getMovebank("sensor", login, tag_study_id=study)
		  studySensors <- unique(data$sensor_type_id)
		  data2 <- lapply(studySensors, function(y, login, study) {try(getMovebank("study_attribute", login, study_id=study, sensor_type_id=y), silent=T)} ,login=login, study=study)
		  data2<-data2[(lapply(data2, class))!='try-error']
		  #data2 <- getMovebank("study_attribute", login, study_id=study, sensor_type_id=studySensors[1])
		  #            if(length(studySensors)>1){
		  #             for (i in 2:length(studySensors)){ 
		  #               dataNew <- getMovebank("study_attribute", login, study_id=study, sensor_type_id=studySensors[i])
		  #               data2<-merge(data2,dataNew)
		  #             }
		  #            }         
		  #cat("##### ATTRIBUTES OF THE SENSORS IN STUDY ID: ",study," \n")
		  return(as.data.frame(do.call(rbind, data2)))
	  })



###all or a certain ID
setGeneric("getMovebankID", function(study, login) standardGeneric("getMovebankID"))
setMethod(f="getMovebankID", 
	  signature=c(study="character", login="missing"), 
	  definition = function(study, login){
		  login <- movebankLogin()
		  getMovebankID(study=study, login=login)
	  })

setMethod(f="getMovebankID", 
	  signature=c(study="character", login="MovebankLogin"), 
	  definition = function(study=NA, login){
		  data <- getMovebank("study", login, sort="name", attributes="id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
		  if (is.na(study)) {
			  #cat("####### STUDY ID #######\n")
			  return(data[ ,c("id","name")])
		  } else {
			  studyNUM <- data[gsub(" ","", data$name)==gsub(" ","", study),c("id")] #get rid of all spaces to avoid miss matching between different spaced words
			  if (length(studyNUM)>1) stop(paste("There was more than one study with the name:",study))
			  return(studyNUM)
		  }
	  })



###retrieving information of a certain study
setGeneric("getMovebankStudy", function(study, login) standardGeneric("getMovebankStudy"))
setMethod(f="getMovebankStudy", 
	  signature=c(study="ANY", login="MovebankLogin"),
	  definition = function(study, login){
		  data <- getMovebank("study", login, id=study)
		  #cat("**** SUMMARY OF THE REQUESTED STUDY: ",levels(data$name)," ****\n")
		  return(data)
	  })

setMethod(f="getMovebankStudy", 
	  signature=c(study="ANY", login="missing"),
	  definition = function(study, login){
		  login <- movebankLogin()
		  getMovebankStudy(study=study,login=login)
	  })


##get all animals with their IDs
setGeneric("getMovebankAnimals", function(study, login) standardGeneric("getMovebankAnimals"))
setMethod(f="getMovebankAnimals",
	  c(study="ANY", login="MovebankLogin"),
	  definition = function(study, login){  
		  if(class(study)=="character") study  <- getMovebankID(study,login)
		  tags <- getMovebank(entity_type="sensor", login, tag_study_id=study)
		  animalID <- getMovebank("individual", login, study_id=study, attributes="id%2Clocal_identifier")
		  deploymentID <- getMovebank("deployment", login=login, study_id=study, attributes="individual_id%2Ctag_id%2Cid")
		  #  if (nrow(deploymentID)==0) warning("There are no deployment IDs available!")
		  names(deploymentID)  <- c("individual_id", "tag_id", "deployment_id")   
		  if (nrow(tags)!=0){ 
			  tagdep <- merge.data.frame(x=tags, y=deploymentID, by.x="tag_id", by.y="tag_id", all=TRUE) #skipping tags that have no deployment
			  tagdepid <- merge.data.frame(x=tagdep, y=animalID, by.x="individual_id", by.y="id", all.y=TRUE)[,-3]#skipping the column of the movebank internal tag id
			  colnames(tagdepid) <- c("individual_id", "tag_id", "sensor_type_id", "deployment_id", "animalName")
			  #if (any(apply(deploymentID[,1:2], 2, FUN=duplicated)))##if there are multiple deployments: idData$local_identifier  <-  paste(localID_deploymentID)
			  if (any(duplicated(tagdepid$individual_id)|duplicated(tagdepid$tag_id))){
				  tagdepid$animalName <- paste(tagdepid$animalName, tagdepid$deployment_id, sep="_")
				  names(tagdepid)  <- c("individual_id", "tag_id", "sensor_type_id", "deployment_id", "animalName_deployment")}
			  return(tagdepid) 
		  } else {
			  return(merge.data.frame(x=deploymentID, y=animalID, by.x="individual_id", by.y="id", all.y=TRUE))
		  }
	  })

setMethod(f="getMovebankAnimals",
	  c(study="ANY", login="missing"),
	  definition = function(study, login){
		  login <- movebankLogin()
		  getMovebankAnimals(study=study,login=login)
	  })



setGeneric("getMovebankData", function(study,animalName=NA,login, ...) standardGeneric("getMovebankData"))
setMethod(f="getMovebankData", 
	  signature=c(study="ANY",animalName="ANY", login="MovebankLogin"),
	  definition = function(study, animalName, login, ...){ 
		  if (class(study)=="character") study <- getMovebankID(study, login) 
		  idData <- getMovebank("individual", login=login, study_id=study)         
		  ##which deployments are imporant
		  deploymentID <- getMovebank("deployment", login=login, study_id=study, attributes="individual_id%2Ctag_id%2Cid") 
		  ##which track Data are important
		  sensors <- getMovebankSensors(study=study, login=login)
		  names(sensors)  <- c("sensor", "sensor_type_id", "tag_id") 
		  #		  new <- merge.data.frame(deploymentID, sensors, by.x="tag_id", by.y="tag_id") 
		  new <- merge.data.frame(deploymentID, idData, by.x="individual_id", by.y="id")
		  if (!all(is.na(animalName))) {
			  new <- new[new$local_identifier%in%animalName, ]
			  if(length(animalName)!=length(unique(new$individual_id))) stop("One or more animal names are spelled incorrectly.")
		  }
		  b <- getMovebank("tag_type", login=login)
		  locSen <- b[as.logical(b$is_location_sensor),"id"] #reduce track to location only sensors & only the correct animals
		  attribs <- c(as.character(getMovebankSensorsAttributes(study, login)$short_name),"sensor_type_id","deployment_id")
		  trackDF <- getMovebank("event", login=login, study_id=study, attributes = attribs , deployment_id=unique(new$id), sensor_type_id=locSen)
		  trackDF$timestamp<-as.POSIXct(strptime(as.character(trackDF$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC")
		  outliers<- is.na(trackDF$location_long)
		  if('algorithm_marked_outlier'%in%names(trackDF))
			  outliers[trackDF$algorithm_marked_outlier=="true"]<-T
		  if('manually_marked_outlier'%in%names(trackDF)){
			  outliers[trackDF$manually_marked_outlier=="true"]<-T
			  outliers[trackDF$manually_marked_outlier=="false"]<-F
		  }
		  spdf<-SpatialPointsDataFrame(trackDF[!outliers,c('location_long','location_lat')], data=trackDF[!outliers,], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), match.ID=T)
		  if(any(duplicated(new$local_identifier)))
			  new$local_identifier<-paste0(new$local_identifier,'_', new$id)
		  rownames(new)<-raster:::.goodNames(new$local_identifier)
		  id<-paste(format(trackDF$timestamp,"%Y %m %d %H %M %OS4"),trackDF$deployment_id, trackDF$sensor_type_id)
		  if(any(s<-id[outliers]%in%id[!outliers]))
		  {
			  warning("There are timestamps ",sum(s)," in the unused data that are also in the real data, those records are omitted")
			  outliers[outliers][s]<-F
		  }
		  if(any(s<-duplicated(id[outliers])))
		  {
			  warning("There are ",sum(s), " duplicated timestamps in the unused that those will be removed")
			  outliers[outliers][s]<-F
		  }
		  name<-b$name
		  names(name)<-b$id
		  local_identifier<-factor(rownames(new))
		  names(local_identifier)<-new$id

		  new<-new[new$id %in% unique(spdf$deployment_id),]
		  unUsed<-	  new('.unUsedRecordsStack', dataUnUsedRecords=trackDF[outliers,],timestampsUnUsedRecords=trackDF$timestamp[outliers], 
					sensorUnUsedRecords= name[as.character(trackDF[outliers,'sensor_type_id'])],#, b, by.x='sensor_type_id', by.y='id')$name, 
					trackIdUnUsedRecords=droplevels(local_identifier[as.character(trackDF[outliers,'deployment_id'])]))#, new, by.x='deployment_id', by.y='id')$local_identifier, 
		  if(any(!(s<-unUsed@trackIdUnUsedRecords %in% local_identifier[unique(as.character(spdf$deployment_id))])))
		  {
			  warning('Omiting individual(s) (n=',length(unique(unUsed@trackIdUnUsedRecords[!s])), ') that have only unUsedRecords')
			  unUsed<-unUsed[s,]
			  unUsed@trackIdUnUsedRecords<-droplevels(unUsed@trackIdUnUsedRecords)
		  }
		  res<-new("MoveStack", spdf, timestamps=spdf$timestamp, 
			   sensor=name[as.character(spdf$sensor_type_id)],
			   trackId=droplevels(local_identifier[as.character(spdf$deployment_id)]),
			   unUsed,
			   idData=new)
		  if(length(unique(res@trackId))==1)
			  res<-as(res, 'Move')
		  return(res)
		  #       new <- new[new$id%in%unique(trackDF[,"deployment_id"]), ]
		  #       
		  #       trackDF <- merge.data.frame(x=trackDF, y=new[, c(names(new)[!names(new)%in%names(trackDF)], "individual_id")], by.x="individual_id", by.y="individual_id", all=TRUE) 
		  #       trackDF$sensor_type_id <- as.vector(unlist(lapply(trackDF$sensor_type_id, function (y,b){b$external_id[which(b$id==y)]  },b=b))) 
		  #       if (!any(is.na(new$local_identifier)) & length(unique(new$individual_id))==length(unique(new$local_identifier))) 
		  #         trackDF$individual_id <- rep(unique(new$local_identifier), unlist(lapply(lapply(unique(trackDF$individual_id), "==", trackDF$individual_id), sum)))            
		  #       ##multiple sensors per tag
		  #       #                 if (length(unique(new$id))!=length(unique(new$sensor_id))){
		  #       #                   trackDF$individual_id  <- paste(trackDF$individual_id, trackDF$deployment_id, trackDF$sensor_type_id, sep="_") ##ADD sensorID!!!
		  #       #                 } else {
		  #       ##individuals with multiple deployments?
		  #       if (length(paste(new$id, new$individual_id, sep="_"))!=length(unique(new$id)))
		  #         trackDF$individual_id <- paste(trackDF$individual_id, trackDF$deployment_id, trackDF$tag_id, sep="_")
		  #       studyDF <- getMovebankStudy(study, login)
		  #       trackDF$study.name <- rep(as.character(studyDF$name),times=nrow(trackDF))
		  #       trackDF$citation <- rep(as.character(studyDF$citation),times=nrow(trackDF))
		  #       trackDF$timestamp <- as.POSIXct(strptime(as.character(trackDF$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC")
		  #       names(trackDF) <- gsub('_', '.', names(trackDF))
		  #       names(trackDF) <- gsub('local.identifier','individual.local.identifier',names(trackDF))
		  #       trackDF$study.name <- gsub(' +', " ", trackDF$study.name)
		  #       .move(df=trackDF, proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
	  })
