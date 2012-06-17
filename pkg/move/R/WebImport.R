## Browsing Movebank data base
 OS <- .Platform$OS.type
if(OS=="unix"){
setGeneric("movebankLogin", function(username, password,...) standardGeneric("movebankLogin"))
setMethod(f="movebankLogin", 
          signature=c(username="character", password="character"), 
          definition = function(username, password){
          require(RCurl)
            curl  <- getCurlHandle()
            curlSetOpt( .opts = list(httpheader = c(user = username, password = password),verbose=FALSE), curl=curl)
            testdownload <- getMovebank("study", curl)
            if(getCurlInfo(curl)$response.code==403) {stop("Your username or password is not correct") } 
            return(curl)
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

# study_id=123413&attributes=id%2Clocal_identifier%2Ctaxon_id&entity_type=individual
# url <- "https://www.movebank.org/movebank/service/direct-read?id=19963&attributes=canonical_name&entity_type=taxon"
# url <- "https://www.movebank.org/movebank/service/direct-read?study_id=123413&attributes=attributes=canonical_name&entity_type=taxon"
# url <- "https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=123413&attributes=id%2Clocal_identifier%2Ctaxon_id"
# url <- "https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=123413&attributes=id%2Clocal_identifier%2Ctaxon_id"
# getURL(url, curl=login, verbose=F)

##construct URLs and download from Movebank
setGeneric("getMovebank", function(entity_type, login,...) standardGeneric("getMovebank"))
setMethod(f="getMovebank", 
          signature=c(entity_type="character", login="CURLHandle"), 
          definition = function(entity_type, login, ...){
            tmp <- unlist(list(...))
            url <- paste("https://www.movebank.org/movebank/service/direct-read?entity_type=",entity_type  ,sep="")
            if(length(tmp!=0))
              url <- paste(url, sep="&",paste(names(tmp),tmp, collapse="&", sep="="))
                print(url)
            web <- getURL(url, curl=login, verbose=F)
            data <- read.csv(textConnection(web))
            #print(web)
            return(data)
          })



#names of the studies
setGeneric("searchMovebankStudies", function(x,login, sensor=FALSE) standardGeneric("searchMovebankStudies"))
setMethod(f="searchMovebankStudies", 
          signature=c(x="character",login="CURLHandle"), 
          definition = function(x,login, sensor=FALSE){  
          data <- getMovebank("study", login, sort="name", attributes="id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
          if (sensor==FALSE){
            res <- as.data.frame(data$name[grepl(x,data$name,useBytes=TRUE)])
            names(res) <- paste("##### Results for ",x,"#####")
            if(nrow(res)>0){return(res)}else{"No study matches your search criteria"}
            } else {
              ### if sensor == TRUE search for studies incldugin this sensor type
            }
          })

setMethod(f="searchMovebankStudies", 
          signature=c(x="character",login="missing"), 
          definition = function(x,login, sensor=FALSE){  
            login=movebankLogin()
            searchMovebankStudies(x=x,login=login, sensor=sensor)
          })



#get all study names
setGeneric("getMovebankStudies", function(x, login) standardGeneric("getMovebankStudies"))
setMethod(f="getMovebankStudies", 
          signature=c(x="missing",login="missing"), 
          definition = function(x,login){
            login <- movebankLogin()
            getMovebankStudies(login=login)
          })

setMethod(f="getMovebankStudies", 
          signature=c(x="missing",login="CURLHandle"), 
          definition = function(x, login){
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
          signature=c(study="missing",login="CURLHandle"), 
          definition = function(study,login){
            data <- getMovebank("tag_type", login)
            cat("##### LIST OF ALL SENSOR TYPES IN MOVEBANK #####\n")
            return(data)
          })

setMethod(f="getMovebankSensors", 
         signature=c(study="numeric",login="CURLHandle"), 
         definition = function(study,login){
           data <- getMovebank("sensor", login, tag_study_id=study)
          return(data)
         })

setMethod(f="getMovebankSensors", 
          signature=c(study="character",login="CURLHandle"), 
          definition = function(study,login){   
            studyNUM  <- getMovebankID(study,login)
            return(getMovebankSensors(studyNUM, login))
         })



setGeneric("getMovebankSensorsAttributes", function(study, login) standardGeneric("getMovebankSensorsAttributes"))
setMethod(f="getMovebankSensorsAttributes", 
          signature=c(study="numeric",login="CURLHandle"), 
          definition = function(study,login){
           data <- getMovebank("sensor", login, tag_study_id=study)
           studySensors <- unique(data$sensor_type_id)
           data2 <- getMovebank("study_attribute", login, study_id=study, sensor_type_id=studySensors[1])
           if(length(studySensors)>1){
            for (i in 2:length(studySensors)){ 
              dataNew <- getMovebank("study_attribute", login, study_id=study, sensor_type_id=studySensors[i])
              data2<-merge(data2,dataNew)
            }
           }         
          cat("##### ATTRIBUTES OF THE SENSORS IN STUDY ID: ",study," \n")
           return(data2)
          })

setMethod(f="getMovebankSensorsAttributes", 
          signature=c(study="character",login="CURLHandle"), 
          definition = function(study,login){   
            studyNUM  <- getMovebankID(study,login)
            return(getMovebankSensorsAttributes(studyNUM, login))
          })
            


###all or a certain ID
setGeneric("getMovebankID", function(x, login) standardGeneric("getMovebankID"))
setMethod(f="getMovebankID", 
          signature=c(x="character", login="missing"), 
          definition = function(x, login){
            login <- movebankLogin()
            getMovebankID(x=x, login=login)
          })

setMethod(f="getMovebankID", 
          signature=c(x="character", login="CURLHandle"), 
          definition = function(x, login){
          data <- getMovebank("study", login, sort="name", attributes="id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
          
          if (x=="all") {
            cat("####### STUDY ID #######\n")
            return(data[ ,c("id","name")])
            }
          else {
            studyNUM <- as.numeric(strsplit(capture.output(data[data$name==x,c("id")]),split=" ")[[1]][[2]])
            studyNUM
            }
          #if data is empty prompt, that study was not found
          })



###retrieving information of a certain study
setGeneric("getMovebankStudy", function(study, login, ...) standardGeneric("getMovebankStudy"))
setMethod(f="getMovebankStudy", 
          signature=c(study="numeric", login="CURLHandle"),
          definition = function(study, login, ...){
              data <- getMovebank("study", login, id=study)
              cat("**** SUMMARY OF THE REQUESTED STUDY: ",levels(data$name)," ****\n")
              return(data)
          })

setMethod(f="getMovebankStudy", 
          signature=c(study="ANY", login="missing"),
          definition = function(study, login, ...){
            login <- movebankLogin()
            getMovebankStudy(study=study,login=login, ...)
          })

setMethod(f="getMovebankStudy", 
          signature=c(study="character", login="CURLHandle"),
          definition = function(study, login, ...){
              studyNUM  <- getMovebankID(study,login)   
              getMovebankStudy(study=studyNUM,login=login, ...)
          }
          )



##get all animals with their IDs
setGeneric("getMovebankAnimals", function(study, login, ...) standardGeneric("getMovebankAnimals"))
setMethod(f="getMovebankAnimals",
          c(study="numeric", login="CURLHandle"),
          definition = function(study, login, ...){
              animals <- getMovebank("sensor", login, tag_study_id=study)
              animalID <- getMovebank("individual", login, study_id=study, attributes="id%2Clocal_identifier")
              if (grepl(pattern="X.p.style", capture.output(animalID)[1])==TRUE) stop("It looks like you are not allowed to download this data.")
              names(animalID) <- c("animalID","animalName")
              animalDF <- cbind(animalID,animals)
              cat("**** LIST OF THE STUDY ANIMALS ****\n")
              return(animalDF)
          })

setMethod(f="getMovebankAnimals",
          c(study="character", login="CURLHandle"),
          definition = function(study, login, ...){
             studyNUM  <- getMovebankID(study,login)   
            getMovebankAnimals(study=studyNUM,login=login, ...)
          })

setMethod(f="getMovebankAnimals",
          c(study="ANY", login="missing"),
          definition = function(study, login, ...){
            login <- movebankLogin()
            getMovebankAnimals(study=study,login=login, ...)
          })



###retrieving data from a certain individual of a study
setGeneric("getMovebankData", function(study,animalName=NA,login, moveObject=TRUE, ...) standardGeneric("getMovebankData"))
setMethod(f="getMovebankData", 
          signature=c(study="ANY",animalName="ANY", login="missing"),
          definition = function(study,animalName,login, ...){
            login <- movebankLogin()
            getMovebankData(study=study, animalName=animalName, login=login, moveObject=moveObject,...)
          })

setMethod(f="getMovebankData", 
          signature=c(study="character",animalName="ANY", login="CURLHandle"),
          definition = function(study,animalName, login, ...){
            studyNUM <- getMovebankID(study, login)
            getMovebankData(study=studyNUM, animalName=animalName, login=login, moveObject=moveObject,...)
          })
          
setMethod(f="getMovebankData", 
            signature=c(study="numeric",animalName="ANY", login="CURLHandle"),
            definition = function(study, animalName, login, moveObject=T, ...){
            data <- getMovebankAnimals(study=study, login)
            attribs <- paste(collapse="%2C",getMovebankSensorsAttributes(study, login)$short_name)
            if (is.na(animalName)) {getMovebankData(study=study, login=login, data=data, attribs=attribs, ...)}
            else {getMovebankData(study=study,animalName=animalName,login=login, data=data, attribs=attribs, ...)}
            })

###create a Move or download data from a single animal within the study
setMethod(f="getMovebankData", 
          signature=c(study="numeric",animalName="character", login="CURLHandle"),
          definition = function(study, animalName, login, moveObject=T, ...){
            #if(animalName!="all"){
            name <- data[data$animalName==animalName,]
            trackDF <- getMovebank("event", login, study_id=study, attributes=attribs, sensor_sensor_type_id=name$sensor_type_id, individual_id=name$animalID)
            if (moveObject==TRUE) {
              studyDF <- getMovebankStudy(study, login)
              sensor <- as.character(getMovebankSensors(,login)[getMovebankSensors(,login)$id==name$sensor_type_id, "external_id"])
              if (any(is.na(trackDF$location_long))|any(is.na(trackDF$location_lat))){   ##ommitt NA fixes
                trackDF <- trackDF[!(is.na(trackDF$location_long)|is.na(trackDF$location_lat)), ]
              }
              coords <- cbind(trackDF$location_long,trackDF$location_lat)
              timestamps <- as.POSIXct(strptime(as.character(trackDF$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC")
              sensorDF <- data.frame(rep(sensor, times=nrow(trackDF)))
              names(sensorDF)  <- "sensor"
              #timesMissedFixes  <- as.POSIXct(origin=0,tz="UTC")                
              spdf <- SpatialPointsDataFrame(
                coords = coords,
                data = sensorDF, #data.frame(sensorDF[rep(1,dim(trackDF[animalName])),]), 
                proj4string = CRS("+proj=longlat +ellps=WGS84"), # proj (function argument ) is not used here Marco
                match.ID = TRUE)
              moveG <- new(".MoveGeneral",
                           dateCreation=Sys.time(),
                           study=as.character(studyDF$name),
                           citation=as.character(studyDF$citation),
                           license=as.character(studyDF$license_terms))
#                 moveTS <- new(".MoveTrackSingle",
#                               timesMissedFixes=timesMissedFixes)
              moveT <- new(".MoveTrack",
                          timestamps = timestamps,
                          spdf)
              move <- new("Move",
                          animal=animalName,
                          species="species name",
                          moveG, #.MoveGeneral
                          #moveTS, #.MoveTrackSingle   ###we need to include this, yet the function does not work at the moment
                          moveT) #.MoveTrack
            return(move)} else{return(trackDF)}
            })
            
###create a MoveStack or download data from all animals within the study
setMethod(f="getMovebankData", 
          signature=c(study="numeric",animalName="missing", login="CURLHandle"),
          definition = function(study, animalName, login, moveObject=T, ...){
                 idData <- getMovebank("individual", login=login, study_id=study)
                 rownames(idData) <- idData$local_identifier
                 colnames(idData)[7] <- "individual.local.identifier" #not the best way!
                 trackDF <- getMovebank("event", login=login, study_id=study)
                 trackDF <- merge(trackDF, idData[,c("id","individual.local.identifier")],by.x="individual_id", by.y="id", all=TRUE)
                  if (moveObject==TRUE){
                    studyDF <- getMovebankStudy(study, login)
                    
                    spdf <- SpatialPointsDataFrame(
                      coords = cbind(trackDF$location_long, trackDF$location_lat),
                      data = data.frame(trackDF[names(trackDF)[!names(trackDF)%in%c("location_lat", "location_long","timestamp")]]), 
                      proj4string = CRS("+proj=longlat +ellps=WGS84"), # proj is not used here
                      match.ID = TRUE)
                   
                    sensorNames <- getMovebankSensors(,login)[,c("id","external_id")] ##names of sensors
                    studySensors <- getMovebankSensors(study,login)
                    DATA  <- data.frame(
                      sensor.type = merge(sensorNames,studySensors,by.x="id",by.y="sensor_type_id",)[,"external_id"],
                      individual.taxon.canonical.name=rep("species",times=nrow(idData)),
                      tag.local.identifier=studySensors$tag_id, #paste("#",names$tag_id,sep=""),
                      #individual.local.identifier=as.character(names$animalName),
                      study.name=rep(as.character(studyDF$name),times=nrow(idData)))
                    
                    timestamps <- as.POSIXct(strptime(as.character(trackDF$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC")
                    
                    move <- new("MoveStack", 
                               spdf, 
                               idData = data.frame(DATA,idData),
                               timestamps = timestamps, 
                               trackId = as.factor(trackDF$individual.local.identifier))
 
                return(move)} else{return(trackDF)}
                })

}