## Browsing Movebank data base

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



##construct URLs and download from Movebank
setGeneric("getMovebank", function(entity_type, login,...) standardGeneric("getMovebank"))
setMethod(f="getMovebank", 
          signature=c(entity_type="character", login="CURLHandle"), 
          definition = function(entity_type, login, ...){
            tmp <- unlist(list(...))
            url <- paste("https://www.movebank.org/movebank/service/direct-read?entity_type=",entity_type  ,sep="")
            if(length(tmp!=0))
              url <- paste(url, sep="&",paste(names(tmp),tmp, collapse="&", sep="="))
            #    print(url)
            web <- getURL(url, curl=login, verbose=F)
            data <- read.csv(textConnection(web))
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
  ######
####  #### 
####
 #####
   #####
     ####
####  ####
  ######
setGeneric("getMovebankSensors", function(x, login) standardGeneric("getMovebankSensors"))
setMethod(f="getMovebankSensors", 
          signature=c(x="ANY",login="missing"), 
          definition = function(x,login){
            login <- movebankLogin()
              getMovebankSensors(x=x, login=login)
          })
setMethod(f="getMovebankSensors", 
          signature=c(x="missing",login="missing"), 
          definition = function(x,login){
            login <- movebankLogin()
            getMovebankSensors(login=login)        
          })

setMethod(f="getMovebankSensors", 
          signature=c(x="missing",login="CURLHandle"), 
          definition = function(x,login){
            data <- getMovebank("tag_type", login)
            cat("##### LIST OF ALL SENSOR TYPES IN MOVEBANK #####\n")
            return(data)
          })

setMethod(f="getMovebankSensors", 
         signature=c(x="numeric",login="CURLHandle"), 
         definition = function(x,login){
           data <- getMovebank("sensor", login, tag_study_id=x)
          return(data)
         })

setMethod(f="getMovebankSensors", 
          signature=c(x="character",login="CURLHandle"), 
          definition = function(x,login){   
            studyNUM  <- getMovebankID(x,login)
            return(getMovebankSensors(studyNUM, login))
         })



setGeneric("getMovebankSensorsAttributes", function(x, login) standardGeneric("getMovebankSensorsAttributes"))
setMethod(f="getMovebankSensorsAttributes", 
          signature=c(x="numeric",login="CURLHandle"), 
          definition = function(x,login){
           data <- getMovebank("sensor", login, tag_study_id=x)
           studySensors <- unique(data$sensor_type_id)
           data2 <- getMovebank("study_attribute", login, study_id=x, sensor_type_id=studySensors[1])
           if(length(studySensors)>1){
            for (i in 2:length(studySensors)){ 
              dataNew <- getMovebank("study_attribute", login, study_id=x, sensor_type_id=studySensors[i])
              data2<-merge(data2,dataNew)
            }
           }         
          cat("##### ATTRIBUTES OF THE SENSORS IN STUDY ID: ",x," \n")
           return(data2)
          })

setMethod(f="getMovebankSensorsAttributes", 
          signature=c(x="character",login="CURLHandle"), 
          definition = function(x,login){   
            studyNUM  <- getMovebankID(x,login)
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
setGeneric("getMovebankData", function(study,animalName,login, moveObject=TRUE, ...) standardGeneric("getMovebankData"))
setMethod(f="getMovebankData", 
          signature=c(study="ANY",animalName="ANY", login="missing"),
          definition = function(study,animalName,login, ...){
            login <- movebankLogin()
            getMovebankData(study=study, animalName=animalName, login=login, moveObject=moveObject,...)
          })

setMethod(f="getMovebankData", 
          signature=c(study="character",animalName="character", login="CURLHandle"),
          definition = function(study,animalName, login, ...){
            studyNUM <- getMovebankID(study, login)
            getMovebankData(study=studyNUM, animalName=animalName, login=login, moveObject=moveObject,...)
          })
          
setMethod(f="getMovebankData", 
            signature=c(study="numeric",animalName="character", login="CURLHandle"),
            definition = function(study, animalName, login, moveObject=T, ...){
            data <- getMovebankAnimals(study=study, login)
            attribs <- paste(collapse="%2C",getMovebankSensorsAttributes(study, login)$short_name)
            ddd <- strsplit(as.character(data[data$animalName==animalName,]),split="\t")
            data2 <- getMovebank("event", login, study_id=study, attributes=attribs, sensor_sensor_type_id=as.numeric(ddd[[4]][[1]]), individual_id=as.numeric(ddd[[1]][[1]]))
            cat("##### FIRST FIVE LINES OF THE ANIMAL DATA #####\n") 
            print(data2[1:5,])
            if (moveObject==TRUE) {
              studyDF <- getMovebankStudy(study, login)
              trackDF <- data2
              trackDF$sensor.type <- as.character(getMovebankSensors(,login)[getMovebankSensors(,login)$id==as.numeric(ddd[[4]][[1]]), "external_id"])
              print(moveObject)
              move <- move(x=trackDF, y=studyDF, animal=animalName)
              return(move)
            }
            else{return(data2)}
            })