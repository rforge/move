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

#getMovebank("event", login, sensor_sensor_type_id=673,study_id=123413)
# study_id=123413&attributes=id%2Clocal_identifier%2Ctaxon_id&entity_type=individual
# url <- "https://www.movebank.org/movebank/service/direct-read?id=19963&attributes=canonical_name&entity_type=taxon"
# url <- "https://www.movebank.org/movebank/service/direct-read?study_id=123413&attributes=attributes=canonical_name&entity_type=taxon"
# url <- "https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=123413&attributes=id%2Clocal_identifier%2Ctaxon_id"
# url <- "https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=123413&attributes=id%2Clocal_identifier%2Ctaxon_id"
# getURL(url, curl=login, verbose=F)

##construct URLs and download from Movebank
setGeneric("getMovebank", function(entity_type, login,...) standardGeneric("getMovebank"))
setMethod(f="getMovebank", 
          signature=c(entity_type="character", login="MovebankLogin"), 
          definition = function(entity_type, login, ...){
            tmp <- unlist(list(...))
            url <- paste("://www.movebank.org/movebank/service/direct-read?entity_type=",entity_type  ,sep="")
            if(length(tmp!=0))
              url <- paste(url, sep="&",paste(names(tmp),tmp, collapse="&", sep="="))
            
            if (login@rcurl){
              require(RCurl)
              curl  <- getCurlHandle()
              curlSetOpt( .opts = list(httpheader = c(user = login@username, password = login@password),verbose=FALSE), curl=curl)
              url <- paste("https", url, sep="")  
              web <- getURL(url, curl=curl, verbose=F, .encoding="UTF-8")
              if(grepl(pattern="The requested download may contain copyrighted material", x=web)) stop("You need a permition to access this data set. Go to movebank.org and accept the license terms when downloading the data set (you only have to do this once per data set).")
              data <- read.csv(textConnection(web))
              if(grepl(pattern="You.may.only.download.it.if.you.agree.with.the.terms", x=names(data)[1])) warning("You need a permition to access this data set. Go to movebank.org and accept the license terms when downloading the data set (you only have to do this once per data set).")
              if (grepl(pattern="X.html..head..title.Apache.Tomcat", capture.output(data)[1])==TRUE) warning("It looks like you are not allowed to download this data.")
            } else {
              url <- paste(paste("http",url, sep=""), sep="&",paste("user=",login@username,"&password=",login@password, sep=""))
              data <- read.csv(url, header=T, sep=",", as.is=T)
            }
print(url)            
            return(data)
          })


#names of the studies
setGeneric("searchMovebankStudies", function(x,login, sensor=FALSE) standardGeneric("searchMovebankStudies"))
setMethod(f="searchMovebankStudies", 
          signature=c(x="character",login="MovebankLogin"), 
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
            cat("##### LIST OF ALL SENSOR TYPES IN MOVEBANK #####\n")
            return(data)
          })

setMethod(f="getMovebankSensors", 
         signature=c(study="numeric",login="MovebankLogin"), 
         definition = function(study,login){      
           data <- getMovebank("sensor", login, tag_study_id=study)
          return(data)
         })

setMethod(f="getMovebankSensors", 
          signature=c(study="character",login="MovebankLogin"), 
          definition = function(study,login){   
            studyNUM  <- getMovebankID(study,login)
            return(getMovebankSensors(studyNUM, login))
         })



setGeneric("getMovebankSensorsAttributes", function(study, login) standardGeneric("getMovebankSensorsAttributes"))
setMethod(f="getMovebankSensorsAttributes", 
          signature=c(study="numeric",login="MovebankLogin"), 
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
          signature=c(study="character",login="MovebankLogin"), 
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
          signature=c(x="character", login="MovebankLogin"), 
          definition = function(x=NA, login){
#browser()            
          data <- getMovebank("study", login, sort="name", attributes="id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see")
          if (is.na(x)) {
            cat("####### STUDY ID #######\n")
            return(data[ ,c("id","name")])
            } else {
              studyNUM <- data[gsub(" ","", data$name)==gsub(" ","", x),c("id")] #get rid of all spaces to avoid miss matching between different spaces words
              if (length(studyNUM)>1) stop(paste("There was more than one study with the name:",x))
            return(studyNUM)
            }
          })



###retrieving information of a certain study
setGeneric("getMovebankStudy", function(study, login) standardGeneric("getMovebankStudy"))
setMethod(f="getMovebankStudy", 
          signature=c(study="numeric", login="MovebankLogin"),
          definition = function(study, login){
              data <- getMovebank("study", login, id=study)
              cat("**** SUMMARY OF THE REQUESTED STUDY: ",levels(data$name)," ****\n")
              return(data)
          })

setMethod(f="getMovebankStudy", 
          signature=c(study="ANY", login="missing"),
          definition = function(study, login){
            login <- movebankLogin()
            getMovebankStudy(study=study,login=login)
          })

setMethod(f="getMovebankStudy", 
          signature=c(study="character", login="MovebankLogin"),
          definition = function(study, login){
              studyNUM  <- getMovebankID(study,login)   
              getMovebankStudy(study=studyNUM,login=login)
          }
          )



##get all animals with their IDs
setGeneric("getMovebankAnimals", function(study, login) standardGeneric("getMovebankAnimals"))
setMethod(f="getMovebankAnimals",
          c(study="numeric", login="MovebankLogin"),
          definition = function(study, login){  
#print("2")  
#browser()           
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
          c(study="character", login="MovebankLogin"),
          definition = function(study, login){
             studyNUM  <- getMovebankID(study,login)   
            getMovebankAnimals(study=studyNUM,login=login)
          })

setMethod(f="getMovebankAnimals",
          c(study="ANY", login="missing"),
          definition = function(study, login){
            login <- movebankLogin()
            getMovebankAnimals(study=study,login=login)
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
          signature=c(study="character",animalName="ANY", login="MovebankLogin"),
          definition = function(study,animalName, login, ...){
#print("1")
#browser()            
            studyNUM <- getMovebankID(study, login)
            getMovebankData(study=studyNUM, animalName=animalName, login=login, moveObject=moveObject,...)
          })
          
setMethod(f="getMovebankData", 
            signature=c(study="numeric",animalName="ANY", login="MovebankLogin"),
            definition = function(study, animalName, login, moveObject=T, ...){
            data <- getMovebankAnimals(study=study, login)
            attribs <- paste(collapse="%2C",getMovebankSensorsAttributes(study, login)$short_name)
            if (is.na(animalName)) {getMovebankData(study=study, login=login, data=data, attributes=attribs, ...)}
            else {getMovebankData(study=study,animalName=animalName,login=login, data=data, attributes=attribs, ...)}
            })

###create a Move or download data from a single animal within the study
setMethod(f="getMovebankData", 
          signature=c(study="numeric",animalName="character", login="MovebankLogin"),
          definition = function(study, animalName, login, moveObject=T, ...){            
            data <- getMovebankAnimals(study=study, login=login)
            name <- data[data$animalName==animalName,]
            trackDF <- getMovebank("event", login, study_id=study, sensor_sensor_type_id=name$sensor_type_id, individual_id=name$animalID)
            
            IDData <- getMovebank("individual", login=login, study_id=study)
            idData <- data.frame(IDData[IDData$local_identifier==animalName,],name) ##id.1 is m.E. = deployment ID!!
            #rownames(idData)  <- idData$local_identifier
            #idData <- idData[ ,names(idData)!="animalName"]
#             df <- merge.data.frame(x=trackDF, y=idData, by.x="individual_id", by.y="animalID", all=TRUE)
#             studyDF <- getMovebankStudy(study, login)
#             df$study.name <- rep(as.character(studyDF$name),times=nrow(trackDF))
#             df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC")
#             names(df) <- gsub('_', '.', names(df))
#             names(df) <- gsub('local.identifier','individual.local.identifier',names(df))
#             
#             .move(x=list(df=df, proj=CRS("+proj=longlat")))
            .getMovebankData(trackDF=trackDF, idData=idData, study=study, login=login, ...)
            })
            
###create a MoveStack or download data from all animals within the study
setMethod(f="getMovebankData", 
          signature=c(study="numeric",animalName="missing", login="MovebankLogin"),
          definition = function(study, animalName, login, moveObject=T, ...){ 
                 idData <- getMovebank("individual", login=login, study_id=study)
                 trackDF <- getMovebank("event", login=login, study_id=study)#, attributes="deployment_id")[1,]
#browser()              
               .getMovebankData(trackDF=trackDF, idData=idData, study=study, login=login, ...)
                })


setGeneric(".getMovebankData", function(trackDF, idData, login, study, ...) standardGeneric(".getMovebankData"))
setMethod(f=".getMovebankData", 
          signature=c("data.frame"),
          definition = function(trackDF, idData, login, study,...){
#print("3")            
browser()
#3615655
            deploymentID <- getMovebank("deployment", login=login, study_id=study, attributes="individual_id%2Ctag_id%2Cid")
                 #if (any(apply(deploymentID, 2, FUN=duplicated))){##test whether animals
                   
                   ##if there are multiple deployments: idData$local_identifier  <-  paste(localID_deploymentID)
                #   df <- merge.data.frame(x=trackDF, y=idData, by.x="individual_id", by.y="id", all=TRUE)
                # } else{
                   df <- merge.data.frame(x=trackDF, y=idData, by.x="individual_id", by.y="id", all=TRUE)
                # }
                 studyDF <- getMovebankStudy(study, login)
                 df$study.name <- rep(as.character(studyDF$name),times=nrow(trackDF))
                 df$timestamp <- as.POSIXct(strptime(as.character(df$timestamp), format = "%Y-%m-%d %H:%M:%OS",tz="UTC"), tz="UTC")
                 names(df) <- gsub('_', '.', names(df))
                 names(df) <- gsub('local.identifier','individual.local.identifier',names(df))
                 df$study.name <- gsub(' +', " ", df$study.name)
            
            .move(df=df, proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
          })
