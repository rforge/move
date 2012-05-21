## Browsing Movebank data base
curl <- NA ##ist das sinnig?

setGeneric("movebankLogin", function(user, password,...) standardGeneric("movebankLogin"))
setMethod(f="movebankLogin", 
          signature=c(user="character", password="character"), 
          definition = function(user, password){
          require(RCurl)
            curl  <- getCurlHandle()
            curlSetOpt( .opts = list(httpheader = c(user = user, password = password),verbose=T), curl=curl)
#            testdownload<-getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=study", curl=curl)
#            Sys.sleep(1)
#            if(getCurlInfo(curl)$response.code==403) {stop("Your username or password is not correct") } else {"You are now logged in!"}
            return(curl)
          })

#names of the studies
searchMovebankStudies  <- function(x, sensor=FALSE){
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)
  data <- read.csv(textConnection(web))
  Sys.sleep(1)
  options(warn=-1)
  if (sensor==FALSE){
    res <- as.data.frame(data$name[grepl(x,data$name,useBytes=TRUE)])
    names(res) <- paste("##### Results for ",x,"#####")
    if(nrow(res)>0){return(res)}else{"No study matches your search criteria"}
    } else {}
  options(warn=0)
}

getMovebankStudies  <- function(){
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)
  data <- read.csv(textConnection(web))
  return(data$name)
}

#names of the sensors
getMovebankSensors  <- function(x=NA){
 if (is.na(x)==TRUE){
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=tag_type", curl=curl)
  data <- read.csv(textConnection(web))
  Sys.sleep(1)
  cat("##### LIST OF ALL SENSOR TYPES IN MOVEBANK #####\n")
 } else {
  if (class(x)=="character"){
    #showing attributes of the sensors within the study
    studyNUM  <- getMovebankID(x)
    a <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?tag_study_id=",studyNUM,"&entity_type=sensor",sep=""), curl=curl)
    data <- read.csv(textConnection(a))
    
    studySensors <- unique(data$sensor_type_id)
    for (i in 1:length(studySensors)){
      sens <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?sensor_type_id=",studySensors[i],"&study_id=",studyNUM,"&entity_type=study_attribute",sep=""), curl=curl)
      data2 <- read.csv(textConnection(sens))
      
 Sys.sleep(1)
 cat("##### ATTRIBUTES OF THE SENSORS IN: ",x," \n")
 print(data)
 return(data2)
    } 
  } 
 }
}

###all or a certain ID
getMovebankID <- function(x){
  if (class(x)!="character") stop("Enter a character string with the full study name.")
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)
  data <- read.csv(textConnection(web))
  
  if (x=="all") {
    cat("####### STUDY ID #######\n")
    return(data[ ,c("id","name")])
    }
  else {
    studyNUM <- as.numeric(strsplit(capture.output(data[data$name==x,c("id")]),split=" ")[[1]][[2]])
    studyNUM
    }
  #if data is empty prompt, that study was not found
}

###retrieving information of a certain study
getMovebankStudy  <- function(study, animal=FALSE){
              if (class(study)=="character"){
                studyNUM  <- getMovebankID(study)}
              else {
                  if (class(study)=="numeric"){
                  studyNUM <- study}
                else{ stop("Use a numerical study ID, or a character string of the whole the study name.")}}
              studyNUM    
              web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?id=",studyNUM,"&entity_type=study",sep=""), curl=curl)
              data <- read.csv(textConnection(web))
              
              if (class(animal)=="logical" && animal==TRUE) {
                web2 <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?tag_study_id=",studyNUM,"&entity_type=sensor",sep=""), curl=curl)
                animals <- read.csv(textConnection(web2))
              } else{animals <- NA}
              
              cat("\n\n**** SUMMARY OF THE REQUESTED STUDY: ",levels(data$name)," ****\n\n")
              print(data,)
              
              web3 <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?study_id=",studyNUM,"&attributes=id%2Clocal_identifier&entity_type=individual",sep=""), curl=curl)
              animalID <- read.csv(textConnection(web3))
              names(animalID) <- c("animalID","animalName")
              animaldf <- cbind(animalID,animals)
              cat("\n\n**** LIST OF THE STUDY ANIMALS ****\n\n")
              Sys.sleep(1)
              
              animaldf
}

###retrieving data from a certain individual of a study
getMovebankData <- function(study,animalName=NA){
  if (is.na(animalName)==TRUE) stop("Please enter a name of the animal in this study")
  data <- getMovebankStudy(study=study,animal=TRUE)
  if (class(study)!="numeric") {studyNUM <- getMovebankID(study)} else {if (class(study)=="numeric") {studyNUM <- study}}
  ddd <- strsplit(as.character(data[data$animalName==animalName,]),split="\t")
  #web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?study_id=",studyNUM ,"&sensor_sensor_type_id=",as.numeric(ddd[[4]][[1]]),"&entity_type=event&individual_id=",as.numeric(ddd[[1]][[1]]),sep=""), curl=curl)
#  web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?study_id=",studyNUM ,"&entity_type=event&individual_id=",as.numeric(ddd[[1]][[1]]),sep=""), curl=curl)
  attributes <- paste(collapse="%2C",getMovebankSensors(study)$short_name)
  web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?study_id=",studyNUM ,"&attributes=",attributes,"&sensor_sensor_type_id=",as.numeric(ddd[[4]][[1]]),"&entity_type=event&individual_id=",as.numeric(ddd[[1]][[1]]),sep=""), curl=curl)
  data <- read.csv(textConnection(web))
  cat("##### FIRST FIVE LINES OF THE ANIMAL DATA #####\n") 
  print(data[1:5,])
  return(data)
}