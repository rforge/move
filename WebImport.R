require(move)
require(RCurl)

 

#a<-getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=study", curl=curl)
#a<-getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=individual&study_id=123413", curl=curl)

#data[data$name=="BCI Ocelot",c("id","name")]

#Lade alle Sensortypen (nötig, um später die Namen der Sensortypen aufzulösen, um sie an der Gui anzuzeigen):

#Liste aller Studien:   #a<-getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)

#Weitere Attribute der Studie:
#a<-getURL("https://www.movebank.org/movebank/service/direct-read?id=2235635&entity_type=study", curl=curl)

#Sensoren in der Studie (dient dazu, eine Liste der Sensortypen zu bekommen):
#  a<-getURL("https://www.movebank.org/movebank/service/direct-read?tag_study_id=2235635&entity_type=sensor", curl=curl)

Verfügbare Attribute pro Sensortyp:
a<-getURL("https://www.movebank.org/movebank/service/direct-read?sensor_type_id=653&study_id=2235635&entity_type=study_attribute", curl=curl) #a<-getURL("https://www.movebank.org/movebank/service/direct-read?sensor_type_id=82798&study_id=2235635&entity_type=study_attribute", curl=curl)

Liste von Individuen in der Studie:
  web<-getURL("https://www.movebank.org/movebank/service/direct-read?study_id=2235635&attributes=id%2Clocal_identifier&entity_type=individual", curl=curl)

#Daten für ein Individuum und einen Sensortyp:
#a<-getURL("https://www.movebank.org/movebank/service/direct-read?study_id=2235635&attributes=location_lat%2Clocation_long%2Ctimestamp&sensor_sensor_type_id=653&entity_type=event&individual_id=1456745", curl=curl)

setGeneric("movebankLogin", function(user, password,...) standardGeneric("movebankLogin"))
setMethod(f="movebankLogin", 
          signature=c(user="character", password="character"), 
          definition = function(user, password){
            curl  <- getCurlHandle()
            curlSetOpt( .opts = list(httpheader = c(user = user, password = password),verbose=T), curl=curl)
#            testdownload<-getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=study", curl=curl)
#            Sys.sleep(1)
#            if(getCurlInfo(curl)$response.code==403) {stop("Your username or password is not correct") } else {"You are now logged in!"}
            return(curl)
          })
curl <- movebankLogin(user="marcosmolla",password="12Siedler")

## Browsing Movebank data base

#names of the studies
searchMovebankStudies  <- function(x, sensor=FALSE){
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)
  data <- read.csv(textConnection(web))
  Sys.sleep(1)
  #options(warn=-1)
  if (sensor==FALSE){
    res <- as.data.frame(data$name[grepl(x,data$name,useBytes=TRUE)])
    names(res) <- paste("Results for \"",x,"\"")}
  else {
    
  }
  print("***************************************")
  if(nrow(res)>0){res}else{"No study matches your search criteria"}
  #options(warn=0)
}

getMovebankStudies  <- function(){
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)
  data <- read.csv(textConnection(web))
  return(data$name)
}

#names of the sensors
getMovebankSensors  <- function(){
 web <- getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=tag_type", curl=curl)
 data <- read.csv(textConnection(web))
 Sys.sleep(1)
# if (x=="gps" ||  x=="bird-ring" ||  x=="radio-transmitter" ||  x=="argos-doppler-shift" ||  x=="natural-mark" ||  x=="acceleration" ||  #x=="solar-geolocator"){return(data[data$external_id==x, "id"])}
# else {
   return(data)#}
}

#all or a certain ID
getMovebankID <- function(x){
  if (class(x)!="character") stop("Enter a character string with the full study name.")
  web <- getURL("https://www.movebank.org/movebank/service/direct-read?sort=name&attributes=id%2Cname%2Ci_am_owner%2Ci_can_see_data%2Cthere_are_data_which_i_cannot_see&entity_type=study", curl=curl)
  data <- read.csv(textConnection(web))
  if (x=="all") {return(data[ ,c("id","name")])}
  else {return(data[data$name==x,c("id","name")])}
  #if data is empty prompt, that study was not found
}

study <- "Zebras of Laikipia-Samburu, Kenya"
#retrieving information of a certain study
getMovebankStudy  <- function(study, animal=FALSE){
              if (class(study)=="character"){
                studyNUM <- as.numeric(strsplit(capture.output(getMovebankID(study)),split=" ")[[2]][[2]])}
              else {
                  if (class(study)=="numeric"){
                  studyNUM <- study}
                else{ stop("Use a numerical study ID, or a character string for the study name.")}}
                  
              web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?id=",studyNUM,"&entity_type=study",sep=""), curl=curl)
              data <- read.csv(textConnection(web))
#               if (class(sensor)=="logical" && sensor==TRUE) {
#                 web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?tag_study_id=",studyNUM,"&entity_type=sensor",sep=""), curl=curl)
#                 sensors <- read.csv(textConnection(web))
#               } else{stop("Use logical TRUE or FALSE for sensor")}
              if (class(animal)=="logical" && animal==TRUE) {
                web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?tag_study_id=",studyNUM,"&entity_type=sensor",sep=""), curl=curl)
                animals <- read.csv(textConnection(web))
              } else{stop("Use logical TRUE or FALSE for animal")}
              cat("\n\n**** NAME OF THE REQUESTED STUDY: ",levels(data$name)," ****\n\n")
                str(data)
#                 if (sensor==TRUE) cat("\n\n**** LIST OF THE STUDY SENSORS ****\n\n")
#                 sensors
                if (animal==TRUE) cat("\n\n**** LIST OF THE STUDY ANIMALS ****\n\n")
                animals
              }
getMovebankStudy("Zebras of Laikipia-Samburu, Kenya",animal=T)

a<-getURL("https://www.movebank.org/movebank/service/direct-read?study_id=2235635&attributes=id%2Clocal_identifier&entity_type=individual", curl=curl)
web <- getURL("https://www.movebank.org/movebank/service/direct-read?tag_study_id=2235635&entity_type=sensor", curl=curl)
data <- read.csv(textConnection(a))
data2 <- read.csv(textConnection(web))
data
data2

study <- 2235635
sensor <- 653
animal <- 1456745
#retrieving data from a certain individual of a study
getMovebankAnimal <- function(study,animal){
  ##get sensor type
    
  ##get animal data
  web <- getURL(paste("https://www.movebank.org/movebank/service/direct-read?study_id=",study ,"&attributes=location_lat%2Clocation_long%2Ctimestamp&sensor_sensor_type_id=",sensor,"&entity_type=event&individual_id=",animal,sep=""), curl=curl)
  data <- read.csv(textConnection(web))
  data[1:5,]
}