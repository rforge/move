setMethod('[', signature(x=".unUsedRecords"), function(x,i,j,...){
			 x@timestampsUnUsedRecords<- x@timestampsUnUsedRecords[i]
			 x@sensorUnUsedRecords<- x@sensorUnUsedRecords[i]
			 x@dataUnUsedRecords<- x@dataUnUsedRecords[i,j]
			 return(x)
})
setMethod('[', signature(x=".unUsedRecordsStack"), function(x,i,j,...){
  if(!missing(i)){
    x@trackIdUnUsedRecords <- droplevels(x@trackIdUnUsedRecords[i])
  }else{i<-T}
  if(missing(j))
    j<-T
  callNextMethod(x=x,i=i,j=j,...)
})
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

setMethod("[", 
          signature(x=".MoveTrackStack"),
          definition=function(x,i,j,...){ #does not work
            if(!missing(i)){
              x@trackId=droplevels(x@trackId[i])
              x@idData=x@idData[as.character(unique(x@trackId[i])),]}else{i<-T}
            if(missing(j))
              j<-T
            callNextMethod(x=x,i=i,j=j,...)
          })

setMethod("[", signature(x="dBMvarianceStack"), function(x, i, j, ...) {
  if(!missing(i)){
    x@means<- x@means[i]
    x@interest<- x@interest[i]
    x@in.windows<- x@in.windows[i]
  }else{i<-T}
  if(missing(j))
    j<-T
  callNextMethod(x=x,i=i,j=j,...)
})
setMethod("[", signature(x="dBMvariance"), function(x, i, j, ...) {
  if(!missing(i)){
    x@means<- x@means[i]
    x@interest<- x@interest[i]
    x@in.windows<- x@in.windows[i]
  }else{i<-T}
  if(missing(j))
    j<-T
  callNextMethod(x=x,i=i,j=j,...)
})
