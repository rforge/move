setGeneric('idData', function(x,i,j,...){standardGeneric('idData')})
setGeneric('idData<-', function(x,i,j,value){standardGeneric('idData<-')})
setMethod('idData',signature=c('.MoveTrack'),function(x, i,j,...){
	  return(x@idData[i,j,...])
})

setMethod('idData<-',signature=c('.MoveTrack'),function(x, i,j,value){
	  x@idData[i,j]<-value
	  return(x)
})
