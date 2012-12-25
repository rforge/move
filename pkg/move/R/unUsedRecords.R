setGeneric('unUsedRecords<-', function(obj, value){standardGeneric('unUsedRecords<-')})
setMethod('unUsedRecords<-', c(obj='.MoveTrackSingle', value='logical'), function(obj, value){
	  if(n.locs(obj)!=length(value))
		  stop('Selection length does not match with number of locations')
	  unUsed<-as(obj, '.unUsedRecords')
	  xNew<-obj[!value,]
	  xOld<-obj[value,]
	  unUsedNew<-new('.unUsedRecords', 
		      timestampsUnUsedRecords=ifelse(is.null(unUsed@timestampsUnUsedRecords), list(xOld@timestamps),list(c(unUsed@timestampsUnUsedRecords, xOld@timestamps)))[[1]],   
		      sensorUnUsedRecords=factor(c(as.character(unUsed@sensorUnUsedRecords), as.character(xOld@sensor))),
		      dataUnUsedRecords=rbind(unUsed@dataUnUsedRecords, xOld@data)
		      ) 
	  new(class(obj), xNew, unUsedNew)
})
setMethod('unUsedRecords<-', c(obj='.MoveTrackStack', value='logical'), function(obj, value){
	  if(sum(n.locs(obj))!=length(value))
		  stop('Selection length does not match with number of locations')
	  unUsed<-as(obj, '.unUsedRecordsStack')
	  xNew<-obj[!value,]
	  xOld<-obj[value,]
	  unUsedNew<-new('.unUsedRecordsStack', 
		      timestampsUnUsedRecords=ifelse(is.null(unUsed@timestampsUnUsedRecords), list(xOld@timestamps),list(c(unUsed@timestampsUnUsedRecords, xOld@timestamps)))[[1]],   
		      sensorUnUsedRecords=factor(c(as.character(unUsed@sensorUnUsedRecords), as.character(xOld@sensor))),
		      trackIdUnUsedRecords=factor(c(as.character(unUsed@trackIdUnUsedRecords), as.character(xOld@trackId))),
		      dataUnUsedRecords=rbind(unUsed@dataUnUsedRecords, xOld@data)
		      ) 
	  new(class(obj), xNew, unUsedNew)
})


