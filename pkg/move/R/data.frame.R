
setAs(".MoveTrack","data.frame", function(from){
      return(data.frame(
			data.frame(from), sensor=from@sensor, timestamps=from@timestamps))
})
setAs('.MoveTrackSinge','data.frame', function(from){
      return(data.frame(as(as(from,'.MoveTrack'), 'data.frame'), from@idData))
})
setAs(".MoveTrackSingleBurst","data.frame", function(from){t<-from@burstId
      t[n.locs(from)]<-NA
      return(data.frame(as(as(from,'.MoveTrack'),'data.frame'), burstId=t))
})
setAs(".MoveTrackStack","data.frame", function(from){ 
      return(data.frame(as(as(from,".MoveTrack"),"data.frame"), trackId=from@trackId, from@idData[as.character(from@trackId),]))
})
setAs("dBMvariance","data.frame", function(from){ 
      return(data.frame(as(as(from,".MoveTrack"),"data.frame"), as(as(from,"dBMvarianceTmp"),"data.frame")))
})
setAs('dBMvarianceTmp','data.frame',function(from){data.frame(window.size=from@window.size, margin=from@margin, means=from@means, in.windows=from@in.windows, interest=from@interest)})
