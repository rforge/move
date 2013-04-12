setAs("Move", "SpatialLinesDataFrame", function(from){
      SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(from)),rownames(from@idData))),proj4string=CRS(proj4string(from))), from@idData)
})
setAs("MoveStack", "SpatialLinesDataFrame", function(from){
      SpatialLinesDataFrame(SpatialLines(mapply('Lines',lapply(lapply(split(as(from,'SpatialPoints'), from@trackId),Line), list) , rownames(from@idData)),proj4string=CRS(proj4string(from))), from@idData)
})
