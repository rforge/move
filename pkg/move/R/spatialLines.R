
setAs("Move", "SpatialLines", function(from){
      SpatialLines(list(Lines(list(Line(data)),rownames(data@idData))),proj4string=CRS(proj4string(data)))
})
