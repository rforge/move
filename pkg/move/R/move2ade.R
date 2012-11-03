setGeneric("move2ade", function(x){standardGeneric("move2ade")})
setMethod("move2ade", 
          signature=".MoveTrackSingle", 
          definition=function(x){ 
            SpatialPointsDataFrame(coords=coordinates(x),
                                   data=data.frame(rep(rownames(x@idData), n.locs(x)))) 
            })

setMethod("move2ade", 
          signature=".MoveTrack", 
          definition=function(x){ 
            SpatialPointsDataFrame(coords=coordinates(x), 
                                   data=data.frame(rep(rownames(x@idData), apply(data.frame(rownames(x@idData)), MARGIN=1, function(z) sum(z==as.character(x@trackId)))) )) 
            })

