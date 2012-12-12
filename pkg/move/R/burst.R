setGeneric("burst", function(x, f, ...) {standardGeneric("burst")})
setMethod("burst", 
          signature=c(x = "Move", f = "factor"), 
          definition = function(x, f, ...) {
            levels(f)<-raster:::.goodNames(levels(f))
          new("MoveBurst", 
              as(x, ".MoveTrackSingle"), 
              as(x, ".MoveGeneral"), 
              burstId = f, 
              idData = x@idData)
          })

setMethod("burst", 
          signature=c(x = "Move", f = "numeric"), 
          definition = function(x, f, ...) {
            burst(x = x, f = as.factor(f))
          })

setMethod("burst", 
          signature=c(x = "Move", f = "character"), 
          definition = function(x, f, ...) {
            if (length(f) == 1) {
                burst(x = x, f = do.call("$", list(x, f))[-n.locs(x)])
            } else {
                burst(x = x, f = as.factor(f))
            }
          })

setMethod("[", 
          signature(x = ".MoveTrackSingleBurst"), 
          definition=function(x, i, j, ...) {
            if (!missing(i)) {
              tmp<- x@burstId[i]
              x@burstId <-tmp[-length(tmp)]
            } else {
              i <- T
            }
            if (missing(j)) 
              j <- T
            if (class(i) == "character") 
              stop("Not sure if these methods work for class character")
            if (class(j) == "character") 
              stop("Not sure if these methods work for class character")
            callNextMethod(x = x, i = i, j = j, ...)
          })

setAs("MoveBurst", "Move", function(from) {
# last id can be different since that one is not telling anything about a segment in this obj
      if (length(unique(from@burstId)) != 1) 
        stop("Does not work with one burst id only")
      new("Move", 
          as(from,".MoveGeneral"), 
          as(from,".MoveTrackSingle"))
    }) 
