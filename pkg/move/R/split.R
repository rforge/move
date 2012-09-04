###create a list of Move objects from a Move Stack (hand over additional arguments!)
setGeneric("split") ##check whether this is necessary or screws up the original method marco
setMethod(f = "split",
          signature = c(x="MoveStack", f="missing"),
          definition = function(x, f, ...){
            moveList <- list()
            for (ID in unique(x@trackId)) {
              spdf <- SpatialPointsDataFrame(coords = matrix(x@coords[x@trackId==ID,], ncol=2),
                                             data=x@data[x@trackId==ID,],
                                             proj4string=x@proj4string)
              mt <- new(Class=".MoveTrack",
                        spdf,
                        timestamps=x@timestamps[x@trackId==ID],
                        sensor=x@sensor[x@trackId==ID])
              #sensor=rep(x@idData$sensor.type[row.names(x@idData)==ID], sum(x@trackId==ID))
tmf<-x@timesMissedFixes[x@timesMissedFixes==ID]
attr(tmf, "names")<-NULL
              moveObj <- new(Class="Move", 
                             mt,
			     timesMissedFixes=tmf,
                             idData=x@idData[row.names(x@idData)==ID, ,drop=F],
                             dateCreation=x@dateCreation,
                             study=x@study,
                             citation=x@citation,
                             license=x@license)
              moveList[[ID]]  <- moveObj
            }
            return(moveList)
          }) ###when splitting a MoveStack there is a warning issued like: In min(x) : no non-missing arguments to min; returning Inf;; this is due to the data that are coerced by the show function for 'min values' and 'max values';; and this is because the original print function ahndels na.rm=T and thus there are no data anymore

setMethod(f = "split",
          signature = c(x="DBBMMStack", f="missing"),
          definition = function(x, f, ...){
            DBBMMList <- list()
            for (Id in as.character(unique(x@DBMvar@trackId))) { 
              UD <- new(Class=".UD", 
                        method=x@method,
                        x[[Id]])
              dbmv <- new(Class="dBMvariance",
                          timestamps=x@DBMvar@timestamps[x@DBMvar@trackId==Id],
                          coords.nrs=x@DBMvar@coords.nrs,
                          coords=x@DBMvar@coords[x@DBMvar@trackId==Id, ],
                          bbox=x@DBMvar@bbox,
                          data=x@DBMvar@data[x@DBMvar@trackId==Id, ],
                          proj4string=x@DBMvar@proj4string,
                          sensor=x@DBMvar@sensor[x@DBMvar@trackId==Id],
                          window.size=x@DBMvar@window.size,
                          break.list=x@DBMvar@break.list,
                          interest=as.logical(x@DBMvar@interest[x@DBMvar@trackId==Id]),
                          means=x@DBMvar@means[x@DBMvar@trackId==Id],
                          in.windows=x@DBMvar@in.windows[x@DBMvar@trackId==Id],
                          margin=x@DBMvar@margin)
              DBBMMObj <- new(Class="DBBMM",
                              UD,
                              ext=x@ext,
                              DBMvar=dbmv)
              DBBMMList[[Id]]  <- DBBMMObj
            }
            return(DBBMMList)
          })
