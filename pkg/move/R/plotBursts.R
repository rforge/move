#  		#Find below the functions to plot a "centroid" point on the line of a certain line segment
#			##add classfication for color and size marco
setGeneric("plotBursts", function(object, add=TRUE, sizeFUN=function(x){as.numeric(diff(range(timestamps(x))), units='mins')}, col=NA, breaks=3, ...){standardGeneric("plotBursts")})
setMethod(f = "plotBursts", 
          signature = c(object="list"),
          definition = function(object, add, sizeFUN, ...){
            lapply(object, FUN=plotBursts, add=add, sizeFUN=sizeFUN, ...)
          })

setMethod(f = "plotBursts", 
          signature = c(object=".MoveTrackSingleBurst"),
          definition = function(object, add, sizeFUN, col, breaks, ...){
            totalDur <- difftime(object@timestamps[n.locs(object)], object@timestamps[1], units="mins") #duration in MIN
            splitobject <- split(object)
            midLST <- lapply(X=splitobject, FUN=lineMidpoint)
            
            if (length(col)==length(levels(object@burstId))){
              col <- col[as.numeric(factor(names(midLST)))]
            } else {
              if(length(levels(object@burstId))>8) warning("There are more burst IDs than colors.")
              col <- as.numeric(factor(names(midLST)))}
            
            #            if (class(sizeFUN)!="function") 
            #sizesdf <- as.numeric(lapply(lapply(lapply(split(object), timestamps), range), diff))/as.numeric(totalDur)
            sizesdf<-unlist(lapply(splitobject, sizeFUN))
            #} else {FUN <- match.fun(sizeFUN)
            #        sizeLST  <- FUN(ll, object)} #think about what to enter for FUN
            #sizes  <- as.numeric(cut(unlist(sizeLST), breaks=breaks))/max(as.numeric(cut(unlist(sizeLST), breaks=breaks))) 
            sizes  <- as.numeric(cut(sizesdf, breaks=breaks))/max(as.numeric(cut(sizesdf, breaks=breaks)),na.rm=T)*2
            #df <- cbind(col, sizes, do.call('rbind', lapply(midLST, data.frame)))
            df <- cbind(col, sizes, data.frame(do.call(rbind, midLST)))
            colnames(df) <- c("color", "size","x","y")
            spdf <- SpatialPointsDataFrame(coords=do.call(rbind, midLST), data=df[,1:2], proj4string=CRS("+proj=longlat"))           
            
            if (add) {
              points(x=df[1,3], y=df[1,4], cex=as.numeric(df[1,2]), col=df[1,1],...)
              res <- apply(df[-1,], MARGIN=1, function(x,...){points(x=x[3], y=x[4], cex=as.numeric(x['size']), col=x['color'],...)}, ...)
            } else {
              plot(coordinates(object), type="l", ...)
              res <- apply(df, MARGIN=1, function(x,...){points(x=x[3], y=x[4], cex=as.numeric(x['size']), col=x['color'],...)}, ...)
            }
            #return(spdf)
            })
