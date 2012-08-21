setGeneric("move2ade", function(x){standardGeneric("move2ade")})
#setMethod("move2ade", signature=".MoveTrack", definition=function(x){ SpatialPointsDataFrame(coords=coordinates(x),data==data.frame(rep(rownames(x@idData), n.locs(x)))) })
setMethod("move2ade", signature=".MoveTrackSingle", definition=function(x){ SpatialPointsDataFrame(coords=coordinates(x),data=data.frame(rep(rownames(x@idData), n.locs(x)))) })


setGeneric("hrBootstrap", function(x,rep=100,plot=TRUE, unin='km', unout='m2',...){standardGeneric("hrBootstrap")})
setMethod("hrBootstrap", 
          signature=c(x=".MoveTrackSingle"),
          definition=function(x,rep,plot,...){
            if (any(grepl('adehabitatHR', installed.packages()))) require(adehabitatHR) else stop("You need to install the adehabitatHR package to proceed")
            j <- round(exp(log(10)*seq(log10(5),log10(n.locs(x)),by=0.1)))
            print(j)
            locs <- lapply(j, function(js,coords) {replicate(n=rep,SpatialPoints(coords[sample(1:n.locs(x),size=js,replace=F),]) )}, coords=coordinates(x))
            mcps <- lapply(locs, lapply, mcp.area, percent=95, plotit=F,unin=unin,unout=unout)
            quant <- lapply(lapply(mcps, unlist), quantile, probs=seq(0,1,.25))
            quantLines <- do.call("rbind", quant)
            if (plot){
              plot(quantLines[,ncol(quantLines)], ylab=paste("habitat area (",unout,")"), type="l", ylim=range(quantLines))
              apply(quantLines, MARGIN=2, lines)
              abline(h=as.numeric(mcp.area(move2ade(x), percent=95, plotit=F, unin=unin, unout=unout)),lty=2)
            }
            return(quantLines)
          })
 

