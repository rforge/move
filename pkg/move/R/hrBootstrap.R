setGeneric("hrBootstrap", function(x,rep=100,plot=TRUE, unin='km', unout='m2',...){standardGeneric("hrBootstrap")})
setMethod("hrBootstrap", 
          #signature=c(x=".MoveTrackSingle"),
          signature=c(x="SpatialPoints"),
          definition=function(x,rep,plot,...){
            if (any(grepl('adehabitatHR', installed.packages()))) require(adehabitatHR) else stop("You need to install the adehabitatHR package to proceed")
            if (class(x)=="SpatialPoints") nlocs <- length(x) else nlocs <- n.locs(x)
            j <- round(exp(log(10)*seq(log10(5),log10(nlocs),by=0.1)))
            print(j)
            locs <- lapply(j, function(js,coords) {replicate(n=rep,SpatialPoints(coords[sample(1:nlocs,size=js,replace=F),]) )}, coords=coordinates(x))
            mcps <- lapply(locs, lapply, mcp.area, percent=95, plotit=F,unin=unin,unout=unout)
            quant <- lapply(lapply(mcps, unlist), quantile, probs=seq(0,1,.25))
            quantLines <- as.data.frame(do.call("rbind", quant))
            rownames(quantLines) <- j
            qll <- do.call("list", quantLines)
            a <- list(2,3,1,3,2)
            col <- list(5,2,1,2,5)
            b <- list(qll,a,col)
            if (plot){
              plot(x=j, quantLines[,ncol(quantLines)], xlab="number of coordinates for mcp", ylab=paste("habitat area (",unout,")"), type="n", ylim=range(quantLines))
              #apply(quantLines, MARGIN=2, lines, x=j)
              if (class(x)=="SpatialPoints"){
              abline(h=as.numeric(mcp.area(x, percent=95, plotit=F, unin=unin, unout=unout)),lty=7)} else {
              abline(h=as.numeric(mcp.area(move2ade(x), percent=95, plotit=F, unin=unin, unout=unout)),lty=7)}
              lapply(1:ncol(quantLines), function(i, j, b) lines(x=j, y=b[[1]][[i]],lty=b[[2]][[i]],col=b[[3]][[i]],lwd=2), j=j, b=b)
            }
            return(quantLines)
          })


setMethod("hrBootstrap", 
          signature=c(x=".MoveTrackStack"),
          definition=function(x,rep,plot,...){
            return(lapply(split(x), hrBootstrap, ...))
          })

