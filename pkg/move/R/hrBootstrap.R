require(move)

##THE AREA IS STILL CALCULATED AS 0!!!


setGeneric("move2ade", function(x){standardGeneric("move2ade")})
setMethod("move2ade", signature=".MoveTrack", definition=function(x){ SpatialPointsDataFrame(coords=coordinates(x),data==data.frame(rep(rownames(x@idData), n.locs(x)))) })
setGeneric("move2ade", function(x){standardGeneric("move2ade")})
setMethod("move2ade", signature=".MoveTrackSingle", definition=function(x){ SpatialPointsDataFrame(coords=coordinates(x),data=data.frame(rep(rownames(x@idData), n.locs(x)))) })

 
x <- move(x="~/Documents/Programming/Rmove/BCI_Ocelot_Bobby.csv")
y <- move(x="~/Documents/Programming/Rmove/BCI_Ocelot_Isaac.csv")

setGeneric("hrBootstrap", function(x,rep=100,plot=T, unin='km', unout='m2',...){standardGeneric("hrBootstrap")})
setMethod("hrBootstrap", 
          signature=c(x=".MoveTrackSingle"),
          definition=function(x,rep,plot,...){
            if (any(grepl('adehabitatHR', installed.packages()))) require(adehabitatHR) else stop("You need to install the adehabitatHR package to proceed")
            #if (any(grepl('maptools', installed.packages()))) require(maptools) else stop("You need to install the maptools package to proceed") #mcp
            j <- round(exp(log(10)*seq(log10(5),log10(n.locs(x)),by=0.1)))
            print(j)
            locs <- lapply(j, function(js,coords) {replicate(n=rep,SpatialPoints(coords[sample(1:n.locs(x),size=js,replace=F),]) )}, coords=coordinates(x))
            mcps <- lapply(locs, lapply, mcp.area, percent=95, plotit=F,unin=unin,unout=unout)
            quant <- lapply(lapply(mcps, unlist), quantile, probs=seq(0,1,.25))
            quantLines <- do.call("rbind", quant)
            if (plot){
              plot(quantLines[,ncol(quantLines)], type="l", ylim=range(quantLines))
              apply(quantLines, MARGIN=2, lines)
              abline(v=mcp.area(move2ade(x), percent=.95, plotit==F, unin=unin, unout=unout),)
            }
            return(quantLines)
          })
hrBootstrap(x,rep=25)
#m <- move(x=rnorm(7000), y=rnorm(7000), time=as.POSIXct(1:7000, origin="1970-1-1"), proj=CRS("+proj=aeqd"))            
            
            
            #vars <- lapply(lapply(mcps, unlist), var)
            #repeat this rep times 
            #j <- c(5,6) #increase number of coordinates exponential
            #i <- lapply(j, function(js) {replicate(n=4,as.list(sample(1:n.locs(x),size=js,replace=F)))}) ##n=rep!!!!!!
            #locs <- lapply(i, function(z,coords) (coords[as.numeric(z), ]), coords=coordinates(x))
            #locs <- coordinates(x)[i,]
            #calculate mcp
            #mcps <- unlist(lapply(unlist(locs), mcp.area, percent=95,unin='km',unout='m2'))
            #areas <- apply(data.frame(c(1:length(j))-1), MARGIN=1, function(mcps, t) {colMeans(data.frame(mcps[(1+t*rep):(4+t*rep)] ))},mcps=mcps)
            #vars <- apply(data.frame(c(1:length(j))-1), MARGIN=1, function(mcps, t) {var(data.frame(mcps[(1+t*rep):(4+t*rep)] ))},mcps=mcps)
            #areas <- lapply(mcps, unlist)
            #mcpx <- mcp(SpatialPoints(coords=data.frame(locs[[1]])), percent=95,unin='km',unout='m2') ##check projection!!
            #caluclate area
            ##calculate for all calculation steps the variance            
            #plot area size ± variance

# xp <- mcp(move2ade(x), percent=95,unin='km',unout='m2') ##check projection!!
# xp
# plot(xp, col=24)#, xlim=c(min(coordinates(x)[,1]), max(coordinates(x)[,1])), xlim=c(min(coordinates(x)[,2]), max(coordinates(x)[,2])))
# lines(x, col=4, lwd=2)
# 
# yp <- mcp(move2ade(y), percent=95,unin='km',unout='m2') ##check projection!!
# yp
# plot(yp, col=24)#, xlim=c(min(coordinates(x)[,1]), max(coordinates(x)[,1])), xlim=c(min(coordinates(x)[,2]), max(coordinates(x)[,2])))
# lines(y, col=4, lwd=2)
# 
# ms <- moveStack(list(x,y))
# msxy <- SpatialPointsDataFrame(coords=coordinates(ms),data=data.frame(ms@trackId))
# 
# par(mfrow=c(2,1))
# hrs <- mcp.area(msxy, percent=seq(50, 100, by = 5),unin='km',unout='m2')

