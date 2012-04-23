#source(file="~/Documents/Programming/Rmove/move/pkg/move/R/ClassMove.R")
#source(file="~/Documents/Programming/Rmove/move/pkg/move/R/ClassDBMvar.R")



###Defining the class of the Brownian Bridge Movement Model object
setClass(Class = "DBBMM",
         representation = representation (
           DBMvar= "DBMvar", 
           raster= "RasterLayer" #storing the raster and the probabilities as values
           )
         )

## Making dBBMM a generic funtion
#if (!isGeneric("dBBMM")) {  
  setGeneric("dBBMM", function(DBMvar, raster) standardGeneric("dBBMM"))
  #}

## Defining the funcitoin dBBMM
setMethod(f="dBBMM", 
          signature=c(DBMvar="DBMvar", raster ="RasterLayer"), 
          definition = function(DBMvar, raster){
            res <- new(Class="DBBMM")
            res@DBMvar <- DBMvar
            res@raster <- raster 
            return(res)
            }
          )
#testing:
#bbmm <- doBBMM(BMvar=c(1,2), rasterX=2,rasterY=2,probability=2,track=data.frame(c(1:2),c(1:2)),dyn=T)









#if (!isGeneric("brownian.bridge.dyn")) {
  setGeneric("brownian.bridge.dyn", function(object,raster=1,dimSize=10,location.error,margin=11, time.step=NULL, window.size=31, ext=0.25,...){standardGeneric("brownian.bridge.dyn")})
#}


###if no raster object but a dimSize is given, the cell size of the raster is calculated with the number of cells given by the dimSize
#NOTE: the dimSize is a raw estimate of number of cells of the highest range side. it is however not the final number of cells in that direction because when calculating the raster it is extended by the ext factor and there is rounding with ceiling also taking part. 
setMethod(f="brownian.bridge.dyn", 
          signature=c(object="Move",raster="missing", dimSize="numeric",location.error="numeric"),
          function(object, raster, dimSize, location.error, ...){
            y <- coordinates(object)[ ,2]
            x <- coordinates(object)[ ,1]
            
            #either one or two factors are set for extension ...
            if(length(ext)==1) {ext <- rep(ext, 2)}
            if(length(ext)==2){
              yRange <- extendrange(y, f=ext[2])
              xRange <- extendrange(x, f=ext[1])
              }          
            
            #... or four! (here the order is: xmin,xmax,ymin,ymax) 
            if(length(ext)==4){
              yRange <- c(range(y)[1]-abs(range(y)[1]*ext[3]), range(y)[2]+abs(range(y)[2]*ext[4]))
              xRange <- c(range(x)[1]-abs(range(x)[1]*ext[1]), range(x)[2]+abs(range(x)[2]*ext[2]))
            }
            
            #largest dimension divided by number of cells (=dimSize) gives cell.size (raster="numeric")
            if (diff(xRange) > diff(yRange)){
              raster <- diff(xRange)/dimSize ##### why times two???
            } else{
              raster <- diff(yRange)/dimSize
            }

#             #largest dimension divided by number of cells (=dimSize) gives cell.size (raster="numeric")
#             if (diff((range(coordinates(object)[ ,1]))) > diff((range(coordinates(object)[ ,2])))){
#               raster <- diff((range(coordinates(object)[ ,1])))/dimSize
#             } else{
#               raster <- diff((range(coordinates(object)[ ,2])))/dimSize
#             }
            
            print("missing::numeric")
            cat("dimSize: ", dimSize, "\n")
            cat("ext: ", ext)
            return(brownian.bridge.dyn(object=object, raster=raster, location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, ext=ext,...))
          })


###if neither a raster nor the dimSize is given, then the sell size is calculated by the defauled dimSize and the largest dimension
setMethod(f="brownian.bridge.dyn", 
          signature=c(object="Move",raster="missing", dimSize="missing",location.error="numeric"),
          function(object, raster, dimSize, location.error,...){
                   cat("Using default dimSize: ", dimSize, "\n")
            return(brownian.bridge.dyn(object=object, dimSize=dimSize, location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, var=var,ext=ext,...))
          })


#if there is no valid raster object, it should be calculated     
#make a raster object and feed it (again) to the brownian.bridge.dyn function (now it will call the right function, because raster is now a raster object)   
setMethod(f = "brownian.bridge.dyn",
          signature = c(object="Move",raster="numeric",dimSize="missing",location.error="numeric"),
          definition = function(object,raster,dimSize,location.error,...){
            y <- coordinates(object)[ ,2]
            x <- coordinates(object)[ ,1]
           
            #either one or two factors are set for extension ...
            if(length(ext)==1) {ext <- rep(ext, 2)}
            if(length(ext)==2){
              yRange <- extendrange(y, f=ext[2])
              xRange <- extendrange(x, f=ext[1])
            }          
            
            #... or four! (here the order is: xmin,xmax,ymin,ymax)
            if(length(ext)==4){ 
              yRange <- c(range(y)[1]-abs(range(y)[1]*ext[3]), range(y)[2]+abs(range(y)[2]*ext[4]))
              xRange <- c(range(x)[1]-abs(range(x)[1]*ext[1]), range(x)[2]+abs(range(x)[2]*ext[2]))
            }
            
            ymin <- yRange[1] - (ceiling(diff(yRange)/raster) * raster - diff(yRange))/2
            ymax <- yRange[2] + (ceiling(diff(yRange)/raster) * raster - diff(yRange))/2
            xmin <- xRange[1] - (ceiling(diff(xRange)/raster) * raster - diff(xRange))/2
            xmax <- xRange[2] + (ceiling(diff(xRange)/raster) * raster - diff(xRange))/2
            
            #Calculate the raster #the raster variable replaces here the cell size
            nrow <- ((ymax-ymin)/raster) 
            ncol <- ((xmax-xmin)/raster)
            ex <- extent(c(xmin,xmax,ymin,ymax))
            rst <- raster(ncols=ncol,nrows=nrow, crs=proj4string(object), ex) #object@sdf@proj4string@projargs, ex)# maybe use projection(object)
                            
            cat("raster from brownian.bridge.dyn pre loop")
            print(rst)
            cat("extention factor is: ", ext, "\n")
            return(brownian.bridge.dyn(object=object, raster=rst, location.error=location.error, margin=margin, time.step=time.step, window.size=window.size, var=var,...))
          }
)


#dyn.load("BBMM.so")

# Output from brownian.bridge.dyn: 
#   UD = list with estimated Brownian.Motion.Variance, X, Y, and Z, 
#       where X and Y are grid center point coordinates and Z is the estimated
#       probability of use with sum(Z) = 1.0.
setMethod(f = "brownian.bridge.dyn",
          signature = c(object="Move", raster="RasterLayer",dimSize="missing", location.error="numeric"),
          definition = function(object, raster, location.error, ...){
            
            print("brownian.bridge.dyn 1")
            #cat("extent:: ", ext)
            x <- coordinates(object)[ ,1] #extracts the xy coordinates from the Move
            y <- coordinates(object)[ ,2]
            time.lag <- time.lag(object)
            n.locs <- n.locs(object) 
            if(length(location.error) == 1){
              location.error <- rep(x = location.error, times = n.locs)
              } else{}
            
            
#             #If a variance is not supplied brownian.motion.variance.dyn is called, which calculates the motion vriance
#             #if a variance is supplied (see else) the single variance value is applied to the object
            #if(is.null(var)){
              DBMvar <- brownian.motion.variance.dyn(object=object, location.error=location.error, margin=margin, window.size=window.size)##<<<<<<<<<<<<<<<##Calling function##<<<<<<<<<<<<<<<<<<<<##
              DBMvar.vec <- DBMvar@means
              
              
        #    }else{
         #     DBMvar.vec <- rep(var, length(x))
          #    BMvar <- data.frame(interest=c(rep(TRUE, length(x)-1), FALSE), in.windows=rep(1, length(x)))
           # }
            print("brownian.bridge.dyn 2")
            
            # Use 10 units (generally minutes) as default
            if(is.null(time.step)){ 
              time.step <- (min(time.lag[-length(time.lag)])/15)
            }
            
            grid.size <- ncell(raster)  #########BEFORE CHANGING: grid.size <- nrow(area.grid)
            probability <- rep(0, grid.size)
            T.Total <- sum(time.lag[DBMvar@interest])
            
            dyn.load("pkg/move/src/BBMM.so")
            interest <- (c(DBMvar@interest, 0)+c(0, DBMvar@interest))[1:length(DBMvar@interest)]!=0
            compsize <- grid.size*(sum(time.lag[DBMvar@interest])/time.step)
            print(paste("Computational size:", sprintf("%.1e", compsize)))
            if (compsize>500000000){
              cat("\n The calculation may take longer than 5 minutes. \n")
              cat("If you don't want to proceed, abort the funciton now! \n")
              ## Waiting 10s
              pb <- txtProgressBar(min = 0, max = 10, style = 1)
              for(i in 1:10){
                Sys.sleep(1)
                setTxtProgressBar(pb, i)
              }
              close(pb)
            } else {}            
            
            #asking to continue, if ncell is to large here is the possibility to end the process
            ##############readline(prompt = "Pause. Press <Enter> to continue or <Esc> to abort.")
                        
            ans <- .Fortran("DBBMM",
                            as.integer(1+sum(DBMvar@interest)), 
                            as.integer(grid.size), 
                            as.double(c(time.lag[DBMvar@interest],0)), 
                            as.double(T.Total), 
                            as.double(x[interest]), 
                            as.double(y[interest]), 
                            as.double(c(DBMvar.vec[DBMvar@in.windows==max(DBMvar@in.windows)],0)), 
                            as.double(location.error[interest]), 
                            as.double(coordinates(raster)[ ,1]), #rasterX 
                            as.double(coordinates(raster)[ ,2]), #rasterY
                            as.double(time.step), 
                            as.double(probability))
            
            print("done with FORTRAN")
            cat("Probability: ", head(ans[[12]]), "\n")
            raster <- setValues(raster, ans[[12]])
            #print(raster)
            
            outerProbability <- outerProbability(raster=raster)
            
            if (outerProbability > .05){
              cat("outer probability: ", outerProbability, "\n")
              warning("Error: the used extent is to small. Choose a larger extent to get a bigger raster.")
            }

            DBBMM <- dBBMM(DBMvar=DBMvar, raster=raster 
                         )
          
            print("DBBMM successfully created")
            return(DBBMM)
          }
)
          
##############################
###Functions to print things:
##############################
# print.dbbmm <- function(x){
#   cat(paste(c("P-range Min:", "Max:"),sprintf("%07f", range(x$probability)), collapse=" "), fill=TRUE)
#   cat("Size of Grid:", length(x$x), "Cells", fill=TRUE)
#   cat( "Grid Cell Size:", max(diff(sort(x$x))), fill=TRUE)
#   cat("Grid Edge Sum:", sum(x$probability[x$x  %in% range(x$x) | x$y %in% range(x$y)]), fill=TRUE)
#   cat(paste(c("Grid length X:", "Y:"), c(length(unique(x$x)), length(unique(x$y)))), fill=TRUE)
# }
# 
# 
# print.dBMvar <- function(x)
# {
#   cat(paste(c("BMvar range min:", "max:"),sprintf("%03f", range(x$means)), colapse=" "), fill=TRUE)
#   cat(paste("Number of unique break locations:", length(unique(x$break.list))), fill=TRUE)
# }


##############################          
###Functions that plot things:
##############################

###validity check for double time stamps
#calculate the amount of probabilities on the edge of the raster

#if (!isGeneric("outerProbability")){
setGeneric("outerProbability", function(raster, border=.1){standardGeneric("outerProbability")})
#}

setMethod(f = "outerProbability",
          signature = c(raster="RasterLayer"),
          definition = function(raster, border){
            rowRange <- ceiling(nrow(raster)*border)
            colRange <- ceiling(ncol(raster)*border)
            innerProbability <- sum(getValuesBlock(x=raster,row=rowRange, nrows=nrow(raster)-rowRange, col=colRange,ncols=ncol(raster)-colRange))
            outerProbability <- cellStats(raster,stat=sum)-innerProbability          
            return(outerProbability/cellStats(raster,stat=sum))
          }  
          )


###extract  raster from DBBMM
setMethod(f="raster",
          signature = "DBBMM",
          definition = function(x){
            return(x@raster)
          }
          )


###extract projection from DBBMM
setMethod(f = proj4string,
          signature = "DBBMM", 
          definition = function(obj){
            return(raster(obj)@crs@projargs)
          }
          )

####################
## Plotting dbbmm ##
####################
setMethod(f = plot,
          signature = "DBBMM",
          definition = function(x){ #maybe some more variables for the desgin
            plot(raster(x))
          }
          ) 

setMethod(f = image,
          signature = "DBBMM",
          definition = function(x,...){ #maybe some more variables for the desgin
            image(raster(x,...))
          }
          )

setMethod(f = contour,
          signature = c(x="DBBMM"),
          definition = function(x, levels, add=F, ...){
            newRaster <- raster(x)
            
            rank <- (1:length(values(newRaster)))[rank(values(newRaster))]
            values(newRaster)<-cumsum(sort(values(newRaster)))[rank]
            
            contour(x = newRaster, levels = levels, add = add, ...)
          }
          )


