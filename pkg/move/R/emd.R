### Using fast Earth Movers Distance with rasters and DBBMM objects
if (!isGeneric("emd")) {
  setGeneric("emd", function(x, y, threshold, integer, greatcircle) standardGeneric("emd"))
  }

##DBBMM for emd
setMethod(f="emd", 
          signature=c(x="DBBMM", y="DBBMM", threshold="numeric", integer="logical", greatcircle="logical"), 
          definition = function(x,y,threshold=NA,integer,greatcircle=FALSE){
            emd(x=raster(x), y=raster(y), threshold=threshold, integer=integer, greatcircle=greatcircle)
          }
          )


#if threshold is set -> fast calculation 
setMethod(f="emd", 
          signature=c(x="RasterLayer", y="RasterLayer", threshold="numeric", integer="logical", greatcircle="logical"), 
          definition = function(x,y,threshold=NA,integer,greatcircle=FALSE){
     # browser()
            r1 <- as.data.frame(rasterToPoints(x))
            r2 <- as.data.frame(rasterToPoints(y))
            if(sum(r1$layer)!=sum(r2$layer))
              warning(paste("Bart: Rasters dont have equal mass, delta:",sum(r1$layer)-sum(r2$layer)))
            #if(sum(r1$layer)!=1)
            if(identical(all.equal(sum(r1$layer),1), FALSE))
              warning("Bart: Raster does not represent probability surface")
            
            res <- 1
            if (integer==FALSE){
              if (is.na(threshold)==TRUE){
              fun <- "emdR"                
              }
              if (is.na(threshold)==FALSE){
                fun <- "emdR_gd"
               # if(any(paste(r2$y,r2$x)!=paste(r1$y, r1$x)))
               #   stop("Rasters unequal not sure if that works")
              }else{}
              a<-.C(fun,
                    Pn=as.integer(nrow(r1)),
                    Qn=as.integer(nrow(r2)),
                    Px=as.double(r1$x),
                    Py=as.double(r1$y),
                    Pw=as.double(r1$layer),
                    Qx=as.double(r2$x),
                    Qy=as.double(r2$y),
                    Qw=as.double(r2$layer),
                    res=as.double(res),
                    th=as.double(threshold),
                    greatcircle=as.integer(greatcircle))
            }
            if (integer==TRUE){
              if (is.na(threshold)==TRUE){
                fun <- "emdRint"                
              }
              if (is.na(threshold)==FALSE){
                fun <- "emdR_gdint"
                if(any(paste(r2$y,r2$x)!=paste(r1$y, r1$x)))
                  stop("Rasters unequal not sure if that works")
              }else{}
              a<-.C(fun,
                    Pn=as.integer(nrow(r1)),
                    Qn=as.integer(nrow(r2)),
                    Px=as.double(r1$x),
                    Py=as.double(r1$y),
                    Pw=as.integer(r1$layer),
                    Qx=as.double(r2$x),
                    Qy=as.double(r2$y),
                    Qw=as.integer(r2$layer),
                    res=as.double(res),
                    th=as.integer(threshold),
                    greatcircle=as.integer(greatcircle))
            }
            return(a$res)
          }
          )
