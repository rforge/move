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

# test2 <- move(x="~/Documents/Programming/Rmove/BCI Ocelot.csv", proj=CRS("+proj=longlat"))
# p2 <- brownian.bridge.dyn(spTransform(test2), location.error=23.5, dimSize=55, ext=0.3, time.step=600)
# emd(p2[[1]], p2[[2]],threshold=1, integer=T, greatcircle=F)
#if threshold is set -> fast calculation 
setMethod(f="emd", 
          signature=c(x="RasterLayer", y="RasterLayer", threshold="numeric", integer="logical", greatcircle="logical"), 
          definition = function(x,y,threshold=NA,integer,greatcircle=FALSE){
            r1 <- as.data.frame(rasterToPoints(x))
            r2 <- as.data.frame(rasterToPoints(y))
            if(round(sum(r1$layer))!=round(sum(r2$layer))) ##bart I round here because differences from our rasters are somewhat like delta: -5.55111512312578e-16
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

# Copyright (c) 2012  Bart Kranstauber <kranstauber@mail.orn.mpg.de>, Marco Smolla <msmolla@orn.mpg.de>  
# 
# This file is free software: you may copy, redistribute and/or modify it  
# under the terms of the GNU General Public License version 3
#   
# This file is distributed in the hope that it will be useful, but  
# WITHOUT ANY WARRANTY; without even the implied warranty of  
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
# General Public License for more details.  
#   
# You should have received a copy of the GNU General Public License  
# along with this program.  If not, see <http://www.gnu.org/licenses/>.  
#   
# This file incorporates work covered by the following copyright and  
# permission notice:  
#
#     Copyright (c) 2009-2012, Ofir Pele
#     All rights reserved.
#     
#     Redistribution and use in source and binary forms, with or without
#     modification, are permitted provided that the following conditions are
#     met: 
#     * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#     * Neither the name of the The Hebrew University of Jerusalem nor the
#     names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#     
#     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
#     IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#     PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#     EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#     PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#     SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
