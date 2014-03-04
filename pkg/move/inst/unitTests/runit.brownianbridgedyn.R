test.brownian.bridge.dyn<-function(){
  data <- move(system.file("extdata","leroy.csv.gz",package="move"))
  checkException(brownian.bridge.dyn(object=leroy, location.error=23.5, dimSize=150, ext=.3, time.step=600))
  
  r <- raster(nrows=100, ncols=100, xmn=0, xmx=10)
  checkException(brownian.bridge.dyn(spTransform(data[1:50,], center=T), raster=r, location.error=20))# equal projection
  checkException(brownian.bridge.dyn(data, raster=r, location.error='2346'))# character loc
  checkException(brownian.bridge.dyn(data, raster=r, location.error=1:5))# multiple loc error not same length
  dataP<-spTransform(data[1:100,], center=T)
  dataPB<-move::burst(dataP, round((1:(n.locs(dataP)-1)/50)))
  checkEquals(class(udS<-brownian.bridge.dyn(dataPB, dimSize=150, location.error=23, ext=.3, time.step=4, window.size=29)),"DBBMMBurstStack", check.attributes=F)
  ud<-brownian.bridge.dyn(dataP, dimSize=150, location.error=23, ext=.3, time.step=4, window.size=29)
  checkEquals(udS@DBMvar@means, ud@DBMvar@means)
  p<-seq(0, 2*pi, length.out=49)
  tmp<-move(sin(p), cos(p), as.POSIXct(1:length(p), origin='1970-1-1'), proj='+proj=aeqd')
  t<-.05
  u<-brownian.bridge.dyn(tmp, dimSize=200, location.error=.1, time.step=t)
  us<-brownian.bridge.dyn(move::burst(tmp, round(1:length(p[-1])/30)), dimSize=200, location.error=.1, time.step=t)
  checkEquals(values(u), c(values(u)))
  p<-seq(0, 2*pi, length.out=199)
  set.seed(3245)
  tmp<-move(sin(p)+rnorm(length(p))*round(sin(p))*.03, cos(p), as.POSIXct(1:length(p), origin='1970-1-1'), proj='+proj=aeqd')

  t<-.25
  u<-brownian.bridge.dyn(tmp, dimSize=500, location.error=.01, time.step=t, ext=.2, margin=15)
  us<-brownian.bridge.dyn(move::burst(tmp, round(1:length(p[-1])/30)), dimSize=500, location.error=.01, time.step=t, ext=.2, margin=15)
#  o<-raster(us)
#  values(o)<-values(us)
  checkTrue(all(abs(values(sum(us))-values(u))< .Machine$double.eps)  )

}
