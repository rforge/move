test.brownian.bridge.dyn<-function(){
  data <- move(system.file("extdata","leroy.csv",package="move"))
  checkException(brownian.bridge.dyn(object=leroy, location.error=23.5, dimSize=150, ext=.3, time.step=600))
  
  r <- raster(nrows=100, ncols=100, xmn=0, xmx=10)
  checkException(brownian.bridge.dyn(spTransform(data), raster=r, location.error=20))
}
