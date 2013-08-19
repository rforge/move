test.brownian.bridge.dyn<-function(){
  data <- move(system.file("extdata","leroy.csv.gz",package="move"))
  checkException(brownian.bridge.dyn(object=leroy, location.error=23.5, dimSize=150, ext=.3, time.step=600))
  
  r <- raster(nrows=100, ncols=100, xmn=0, xmx=10)
  checkException(brownian.bridge.dyn(spTransform(data, center=T), raster=r, location.error=20))# equal projection
  checkException(brownian.bridge.dyn(data, raster=r, location.error='2345'))# character loc
  checkException(brownian.bridge.dyn(data, raster=r, location.error=1:5))# multiple loc error not same length
}
