test.brownian.bridge.dyn<-function(){
  data <- move(system.file("extdata","leroy.csv.gz",package="move"))
  ud<-dynBGB(spTransform(data[1:60,], center=T), windowSize=31, margin=13, locErr=4, raster=45, maxInt=12342342, ext=3)
}
