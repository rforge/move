test.brownian.bridge.dyn<-function(){
  data <- move(system.file("extdata","leroy.csv",package="move"))
  checkException(brownian.bridge.dyn(object=leroy, location.error=23.5, dimSize=150, ext=.3, time.step=600))
}
