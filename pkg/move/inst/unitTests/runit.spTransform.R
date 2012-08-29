test.spTransfrom<-function(){
	data <- move(system.file("extdata","leroy.csv",package="move"))
	data2<-spTransform(x=d<-spTransform(data,center=T), CRSobj=(proj4string(data)))
	checkEquals(data,data2)
}
