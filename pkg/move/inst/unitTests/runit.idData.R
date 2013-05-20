
test.idData<-function(){
	data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	checkEquals(class(idData(data)),'data.frame')
	checkEquals(idData(data), idData(data,1))
	checkEquals(idData(data), idData(data,T))
	dataOld<-data
	idData(data,1)<-idData(data)
	checkEquals(data, dataOld)


}
