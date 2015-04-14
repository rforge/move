context('idData')
test_that('idData',{
	data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	expect_equal(class(idData(data)),'data.frame')
	expect_equal(idData(data), idData(data,1))
	expect_equal(idData(data), idData(data,T))
	expect_equal(class(idData(data,,'sensor.type')),'factor')
	expect_equal(class(idData(data,,'sensor.type', drop=F)),'data.frame')
	dataOld<-data
	idData(data,1)<-idData(data)
	expect_equal(data, dataOld)
})
