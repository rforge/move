
test.interpolateTime<-function(){
	data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	dataP<-spTransform(data, center=T)
	checkEquals(data, interpolateTime(data, timestamps(data), spaceMethod='gr'))
	checkEquals(data, interpolateTime(data, timestamps(data), spaceMethod='rh'))
	checkEquals(dataP, interpolateTime(dataP, timestamps(dataP), spaceMethod='eu'))
	sl<-c(levels(data@sensor),'interpolateTime')
	dataS<-new('Move', data,sensor=factor(data@sensor, levels=sl), sensorUnUsedRecords=factor(data@sensorUnUsedRecords, levels=sl))
	checkEquals(dataS[c(1,n.locs(data)),], interpolateTime(data, 30, spaceMethod='gr')[c(1,30),])
	checkEquals(dataS[c(1,n.locs(data)),], interpolateTime(data, 30, spaceMethod='rh')[c(1,30),])
	checkEquals(spTransform(dataS[c(1,n.locs(data)),], proj4string(dataP)), interpolateTime(dataP, 30, spaceMethod='eu')[c(1,30),])
	checkEquals(40, n.locs(interpolateTime(data, 40, spaceMethod='g')))
	t<-move(0:1,0:1, as.POSIXct(0:1, origin='1970-1-1'))
	tt<-move(0:1,0:1, as.POSIXct(c(0,10), origin='1970-1-1'))
	checkEqualsNumeric(coordinates(interpolateTime(t, as.POSIXct(.1,origin='1970-1-1'))), cbind(.1,.1))
	checkEquals(coordinates(interpolateTime(t, as.POSIXct(.16,origin='1970-1-1'))), coordinates(interpolateTime(tt, as.POSIXct(1.6,origin='1970-1-1'))))
	# prevent lineMidpoint from failing
	d<-data[1:40,]
	crd<-do.call(rbind,lapply(lapply(split(burst(d, 1:39)),move:::lineMidpoint), coordinates))
	crd2<-coordinates(interpolateTime(d, timestamps(d)[-40]+ (timestamps(d)[-1]-timestamps(d)[-40])/2, spaceMethod='g'))
	
	checkEquals(c(crd), c(crd2))


}
