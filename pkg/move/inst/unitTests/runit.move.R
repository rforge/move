test.move<-function()
{
	checkException(move())
	#checkException(move(x=1:10, y=1:10, time=as.POSIXct(1:10, origin="1970-1-1"))) #we now allow projection to be NA
	checkEqualsNumeric(coordinates(move(x=1:10, y=1:10, time=as.POSIXct(1:10, origin="1970-1-1"), proj=CRS("+proj=longlat"))), cbind(1:10,1:10))
}
