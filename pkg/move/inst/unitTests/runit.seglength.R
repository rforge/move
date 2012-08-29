test.seglength<-function(){
	a<-move(x=0:2,y=c(0,1,1),time=as.POSIXct(1:3, origin='1970-1-1'),proj=CRS('+proj=longlat'))
	require(geosphere)
	d<-distMeeus(coordinates(a)[-n.locs(a),],coordinates(a)[-1,] , a=6378.137, f=1/298.257223563)
	dd<-seglength(a)
	DEACTIVATED("Need to look what we want here")
	checkEquals(d,dd)
}


