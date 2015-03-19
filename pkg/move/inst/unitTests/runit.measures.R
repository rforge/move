
test.measures<-function()
{
	x<-move(x=(dx<-c(1,1:7)), y=c(1,1:7), as.POSIXct(c(1:7,10), origin='1970-1-1'))
	d<-c(0,rep(sqrt(2),6))
	t<-c(rep(1, 6), 3)
	checkEquals(distance(x),d)
	checkEquals(distanceSummary(x)$AverDist,mean(d))
	checkEquals(speed(x),d/t)
	checkEquals(speedSummary(x)$AverSpeed, mean(d/t))
	checkEquals(timeLag(x), t)
	checkEquals(timeLag(x, 'secs'), t)
	checkEquals(timeLag(x, 'mins'), t/60)
	xx<-moveStack(list(x,x))
	checkEquals(distance(xx),list(unnamed=d,unnamed1=d))
	checkEquals(timeLag(xx),list(unnamed=t,unnamed1=t))
	checkEquals(speed(xx),list(unnamed=d/t,unnamed1=d/t))
	proj4string(x)<-'+proj=longlat'
	checkEquals(distance(x),(dd<-distHaversine( cbind(dx,dx)[-n.locs(x),], cbind(dx,dx)[-1,])))
	checkEquals(speed(x),dd/t)


}
