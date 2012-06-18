qSum<-function(x, tstamps,t,e)
{
	g<-expand.grid(t=t,e=e)
	
	z<-((x-t)/var(x))
	zCum<-cumsum(z)
	l<-sum(tstamps<=(tstamps[1]+e))
	zInt<-zCum[l]+(zCum[l+1]-zCum[l])*(((tstamps[1]+e)-tstamps[l])/(tstamps[l+1]-tstamps[l]))# see if E runs to less than minimal time difference
	positive<- zCum[1]<zInt
	potBreaks<-(2:(length(x)-1))[diff(diff(zCum)<0)!=0]
	trend<-rep(positive, length(x))
	breaks<-c()
	for(i in potBreaks){
		l<-sum(tstamps<=(tstamps[i]+e))
		zInt<-zCum[l]+(zCum[l+1]-zCum[l])*(((tstamps[i]+e)-tstamps[l])/(tstamps[l+1]-tstamps[l]))
		if((tstamps[i]+e)>tail(tstamps,1))# check if i am interpolating to values outside the track if so take the last value
		{
			zInt<-tail(zCum,1)
		}
		if(((zCum[i]<zInt)!=positive))
		{
			positive<- !positive
			trend[i:length(x)]<-positive
			breaks<-c(breaks,i)
		}
	}
	pres<-1-.5*(
		    sum((x[trend]/sum(x[trend])-1/sum(trend))^2) +
		    sum((x[!trend]/sum(x[!trend])-1/sum(!trend))^2))



}

