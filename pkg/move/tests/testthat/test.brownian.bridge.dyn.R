context('Brownian bridge dyn')
test_that('dbbmm error handling',{
	data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	expect_error(brownian.bridge.dyn(object=leroy, location.error=23.5, dimSize=150, ext=.3, time.step=600))
	r <- raster(nrows=100, ncols=100, xmn=0, xmx=10)
	expect_error(brownian.bridge.dyn(spTransform(data[1:50,], center=T), raster=r, location.error=20))# equal projection
	expect_error(brownian.bridge.dyn(data, raster=r, location.error='2346'))# character loc
	expect_error(brownian.bridge.dyn(data, raster=r, location.error=1:5))# multiple loc error not same length
})
test_that('brownian bridge dyn for bursted',{
	data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	dataP<-spTransform(data[1:100,], center=T)
	dataPB<-move::burst(dataP, round((1:(n.locs(dataP)-1)/50)))
	udS<-brownian.bridge.dyn(dataPB, dimSize=150, location.error=23, ext=.3, time.step=4, window.size=29)
	expect_equal(class(udS),"DBBMMBurstStack", check.attributes=F)
	ud<-brownian.bridge.dyn(dataP, dimSize=150, location.error=23, ext=.3, time.step=4, window.size=29)
	expect_equal(udS@DBMvar@means, ud@DBMvar@means)
	b<-as.numeric(getZ(udS))
	expect_equal(colSums(values(udS)), b/sum(b), check.attributes=F)
})
test_that('brownian bridge dyn value comparison bursted',{
	p<-seq(0, 2*pi, length.out=49)
	tmp<-move(sin(p), cos(p), as.POSIXct(1:length(p), origin='1970-1-1'), proj='+proj=aeqd')
	t<-.05
	u<-brownian.bridge.dyn(tmp, dimSize=200, location.error=.1, time.step=t)
	us<-brownian.bridge.dyn(move::burst(tmp, round(1:length(p[-1])/30)), dimSize=200, location.error=.1, time.step=t)
	expect_equal(values(u), c(values(u)))
	p<-seq(0, 2*pi, length.out=199)
	set.seed(3245)
	tmp<-move(sin(p)+rnorm(length(p))*round(sin(p))*.03, cos(p), as.POSIXct(1:length(p), origin='1970-1-1'), proj='+proj=aeqd')
	t<-.25
	u<-brownian.bridge.dyn(tmp, dimSize=500, location.error=.01, time.step=t, ext=.2, margin=15)
	us<-brownian.bridge.dyn(move::burst(tmp, round(1:length(p[-1])/30)), dimSize=500, location.error=.01, time.step=t, ext=.2, margin=15)
	expect_true(all(abs(values(sum(us))-values(u))< .Machine$double.eps)  )
})
