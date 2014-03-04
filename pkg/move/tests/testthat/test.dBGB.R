# echo '`require(testthat); require(raster);require(geosphere); suppressMessages(require(rgdal)); t<-sapply(list.files("R", full.names=TRUE), source); test_dir("tests/testthat")' | R -q --vanilla
context("dynBGB")
test_that("dbgb vs dbbmm variance",{
	 vBBMM<-move:::brownian.motion.variance(x<-c(1,1.5,3,3.5,5,5.5,7,7.5,9), y<-c(0,.5,0,.5,0,.5,0,.5,0), time.lag=rep(1,9), location.error=rep(.1,9))
	 yy<-move(x,y, Sys.time()+1:9*60)
	 vBGB<-move:::BGBvar(yy, locErr=rep(.1,9))
	 expect_equal(unlist(vBBMM$cll), unlist(vBGB[3]), check.names=F, tolerance=1e-7)
	 expect_equal(vBBMM$BMvar, prod(vBGB[1:2]), tolerance=2e-5)
})
test_that("dbgb vs dbbmm",{
	 x<-12:42
	 xx<-floor(x/2)+(y<-((x/2)%%1)/6)
	 m<-move(x+y,y, Sys.time()+x)
#	 l<-(x+max(x))/(max(x)*5)
	 l<-rep(.1, length(x))
	 r<-raster(extent(m)+c(-22,1.5))
	 res(r)<-.025
	 tss<-.00110523/60
	 bgb<-dynBGB(m,r ,locErr=l, windowSize=23, margin=9, maxInt=123,  timeStep=(tss<-.00110523/60) ) 
	 bbmm<-brownian.bridge.dyn(m,raster=bgb ,location.error=l, window.size=23, margin=9 , time.step=tss)
#	 par(mfrow=c(2:1));plot(bgb); lines(m); points(m);plot(bbmm); lines(m); points(m)
#	 plot(bgb-bbmm); lines(m)
#	 plot((bgb-bbmm)/(bgb+bbmm), zlim=c(-.1,.1)); lines(m)
#	 plot( bgb@var@orthSd*bgb@var@paraSd, bbmm@DBMvar@means); abline(0,1)
	 bbmm@method<-'dynBGB'
	 expect_equal(as(bgb, '.UD'), as(bbmm, '.UD'), tolerance=tss*60)
})
test_that("deltaParaOrth",{
	  expect_equivalent(move:::deltaParaOrth(cbind(0,1), cbind(1,2), cbind(0,1)), cbind(0,0))
	  expect_equivalent(move:::deltaParaOrth(cbind(0,1), cbind(1,2), cbind(0,0)), cbind(-sqrt(.5),sqrt(.5)))
	  expect_equivalent(move:::deltaParaOrth(cbind(0,0), cbind(2,2), cbind(1,1)), cbind(sqrt(2),0))
	  suppressWarnings(expect_equivalent(move:::deltaParaOrth(cbind(0,0), cbind(0,0), cbind(1,1)), cbind(1,1)))
	  expect_warning(move:::deltaParaOrth(cbind(0,0), cbind(0,0), cbind(1,1)), "Brownian motion assumed, because no direction could be calculated")
	  
})
test_that("dyn bgb basics",{
	  data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	  dataC<-spTransform(data, center=T)
	  resUd<-5.3
	  ud<-dynBGB(dataC[1:45,], windowSize=31, margin=15, locErr=4, raster=resUd, maxInt=12342342, ext=3)
	  ud2<-dynBGB(dynBGBvariance(dataC[1:45,], windowSize=31, margin=15, locErr=l<-rep(4, n.locs(dataC))), raster=resUd, maxInt=12342342, ext=3, locErr=l)
	  expect_is(ud, 'dynBGB')
	  expect_is(ud, '.UD')
	  expect_equal(res(ud), resUd[c(1,1)])
	  expect_equal(proj4string(ud), proj4string(dataC))
	  expect_equal(ud2, ud)
})
