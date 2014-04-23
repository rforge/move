context('Brownian bridge dyn')
test_that('extent calculations',{
	data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	expect_equal(
		extent(data),
		extent(.extcalc(data,0))
	)
	expect_equal(
		extent(data)*2,
		extent(.extcalc(data,0.5))
	)
	expect_equal(
		extent(data)*1:2,
		extent(.extcalc(data,c(0,0.5)))
	)
})
test_that('extent calculations between bgb and dbbmm',{
	data<-move(x=rep(2:3,9)/5, y=rep(2:3,9)/5, Sys.time()+1:18)# needs to be divisable by 50, and specifc rounding because otherwise one cell gets added in the ud calc of brownian
	expect_equal(
		raster(dynBGB(data, locErr=.1, ext=7, dimSize=50, windowSize=11, margin=5)),
		raster(brownian.bridge.dyn(data, location.error=.1, ext=7, dimSize=50, window.size=11, margin=5))
	)
	expect_equal(
		raster(dynBGB(data, locErr=.1, ext=7, raster=.020, windowSize=11, margin=5))
		,
		raster(brownian.bridge.dyn(data, location.error=.1, ext=7, raster=.020, window.size=11, margin=5))
	)
})
