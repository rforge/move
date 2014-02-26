# echo 'require(testthat); require(raster);require(geosphere); suppressMessages(require(rgdal)); t<-sapply(list.files("R", full.names=TRUE), source); test_dir("tests/testthat")' | R -q --vanilla
context("dynBGB")
test_that("dyn bgb basics",{
	  data <- move(system.file("extdata","leroy.csv.gz",package="move"))
	  dataC<-spTransform(data, center=T)
	  resUd<-45.3
	  ud<-dynBGB(dataC[1:50,], windowSize=31, margin=15, locErr=4, raster=resUd, maxInt=12342342, ext=3)
	  ud2<-dynBGB(dynBGBvariance(dataC[1:50,], windowSize=31, margin=15, locErr=l<-rep(4, n.locs(dataC))), raster=resUd, maxInt=12342342, ext=3, locErr=l)

	  expect_is(ud, 'dynBGB')
	  expect_is(ud, '.UD')
	  expect_equal(res(ud), resUd[c(1,1)])
	  expect_equal(proj4string(ud), proj4string(dataC))
	  expect_equal(ud2, ud)
	  
})
