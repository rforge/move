# source(file="~/Documents/Programming/Rmove/move/pkg/move/R/ClassDBBMM.R")
# test <- move(x="~/Documents/Programming/Rmove/old_move/data/BCI_Ocelot_Bobby.csv")#, proj=CRS("+proj=longlat"))
 testtest <- spTransform(test, proj="+proj=longlat", center=TRUE)
 p <- brownian.bridge.dyn(object=testtest, location.error=23.5, dimSize=45, ext=1.8)
 plot(p)
 image(p)
 plot(testtest, add=TRUE, col="grey")
 contour(p, levels=c(.5,.9), add=T)
 plot(test, google=TRUE)
# testtest


###TEST ONLY FUNCTIONS
# test <- move("~/Documents/Programming/Rmove/move/data/Lab Rat-1940314.csv", proj="+aa")
# test <- move("~/Documents/Programming/Rmove/move/data/Lab Rat-1940314.csv", proj="+proj=longlat")
# 
# test <- move(x="~/Documents/Programming/Rmove/move/data/Lab_Rat_1940314.csv", proj=CRS("+proj=longlat"))
# test <- move(x="~/Documents/Programming/Rmove/move/data/BCI_Ocelot_Bobby.csv", proj=CRS("+proj=longlat"))
# test <- move(x="~/Documents/Programming/Rmove/move/data/BCI_Ocelot_Barrote.csv", proj=CRS("+proj=longlat"))
# test <- move(x="~/Documents/Programming/Rmove/move/data/BCI_Ocelot_Isaac.csv", proj=CRS("+proj=longlat"))
# 
#   test
#   plot(test)
# 
#  str(test)
# test@sdf@data[1:2, ]
#
#
#
#x <- "/Users/marcosmolla/Documents/Programming/Rmove/old_move/data/BCI_Ocelot_Bobby.csv"
#x <- "/Users/marcosmolla/Desktop/test_DATA.csv"
#data <- read.csv(x, header=TRUE, sep=",", dec=".")
#newmove <- move(x=data$location.long,y=data$location.lat,time=data$timestamp,data=data)
#newmove <- move(x=data$location.long,y=data$location.lat,time=data$timestamp,data=data)
#newmove