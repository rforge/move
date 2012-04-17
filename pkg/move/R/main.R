#source(file="~/Documents/Programming/Rmove/move/R/ClassDBBMM.R")
# test <- move(x="~/Documents/Programming/Rmove/move/data/BCI_Ocelot_Bobby.csv")#, proj=CRS("+proj=longlat"))
# testtest <- spTransform(test, proj="+proj=longlat", center=TRUE)
# p <- brownian.bridge.dyn(object=testtest, location.error=23.5, dimSize=45, ext=1.8)
# plot(p)
# image(p)
# plot(testtest, add=TRUE)
# contour(p, levels=c(.5,.9), add=T)
# plot(test, google=TRUE)
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