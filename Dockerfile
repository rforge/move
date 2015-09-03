# This is a comment
FROM rocker/r-devel
MAINTAINER bart
RUN Rdevel -e 'install.packages(c("sp","raster","testthat","ggmap","geosphere","adehabitatLT","adehabitatHR","circular","mapproj","maptools"))'
RUN apt-get update \
  && apt-get install -t unstable -y --no-install-recommends \
    libgdal-dev \
	libproj-dev
RUN Rdevel -e 'install.packages(c("RCurl","rgdal","lattice"))'

