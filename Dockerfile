# This is a comment
FROM rocker/r-devel
MAINTAINER bart
RUN apt-get update \
  && apt-get install -t unstable -y --no-install-recommends \
    libgdal-dev \
	libproj-dev \ 
	ghostscript \
	qpdf
RUN Rdevel -e 'install.packages(c("sp","raster","testthat","ggmap","geosphere","adehabitatLT", "adehabitatHR","mapproj","httr","rgdal","lattice","maptools","circular","MASS","boot","foreign"))'

