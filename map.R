
library(maps)
library(raster)
library(rgdal)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(rgeos)
library(dplyr)
# note that you don't need to call maptools to run the code below but it needs to be installed.
library(maptools)
# to add a north arrow and a scale bar to the map
library(ggsn)
# set factors to false
options(stringsAsFactors = FALSE)


map('world2', xlim = c(100,300))
map.axes()
# xlim is performed before wrapping:
map('world', wrap=c(0,360), xlim = c(100, 300))
# so to emulate "world2":
ww2 <- map('world', wrap=c(0,360), plot=FALSE, fill=TRUE)
map(ww2, xlim = c(100, 300), fill=TRUE)

map('world2', xlim = c(100, 300))
map.axes()
# xlim is performed before wrapping:
map('world', wrap=c(0,360), xlim = c(100, 300))
# so to emulate "world2":
ww2 <- map('world', wrap=c(0,360), plot=FALSE, fill=TRUE)
map(ww2, xlim = c(100, 300), fill=TRUE)