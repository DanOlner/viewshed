#distance matrix to coastal points for houses
library(dplyr)
library(tidyr)
library(pryr)
library(zoo)
library(ggplot2)
library(lubridate)
#library(sp)
geolibs <- c("pryr","stringr","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#Get points extracted from coastal polygon
coast <- readShapePoints("C:/Data/Temp/QGIS/nodes_GB.shp")
# coast <- readOGR(dsn="C:/Data/Temp/QGIS", 
#                        layer="nodes_GB")

coast <- coast[,c('GADMID')]

coast@data$id <- seq(1:nrow(coast))

#Wrong projection - which might be what was causing the problem in the first place
bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
proj4string(coast) <- 

coast2 <- spTransform(coast, CRS(bng))

writeOGR(coast, dsn="C:/Data/Temp/QGIS", layer="nodes_GB_withID", driver="ESRI Shapefile")
