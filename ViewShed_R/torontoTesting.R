#Prep data for toronto viewshed checks.
library(plyr)
#Memory checking
library(pryr)
library(stringr)

geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)


#Nab DEM / shapefile code.
#Actually might just be able to use existing DEM

#Load raster made by Stephan
buildingz <- raster('C:/Users/SMI2/Dropbox/SheffieldMethodsInstitute/SharedDropboxFolders/WindFarms/Toronto/RasterBuildings.tif')

summary(buildingz)

#Make ground-level raster with the same extent
#http://gis.stackexchange.com/questions/23841/create-a-raster-with-georeferenced-information-in-r
# buildingz@extent
# xrange <- buildingz@extent[2]-buildingz@extent[1]
# yrange <- buildingz@extent[4]-buildingz@extent[3]

#NOOOO! That's the projection, eejut. This.
ncol(buildingz)
nrow(buildingz)

ground <- matrix(0,nrow(buildingz),ncol(buildingz))

ground <- raster(ground)
extent(ground) <- buildingz@extent
crs(ground) <- buildingz@crs

r2 <- max(buildingz, ground, na.rm=T)

writeRaster(r2, 'C:/Data/WindFarmViewShed/QGIS/TorontoTests/buildings_w_zeroGround.tif')
