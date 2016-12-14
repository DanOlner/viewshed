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

#Write ground. Underlying raster for Java code.
#writeRaster(ground, 'C:/Data/WindFarmViewShed/QGIS/TorontoTests/zero_ground.tif')
writeRaster(ground, 'C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/rasters/1.tif')

r2 <- max(buildingz, ground, na.rm=T)

#writeRaster(r2, 'C:/Data/WindFarmViewShed/QGIS/TorontoTests/buildings_w_zeroGround.tif')
writeRaster(r2, 'C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/rasters_w_buildingheight/1.tif',
            datatype='FLT4S')

#~~~~~~~~~~~~~~~~~~~~
#Random points----

#Within the toronto area. Use the convex hull from QGIS
#hull <- readOGR(dsn = 'C:/Data/WindFarmViewShed/QGIS/Tests',layer = 'convexHullToronto')
#Wrong projection, try again... 
hull <- readOGR(dsn = 'C:/Data/WindFarmViewShed/QGIS/TorontoTests',layer = 'hullReprojection')

plot(hull)

#~~~~
#Test smaller subset
xy=locator(4,"p") 

#http://gis.stackexchange.com/questions/206929/r-create-a-boundingbox-convert-to-polygon-class-and-plot
boundingBox <- Polygon(xy)
boundingBoxPoly <- SpatialPolygons(list(Polygons(list(boundingBox), ID = "a")))
proj4string(boundingBoxPoly) <- proj4string(hull)

plot(boundingBoxPoly,add=T)

#~~~~

observers <- spsample(hull,500000,'random')
points(observers[sample(1000),])

targets <- spsample(hull,500000,'random')
points(targets[sample(1000),],col='red')

#Or for subset test

observers <- observers[boundingBoxPoly,]
points(observers[sample(50),],col='green')

targets <- targets[boundingBoxPoly,]
points(observers[sample(50),],col='green')

#To dataframes
ob_df <- data.frame(observers)
tg_df <- data.frame(targets)

#id columns
ob_df$id <- seq(from = 0, to = nrow(ob_df)-1)
ob_df[nrow(ob_df),]

tg_df$id <- seq(from = 0, to = nrow(tg_df)-1)
tg_df[nrow(tg_df),]


#Heights for the "observers"
#Which is the trees in this example
#All back to front there. Oh well.
hist(rnorm(10000,mean = 25,sd = 5))

ob_df$height <- rnorm(nrow(ob_df),mean = 25,sd = 5)

ob_df <- ob_df[,c(3,1,2,4)]
tg_df <- tg_df[,c(3,1,2)]

#write straight to the right place
write.csv(ob_df,'C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/observers/1.csv',row.names = F)
write.csv(tg_df,'C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/targets/1.csv',row.names = F)

























