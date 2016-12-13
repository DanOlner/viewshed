#Various!
#Quick digging around into some price impact stuff / getting head around DiD.
library(dplyr)
library(tidyr)
library(pryr)
library(zoo)
library(ggplot2)
library(modeest)
library(readstata13)
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

chk <- read.dta13("C:/Users/SMI2/Dropbox/WindFarmsII/data/work/distanceMatrix_DanCheck.dta")

#merge back in geocodes
#geoc <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/UniqueAddressesOnly_repeatSales_areacodes2.csv")

rpts <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/repeatSales_22_5_16.csv")

ids <- unique(rpts[,c(1,2,39,40)])

chk2 <- merge(chk,ids,by.x = "id_house", by.y = "id")

#save for checking on map
write.csv(chk2,"C:/Data/temp/checkStephanOutput1.csv")

chk3 <- read.dta13("C:/Users/SMI2/Dropbox/WindFarmsII/data/work/master_001.dta")

#Check that for those turbs in-bounds
write.csv(chk3[,c(1:32)],"C:/Data/temp/checkStephanOutput2.csv")

#All good. Check building-heights-cross data
xx <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/doHousesCross_BuildingHeightData.csv")

#Subset all data to old council areas
#Load those subsetted council areas
oldca <- readShapeSpatial("C:/Data/temp/QGIS/misc/windfarms_southernCAs.shp")

#Load latest zone file to attach the result to / filter down to
zns <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/UniqueAddressesOnly_repeatSales_areacodes3_OAC.csv")

#check we have them all. Tick.
unique(oldca@data$label) %in% unique(zns$councilArea) %>% length

old_zns <- zns[zns$councilArea %in% unique(oldca@data$label),]

#old_zns_geo <- old_zns

old_zns_geo <- merge(old_zns,ids,by="Title")
coordinates(old_zns_geo) <- ~eastingsFinal+northingsFinal

plot(old_zns_geo)
lines(oldca)

#save that
write.csv(old_zns,
          "C:/Users/SMI2/Dropbox/WindFarmsII/data/original/firstreport_councilareas/unique_properties_areacodes_firstReportCouncilAreasOnly.csv")








