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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gumph 1-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Random checks on centroid data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#On centroid data... 15km band, right?
rez <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/wholeWindfarms/viewshedOutput_centroids_wholeWindfarms.csv")
rez2 <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/windfarms_brokenDownByExtension/viewshedOutput_centroids_WindfarmsByExtension.csv")
demz <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/housingDEMstats_postcodecentroids2.csv")

table(0 + (rez$distanceToNearest!=-1))
table(0 + (rez2$distanceToNearest!=-1))

#Check that against postcodes in the actual sales data
salez <- 
  read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/SingleSalesPlusRepeatSales_filtered_July16.csv")

#unique postcodes from sales
salez_pcs <- unique(salez$postcode_via_pip)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checks before running all-sales through viewshed model-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Just look at previously used repeat sales and the new file, make sure it's all being sensible.
#Then make a plan for keeping in with the previous ID/Title pairs.

latestSalez <- 
  read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/SingleSalesPlusRepeatSales_filtered_July16.csv")

prevSalez <- 
  read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/repeatSales_22_5_16.csv")

#latest sales' repeat sales should just be the same as the previous 
#Yup.
table(latestSalez$isRepeatSale)
#Yup.
unique(latestSalez$Title[latestSalez$isRepeatSale==1]) %in% unique(prevSalez$Title) %>% table

#So I need to re-run things just for those non-repeat-sales properties. 
#Then I can just come back here and append everything.
singleSalez <- latestSalez %>% filter(isRepeatSale == 0)

#Those are by definition already individual properties...
unique(singleSalez$Title) %>% length

#What does previous housing file going into batch creator look like? Need same structure.
prevhs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/houses_finalMay2016.csv")
#"id"             "Title"          "eastingsFinal"  "northingsFinal"
names(prevhs)

#Ah, so need to sort ID! Not complex: just find previous max and take it from there.
#652472
#Or Possibly just start from a million so it's easier to distinguish them? Yeah.
max(prevhs$id)

#Previously, id matched alphabetical order of title numbers.
#Best do the same.
singleSalez <- singleSalez %>% arrange(Title)

singlez4batch <- singleSalez %>% dplyr::select(Title,eastingsFinal,northingsFinal)

#New ID starting at 1000000
singlez4batch$id <- seq(from = 1000000, to = (1000000 + (nrow(singlez4batch)-1) )) 

#match column order
singlez4batch <- singlez4batch %>%  dplyr::select(id,Title:northingsFinal)

#And that's our single sales property file to go into batch making
write.csv(singlez4batch, 
          "C:/Data/WindFarmViewShed/ViewshedPython/Data/singleSales/singleSalesHouses_July2016.csv", row.names = F)












