#Create lines between properties and turbines
#using 15km buffer around turbines for line limit

library(dplyr)
#Memory checking
library(pryr)
library(stringr)

geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

tb <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns.csv")
#hs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/houses_finalMay2016.csv")
hs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/singleSales/singleSalesHouses_July2016.csv")

coordinates(tb) <- ~Feature.Easting+Feature.Northing
coordinates(hs) <- ~eastingsFinal+northingsFinal

proj4string(tb)=CRS("+init=epsg:27700")
proj4string(hs)=CRS("+init=epsg:27700")

#To start with, how many lines?
#buff <- gBuffer(tb, byid = F, width=15000)
buff <- gBuffer(tb,byid = T, width=15000)

#Far too large! We just want count per turbine... 
#hsesover <- over(hs, buff, returnList = T)
#Works for data
#apply(buff@data, 1, function(x) print(x))
# countz = c()
# 
# for(i in 1:nrow(buff)) {
# # for(i in 6:7) {
#   countz = c(countz,hs[buff[i,],] %>% nrow)
# }
# 
# #I might save that! Could be useful
# perTurbineBufferHouseCounts <- data.frame(index = tb@data$index, count = countz)
# write.csv(perTurbineBufferHouseCounts,"C:/Data/WindFarmViewShed/ViewshedPython/Data/houseCountPerTurbine15kmBuffer.csv")
# 
# #So total number of lines is per turbine count summed, innit?
# sum(perTurbineBufferHouseCounts$count)

#17,441,796. Hmm. 
#4.5 million was a 10gb shapefile. Won't be doing that all in one go!

#Haven't tried this yet. Just did. Don't think so!
#http://gis.stackexchange.com/questions/148655/r-apply-function-in-turn-to-each-subset-of-a-spatialpolygonsdataframe
#testz <- sapply(slot(buff[1:2,],"polygons"), function(x) hs[x,])

######

#allLines <- c()

start = proc.time()

#cycle over all turbines, find houses in view, find lines between turbine and houses
#Label with house ID
#for(j in 1:3) {
#for(j in sample(1:nrow(tb),8)) {
for(j in 1:nrow(buff)) {
# for(j in as.numeric(row.names(tb[tb@data$nameMinusTurbine=='Cathekin Braes',]))) {

  bfhouses <- hs[buff[j,],]
  bfhouses_df <- data.frame(bfhouses)
  #bfhouses <- list(data.frame(hs[buff[j,],]))
  
  bfhouses.list <- as.list(data.frame(t(bfhouses_df),stringsAsFactors = F))
  
  print(paste0("Turbine ",j," starting. ",nrow(bfhouses)," houses."))
  
  thistime = proc.time()
  
  #For loop from here... changed to lapply below to speed up
  #http://stackoverflow.com/questions/20531066/convert-begin-and-end-coordinates-into-spatial-lines-in-r
  
  #Thing that took the time here: working out that the list had the coords in as factors.
  l2 <- lapply(bfhouses.list, function(x) 
    Lines(list(Line(rbind(
    coordinates(tb[j,]), 
        # tb[j,c('Feature.Easting','Feature.Northing')], 
    cbind(as.numeric(x[3]),as.numeric(x[4]))
        # bfhouses[i,c('eastingsFinal','northingsFinal')]
    ))),
    paste0(x[2],"_turbine",tb@data[j,'index'])))

  #allLines <- c(allLines,l2)
  
  #Write all shapefiles to folder
  ldf <- SpatialLines(l2)
  ldf2 <- SpatialLinesDataFrame(ldf, data = data.frame(row.names(ldf)), match.ID = F)
  
  writeLinesShape(ldf2,paste0("C:/Data/temp/QGIS/linesOfSightShapefiles/singleSales/linesbfhouses_",j,".shp"))
  
  print(paste0("Turbine ",j," done. ",round((proc.time() - thistime)[[3]],digits=2)," secs. ",
               round(((proc.time() - start)[[3]]/60),digits=2)," mins since start."))
  
}

#~~~~~~~~~~~~~~~~~~
# Part two: "Did this property have line of sight passing through building height data?"----

#After having shifted the above line shapefiles through SAGA's polygon/line intersection
#Here, let's attach the flag to the housing data - which ones passed through building height data?
#And so could potentially have had line of sight blocked by a building?

#Get each list of files for mastermap/CEDA
mm_filez <- list.files("C:/Data/temp/QGIS/linesOfSightIntersects_mastermap/singleSales",pattern = "*.csv$",full.names = T)
ceda_filez <- list.files("C:/Data/temp/QGIS/linesOfSightIntersects_CEDA/singleSales",pattern = "*.csv$",full.names = T)

#Why diff number? More CEDA files? If empty intersects were still writing, why aren't there 2560?
#There are in fact 2560 line shapefiles. I don't like this!
#mm_filez2 <- list.files("C:/Data/temp/QGIS/linesOfSightIntersects_mastermap",pattern = "*.csv$")
#ceda_filez2 <- list.files("C:/Data/temp/QGIS/linesOfSightIntersects_CEDA",pattern = "*.csv$")

#That's odd: sometimes it seems to have written an empty files
#Other times just not written one at all.
#Oh well... press on!

#Get the housing data too: reduce as well, but use for also marking which properties 
#are within 15km of at least one turbine
hs <- read.csv("C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/output/singleSales.csv")

hs <- hs %>% dplyr::select(id:distanceToNearest)
#distanceToNearest is -1 if no turbine was within 15km
#Oh hang on - that already is a flag. Leave!

#Empty mastermap and CEDA flag columns
hs$crossesMM <- 0
hs$crossesCEDA <- 0

for (file in mm_filez){
  
  print(file)  
  
  df <- read.csv(file, as.is = T)
  
  #Turbine number is there if we need it... but right now we don't.
  #Field looks like: MID100163_turbine1114
  df$Title <- sapply(df$row_names_, function(x) strsplit(x,"_")[[1]][1])
  
  #Any one we have a record for: that property crosses mastermap data
  #And we have no way of doing this without churning over them all!
  
  hs$crossesMM[hs$Title %in% unique(df$Title)] <- 1
  
}


#CEDA
for (file in ceda_filez){
  
  print(file)  
  
  df <- read.csv(file, as.is = T)
  
  #Turbine number is there if we need it... but right now we don't.
  #Field looks like: MID100163_turbine1114
  df$Title <- sapply(df$row_names_, function(x) strsplit(x,"_")[[1]][1])
  
  #Any one we have a record for: that property crosses mastermap data
  #And we have no way of doing this without churning over them all!
  
  hs$crossesCEDA[hs$Title %in% unique(df$Title)] <- 1
  #table(hs$crossesMM)#yup, working, it appears...
  
  
}

table(hs$crossesMM)#yup, working, it appears...
table(hs$crossesCEDA)#yup, working, it appears...
table(hs$crosses_BH)#yup, working, it appears...
table(hs$crossesMM[hs$distanceToNearest!=-1])#yup, working, it appears...
table(hs$crossesCEDA[hs$distanceToNearest!=-1])#yup, working, it appears...

#If crosses either...
hs$crosses_BH <- bitwOr(hs$crossesMM,hs$crossesCEDA)

#Save!
write.csv(hs,"C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/singleSales/doSingleSalesHousesCross_BuildingHeightData.csv",row.names = F)














