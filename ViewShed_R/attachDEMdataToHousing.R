#Attach slope/elevation/aspect data to housing files
#Working on each batch
#library(plyr)
#Memory checking
library(pryr)
library(stringr)

geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#~~~~~~~~~~~~~~~
#Housing DEM values attached to file batches, not all houses-----

#All-points version
#rootfolder = 'data'

#whole-windfarm-centroid version. Hang on... just need the one version, don't we? Bonza!
#(It's only related to the postcode centroids not the windfarms.)
#rootfolder = 'data_centroids1'
rootfolder = 'data_centroids2'

#Find the number of batches to work with
#will be sequenced 1 to x
filez <-  list.files(
  paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/targets'), pattern="*.csv")
#Note $: avoids *.tif.aux.xmls that may have been saved
rastaz <- list.files(
  paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/rasters'), pattern="*.tif$")



tottm <- proc.time()

for (i in 1:length(filez)) {
  
  houses <- read.csv(paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/targets/',filez[i]))
  
  #coordinates(houses) =~eastingsFinal+northingsFinal
  #different variable names for postcode centroids, oops
  coordinates(houses) =~x+y
  
  r <- raster(paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/rasters/',rastaz[i]))
  
  #make slope and aspect rasters from that
  #Do both at same time, get rasterbrick
  tm <- proc.time()
  s <- terrain(r, neighbors = 8, opt=c('slope', 'aspect'), unit='degrees')
  print(proc.time() - tm)
  
  #add elevation original to rasterbrick
  s <- addLayer(r,s)
  
  #http://www.gisremotesensing.com/2012/10/extract-raster-values-from-points.html
  v <- as.data.frame(extract(s, houses))
  combinz <- cbind(houses,v)
  
  #random 'optional = TRUE' column appears
  combinz <- subset(combinz, select = -optional)
  
  #And rename elevation column
  names(combinz)[5] <- 'elevation'
  
  #save new housing CSV
  write.csv(combinz, paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/targetsplusDEMdata/',filez[i]))
   
}

print("Done:")
proc.time() - tottm


#Version for doing any postcodes in centroids2 not covered by centroids1
#If running centroids2, get first set of results 
#and only run for postcode centroids not done before
firstSet <- read.csv("data/housingDEMstats_postcodecentroids.csv")

tottm <- proc.time()

for (i in 1:length(filez)) {
  
  houses <- read.csv(paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/targets/',filez[i]))
  
  #keep only those that haven't been processed already
  print(paste0("Total postcodes: ", nrow(houses)))
  
  houses <- houses[!(houses$postcode %in% firstSet$postcode), ]
  
  print(paste0("Not processed: ", nrow(houses)))
  
  #If running for centroids2 (windfarms including extensions)
  #Only run for those centroids not already done in 1.
  
  #only needs doing if we have any values to process
  if(nrow(houses) > 0 ) {
  
    coordinates(houses) =~x+y
    
    r <- raster(paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/rasters/',rastaz[i]))
    
    #make slope and aspect rasters from that
    #Do both at same time, get rasterbrick
    tm <- proc.time()
    s <- terrain(r, neighbors = 8, opt=c('slope', 'aspect'), unit='degrees')
    print(proc.time() - tm)
    
    #add elevation original to rasterbrick
    s <- addLayer(r,s)
    
    #http://www.gisremotesensing.com/2012/10/extract-raster-values-from-points.html
    v <- as.data.frame(extract(s, houses))
    combinz <- cbind(houses,v)
    
    #random 'optional = TRUE' column appears
    combinz <- subset(combinz, select = -optional)
    
    #And rename elevation column
    names(combinz)[5] <- 'elevation'
    
    #save new housing CSV
    write.csv(combinz, paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/targetsplusDEMdata/',filez[i]))
  
  }#endif
  
   
}

print("Done:")
proc.time() - tottm

#~~~~~~~~~~~~~~~~
#Now reload all of those and keep only one per property
filez2 <-  list.files(paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/',rootfolder,'/targetsplusDEMdata/'), 
                      pattern="*.csv$",
                      full.names = T)

allz <- lapply(filez2, read.csv)
inone <- do.call("rbind", allz)

#duplicate columns should have same values... Yup.
dups <- subset(inone, duplicated(inone$Title)|duplicated(inone$Title, fromLast = T))
dups <- dups[order(dups$Title),]
#for centroids
dups <- subset(inone, duplicated(inone$postcode)|duplicated(inone$postcode, fromLast = T))
dups <- dups[order(dups$postcode),]

#Keep only unique titles
#Oh good, it's not all of them! Which, now I think about it, makes sense: 
#not all properties will be anywhere near the created rasters.
oonz <- subset(inone, !duplicated(inone$postcode))

#I'll check that's actually the issue... Yup yup. Hmm. Different approach needed huh?
# write.csv(oonz[,2:8],"data/housingDEMstats.csv", row.names = F)
#write.csv(oonz[,2:8],"data/housingDEMstats_postcodecentroids.csv", row.names = F)

#write.csv(oonz[,2:8],"data/housingDEMstats_postcodecentroids2.csv", row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Different setup if adding extras from centroids2
#I only want those unique to centroids2 - which is all postcode centroids in the target files.
#Let's get that list first.
centroids2keep <- list.files(
  paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data_centroids2/targets'), pattern="*.csv",
  full.names = T)

centroids2keep <- lapply(centroids2keep, read.csv)
centroids2keep <- do.call("rbind", centroids2keep)

centroids2keep <- subset(centroids2keep, !duplicated(centroids2keep$postcode))

#Then combine first set with second ones just found
filez2 <-  list.files(paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data_centroids2/targetsplusDEMdata/'), 
                      pattern="*.csv$",
                      full.names = T)

allz <- lapply(filez2, read.csv)
inone <- do.call("rbind", allz)

oonz <- subset(inone, !duplicated(inone$postcode)) %>% dplyr::select(2:8)

#Add that to the first one...
oonz2 <- rbind(firstSet, oonz)

#And keep only those from centroids2 run
oonz3 <- oonz2[oonz2$postcode %in% centroids2keep$postcode,] 

#Oh: there are none missing from centroids1 in centroids2. Thought there might be. Oh well!
#Might as well save over the previous one....?
write.csv(oonz3,"data/housingDEMstats_postcodecentroids2.csv", row.names = F)


#~~~~~~~~~~~~~~~
#Housing DEM values attached to all houses-----

#Load entire housing set
# hse <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/houses_finalMay2016.csv")
# 
# #Load grid ref lookup for those
# grid <- readOGR(dsn="C:/Data/MapPolygons/Generated/NationalGrid5kmSquares_for_OSterrain", 
#               layer="NationalGrid5kmSquares_for_OSterrain")
# 
# coordinates(hse) <- ~eastingsFinal+northingsFinal
# #Coz I know it's national grid...
# proj4string(hse) <- proj4string(grid)
# 
# #hse@data$OSgridSquare <- (hse %over% grid) %>% dplyr::select(raster_ref)
# #Ah, that seemed to work!
# hse@data$OSgridSquare <- over(hse,grid,returnList = F) %>% dplyr::select(raster_ref) %>% unlist
# #test <- over(hse,grid,returnList = F)
# 
# 
# 
# #There should be no houses without a raster
# table(0 + (is.na(hse@data$raster_ref)))
# 
# class(hse@data$OSgridSquare)
# 
# hse@data$grid <- as.character(hse@data$OSgridSquare)








