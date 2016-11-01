#Distance matrix between all houses and all turbines.
library(Matrix)
library(dplyr)
library(pryr)
library(sp)
library(tidyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Distance matrix for all houses/all turbines----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Doesn't matter about updated tip heights
tb <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns.csv")
hs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/singleSales/singleSalesHouses_July2016.csv")

#1.6 billion possible cells. Ulp.
#m <- Matrix(0, nrow = 652473, ncol = 2560, sparse = TRUE)
#object_size(m)
#rownames(m) <- hs$id
#colnames(m) <- tb$index

#Slight time-saver. IDs will be in same order
tblocs <- as.matrix(tb[,c('Feature.Easting','Feature.Northing')])
hslocs <- as.matrix(hs[,c('eastingsFinal','northingsFinal')])

findDists <- function(rownum) {
  
  print(paste0("row ",rownum))
  
  #produce distance values per-property, so row-wise
  dists <- spDistsN1(tblocs, hslocs[rownum,],longlat = F)
    
  #Filter out any above 15km. Set to -1
  #Actually, -1 will not store well. Let's assume no property is on top of a turbine!
  #So: zero
  dists <- ifelse(dists < 15000, dists, 0)
  
  return(dists)
  
}


#Do in batches: each round of sapply keeps the result in memory
#Sticking it into the sparse matrix reduces it...
#Batches of 10000 each should do?
hs$batch <- as.numeric(cut(seq(1:nrow(hs)),breaks = nrow(hs)/65000))

for(i in 1:max(hs$batch)) {

#Get rows to process on this go
sequence <- as.integer(rownames(hs[hs$batch == i,]))
#sequence <- as.numeric(rownames(hs[hs$batch == i,]))

#transpose...
m <- sapply(sequence, function(x) findDists(x)) %>% t
#rownames(m) <- sequence - 1#to match house ID
rownames(m) <- (sequence - 1 + 1000000)#to match house ID, single Sales, starting at 1000000
colnames(m) <- tb$index

print(paste0("Rows: ", min(sequence)," to ",max(sequence)))
print(object_size(m))

write.csv(m,paste0("data/distanceMatrix/distanceMatrix",i,".csv"))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Distance matrix postcode centroids/WHOLE-windfarm centroids----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tb <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/whole_windfarm_centroids.csv")
hs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/postcode_centroids.csv")

#Slight time-saver. IDs will be in same order
tblocs <- as.matrix(tb[,c('Feature.Easting','Feature.Northing')])
#For some reason, matrix doesn't like x.
names(hs) <- c('id','postcode','eastings','northings')
hslocs <- as.matrix(hs[,c('eastings','northings')])

#Should be able to do in one file
m <- sapply(as.integer(rownames(hs)), function(x) findDists(x)) %>% t
rownames(m) <- as.integer(rownames(hs)) - 1#to match house ID
colnames(m) <- tb$id

#print(paste0("Rows: ", min(sequence)," to ",max(sequence)))
print(object_size(m))

write.csv(m,"C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/distanceMatrix_wholeWindfarms_vs_postcodes.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Distance matrix postcode centroids/windfarm-by-extension centroids----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tb <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/windfarm_centroids_extensions_separate.csv")
hs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/postcode_centroids.csv")

#Slight time-saver. IDs will be in same order
tblocs <- as.matrix(tb[,c('Feature.Easting','Feature.Northing')])
#For some reason, matrix doesn't like x.
names(hs) <- c('id','postcode','eastings','northings')
hslocs <- as.matrix(hs[,c('eastings','northings')])

#Should be able to do in one file
m <- sapply(as.integer(rownames(hs)), function(x) findDists(x)) %>% t
rownames(m) <- as.integer(rownames(hs)) - 1#to match house ID
colnames(m) <- tb$id

#print(paste0("Rows: ", min(sequence)," to ",max(sequence)))
print(object_size(m))

write.csv(m,"C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/distanceMatrix_Windfarms_W_EXTENSIONS_vs_postcodes.csv")
























#Sparse matrix version, not working after ~100K
# hs$batch <- as.numeric(cut(seq(1:nrow(hs)),breaks = nrow(hs)/10000))
# 
# for(i in 10:max(hs$batch)) {
#   #for(i in 1:10) {
#   
#   #Get rows to process on this go
#   sequence <- as.integer(rownames(hs[hs$batch == i,]))
#   #sequence <- as.numeric(rownames(hs[hs$batch == i,]))
#   
#   m[min(sequence):max(sequence),] <- sapply(sequence, function(x) findDists(x))
#   
#   print(paste0("Rows: ", min(sequence)," to ",max(sequence)))
#   print(object_size(m))
#   
# }

#Check matrix results
chk <- read.csv("C:/Data/WindFarmViewShed/ViewShed_R/data/distanceMatrix/distanceMatrix5.csv")

#Let's keep only non-zero cells
chk2 <- chk %>% gather(turbine,distance,X0:X2559) 

chk2 <- chk2 %>% filter(distance != 0)

#turbine 82
check82 <- chk2 %>% filter(turbine=='X82')

check82 %>% filter(X == 1307037)




