#Looking for some inequalities in the housing/turbine data
#Based on data processed in lookingForErrorsinData
library(readstata13)
library(dplyr)
library(pryr)
library(zoo)
library(stringr)
library(qdap)
library(ggplot2)
library(ineq)
library(stringdist)
library(tidyr)
library(scales)
library(data.table)
library(NCmisc)
library(class)

geolibs <- c("pryr","stringr","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#from lookingForErrorsInData.R, in the "repeat all that for distance BANDS" section
# hs_turbDistBANDS <- readRDS("data/houses_withModalDate_turbDistBANDS.rds")
# sales_plus_turbBANDS <- readRDS("data/temp/sales_plus_turbBANDS.rds")

#Stephan's master file (all sales)
master <- read.dta13("C:/Users/SMI2/Dropbox/WindFarmsII/data/work/master_combined_001.dta")

unique(master$title) %>% length

#Just checking viz is doing what I think it is... yup
chk <- master[,c("turb5","turb_vis_trn5","turb_vis_bldg5")]

#And the date version. This is "turbine was built after this sale" (if I use time)
chk2 <- master[,c("house_date_q","turb_after5","turb_vis_trn_after5","turb_vis_bldg_after5")]

#and check we do have repeat sales
master <- master[order(master$title),]

#Err...
master <- master[order(master$id_house),]
master_mill <- master[master$id_house > 999999,]

#Oh I remember! I added in all single sales. That's why they're all single sales! Right then.

#So to start with, just look at properties / location, not sales
#Info repeated for each, so just need to keep titles
properties <- subset(master, !duplicated(master$title)) %>% dplyr::select(-(id_house:ur2_indicator))

#Flag properties for whether there's at least one turbine in
#each of 3 distance bands
#names(properties)
#unique(properties %>% dplyr::select(tipheight0:tipheight14))

#Think I only want to look at places with larger turbines
properties$maxTipHeight <- apply(properties %>% dplyr::select(tipheight0:tipheight14),1,max)

head(properties$maxTipHeight,100)

hist(properties$maxTipHeight)

ggplot(properties, aes(x = maxTipHeight)) +
  geom_histogram(binwidth = 10, colour = "black")

#Keep only those properties with a turbine 80m or more within 15km
#771791 down from ~1m
properties <- properties[properties$maxTipHeight > 79,]

#Get SIMD scores. Lower values more deprived
simd <- read.csv("file:///C:/Data/IMD/ScotlandIMD/Scots_IMD2011.csv")

#check datazones match. Hmm, that's not great!
table(simd$Data_Zone %in% properties$datazone)

#What's the count of each?
unique(simd$Data_Zone) %>% length
unique(properties$datazone) %>% length

#Oh OK: it's just where the housing data is. 
#There isn't any in quite a few of the datazones.
#So properties: 4948 datazones (one of which I suspect is NA)
#4947 match from the SIMD.

#Merge in the SIMD rank
properties2 <- merge(properties, simd[,c("Data_Zone","Overall_SIMD16_rank")],
                     by.x = 'datazone', by.y = 'Data_Zone')

#save that
saveRDS(properties2, "data/properties_w_turbines_and_simd.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get distance band (by column name) with the largest number of turbines in
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Can come back to getting the first...

#http://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
maxcolz <- names(properties2[,c(18:32)])[max.col(properties2[,c(18:32)],ties.method="first")]

properties2$modalDistanceBand <- substr(maxcolz,5,7) %>% as.numeric()

#bin SIMD
properties2$SIMD_bins <- cut_number(properties2$Overall_SIMD16_rank,10) %>% as.numeric()

#Stare at some sample columns
chk <- properties2 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins)

#No layering, dammit!
plot(properties2$modalDistanceBand,properties2$Overall_SIMD16_rank,col=rgb(0, 0, 0, 0.005))

ggplot(properties2, aes(x = modalDistanceBand, y = Overall_SIMD16_rank)) +
  geom_point(alpha = 0.005)

#Table shows the numbers...
table(properties2$modalDistanceBand,properties2$SIMD_bins)

rankbinVsModalDist <- table(properties2$modalDistanceBand,properties2$SIMD_bins) %>% data.frame()

ggplot(rankbinVsModalDist, aes(x = Var2, y = Freq, colour = Var1, group = Var1)) +
  geom_line()

#Proportion version?
tbl <- table(properties2$modalDistanceBand,properties2$SIMD_bins)
tbl <- prop.table(tbl,1) * 100

rankbinVsModalDist <- tbl %>% data.frame()

ggplot(rankbinVsModalDist, aes(x = Var2, y = Freq, colour = Var1, group = Var1)) +
  geom_line()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Let's try nearest rather than modal.----
#~~~~~~~~~~~~~~~~~~~

#If I set all positive distance band columns to one
#Can just use max col picking first again.
#(Presuming first means what I think it does)
#So get vanilla distance band columns... 
binaryz <- apply(properties2[,c(18:32)], c(1,2), function(x) ifelse(x != 0, 1,0))
binaryz <- data.frame(binaryz)
#names(binaryz) <- paste0("binary_",names(binaryz))

#http://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
#Can use same cos names in binaryz same
maxcolz <- names(binaryz)[max.col(binaryz,ties.method="first")]

properties2$nearestTurbine <- substr(maxcolz,5,7) %>% as.numeric()

#bin SIMD
properties2$SIMD_bins <- cut_number(properties2$Overall_SIMD16_rank,10) %>% as.numeric()

#Stare at some sample columns
chk <- properties2 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine)

#No layering, dammit!
plot(properties2$nearestTurbine,properties2$Overall_SIMD16_rank,col=rgb(0, 0, 0, 0.005))

ggplot(properties2, aes(x = modalDistanceBand, y = Overall_SIMD16_rank)) +
  geom_point(alpha = 0.005)

#Table shows the numbers...
table(properties2$nearestTurbine,properties2$SIMD_bins)

rankbinVsnearest <- table(properties2$nearestTurbine,properties2$SIMD_bins) %>% data.frame()

ggplot(rankbinVsnearest, aes(x = Var2, y = Freq, colour = Var1, group = Var1)) +
  geom_line()

#Proportion version?
tbl <- table(properties2$nearestTurbine,properties2$SIMD_bins)
tbl <- prop.table(tbl,1) * 100

rankbinVsnearest <- tbl %>% data.frame()

ggplot(rankbinVsnearest, aes(x = Var2, y = Freq, colour = Var1, group = Var1)) +
  geom_line()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Next: anything from nearest visible?----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

binaryz2 <- apply(properties2[,c(33:62)], c(1,2), function(x) ifelse(x != 0, 1,0))
binaryz2 <- data.frame(binaryz2)
#names(binaryz) <- paste0("binary_",names(binaryz))


#http://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
#Can use same cos names in binaryz same
maxcolz <- names(binaryz2)[max.col(binaryz,ties.method="first")]

properties2$nearestVisibleTurbine <- substr(maxcolz,13,21) %>% as.numeric()

#bin SIMD
properties2$SIMD_bins <- cut_number(properties2$Overall_SIMD16_rank,10) %>% as.numeric()

#Stare at some sample columns
chk <- properties2 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine)

#Table shows the numbers...
table(properties2$nearestVisibleTurbine,properties2$SIMD_bins)

rankbinVsnearest <- table(properties2$nearestVisibleTurbine,properties2$SIMD_bins) %>% data.frame()

ggplot(rankbinVsnearest, aes(x = Var2, y = Freq, colour = Var1, group = Var1)) +
  geom_line()

#Proportion version?
tbl <- table(properties2$nearestVisibleTurbine,properties2$SIMD_bins)
tbl <- prop.table(tbl,1) * 100

rankbinVsnearest <- tbl %>% data.frame()

ggplot(rankbinVsnearest, aes(x = Var2, y = Freq, colour = Var1, group = Var1)) +
  geom_line()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Next! Break down SIMDs by which properties can / cannot see a turbine at all----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

properties2$anyVisibleTurbine <- ifelse(apply(properties2[,c(33:47)],1,sum) > 0,1,0)

# test <- apply(properties2[,c(33:62)],1,sum)
# test <- data.frame(test)
# test <- test[order(-test$test),]


#Stare at some sample columns
chk <- properties2 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine, anyVisibleTurbine)

#http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
# proportionz <- properties2 %>% group_by(Overall_SIMD16_rank, anyVisibleTurbine) %>% 
#   summarise(count = n())

#I think I need to add a zero in for ones where it's missing. 
#Oh, except I'd also need to add ones in for ones that are all zero. Hmm!
#Will need to do separately and re-join
SIMDsCountAnyVizTurbineIsOne <- properties2[properties2$anyVisibleTurbine == 1,]

summaryAnyVizIsOne <- SIMDsCountAnyVizTurbineIsOne %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount = n())

summarySIMDCount <- properties2 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(countz = n())

#merge together on SIMD number
mrg <- merge(summaryAnyVizIsOne, summarySIMDCount, by = 'Overall_SIMD16_rank', all.y = T)

#Any counts of viz = 1 that are NA, set to zero
mrg$vizOneCount[is.na(mrg$vizOneCount)] <- 0

#Right, finally! Can now work out proportion of properties
#In each SIMD zone that can see at least one turbine
mrg$prop <- (mrg$vizOneCount/mrg$countz)*100

plot(mrg$Overall_SIMD16_rank,mrg$prop)

hist(mrg$Overall_SIMD16_rank[mrg$prop == 0])
hist(mrg$Overall_SIMD16_rank[mrg$prop >= 0 & mrg$prop < 50])
hist(mrg$Overall_SIMD16_rank[mrg$prop > 50 & mrg$prop <= 100])
hist(mrg$Overall_SIMD16_rank[mrg$prop > 30 & mrg$prop <= 70])
hist(mrg$Overall_SIMD16_rank[mrg$prop==100])

#How many is that last one? OK, a lot!
mrg$Overall_SIMD16_rank[mrg$prop==100] %>% length

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat successful SIMD viz/non proportions for building height data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Hmm: I should first-up reduce the sample to those houses that had building height data
#So yes:
#REPEAT FOR TERRAIN BUT USING PROPERTIES THAT DO CROSS BH DATA

#Oops, I lost the crosses-BH indicator. Get it back from master.
crosses_bh <- subset(master, !duplicated(master$title)) %>% dplyr::select(title,crosses_bh)

properties3 <- merge(properties2,crosses_bh,by = 'title', all.x = T)

#All match
table(properties3$crosses_bh, useNA='always') 

#save for later!
saveRDS(properties3,'data/properties3.rds')
properties3 <- readRDS('data/properties3.rds')

#~~~~~~~~~~~~~~
#Some summary stats for Ragne----
#Sample numbers per distance band used in the project

#Just checking... Oh, I'd already dropped ID column
#Reloaded master... yes, single sales are correct (id is above 1000000)
chk <- master %>% filter(title == 'ANG3388')

#So using the repeat sales we used in the project: 2 to 5, I believe?
#Actually, aggregate master, then I can say how many sales vs properties
#Repeat is a keyword, need to use index
rpts <- master %>% filter(repeat_sale > 1 & repeat_sale < 6)

#Keep only useful columns too
rpts <- rpts %>% dplyr::select(title,26:71,crosses_bh)

#OK, so first up: summaries of total sales per distance band
#Only need for "turbines in this band, can vs cannot see"
#But that needs some subtraction
#turb_viz is "can see turbine in this band" (for both terrain and building height)
#So "there is a turbine but cannot see it"...

#What were the bands again?
#0-2/2-3/3-4/4-5/5-8/8-15 (though it says 8 to 14 in report)

#You know, maybe don't give so much. The main point is the number of sales/properties in each band.
#Let's just do that for those bands.
rpts$range0to2 <- ifelse(apply(rpts[,c(2:3)],1,sum) > 0,1,0)
rpts$range2to3 <- rpts[,4]
rpts$range3to4 <- rpts[,5]
rpts$range4to5 <- rpts[,6]
rpts$range5to8 <- ifelse(apply(rpts[,c(7:9)],1,sum) > 0,1,0)
rpts$range8to15 <- ifelse(apply(rpts[,c(10:16)],1,sum) > 0,1,0)
rpts$totals <- 1#count of all properties. Coz all set to one, it'll count them all along with the bands

#How many sales per band?
#salesPerBand <- rpts %>% dplyr::select(range0to2:range8to15) %>% 
salesPerBand <- rpts %>% dplyr::select(range0to2:totals) %>% 
  summarise_each(funs(sum(.!=0)))

#How many properties per band?
propertiez <- rpts %>% distinct(title)

propertiesPerBand <- propertiez %>% dplyr::select(range0to2:totals) %>% 
  summarise_each(funs(sum(.!=0)))

#repeat for the building height result
salesPerBand_BH <- rpts %>% filter(crosses_bh == 1) %>% 
  dplyr::select(range0to2:totals) %>% 
  summarise_each(funs(sum(.!=0)))

#How many properties per band?
propertiez_bh <- rpts %>% filter(crosses_bh == 1) %>% 
  distinct(title)

propertiesPerBand_BH <- propertiez_bh %>% dplyr::select(range0to2:totals) %>% 
  summarise_each(funs(sum(.!=0)))

#Stick em all together with labels and save
salesPerBand$source <- 'sales per band (terrain)'
propertiesPerBand$source <- 'properties per band (terrain)'
salesPerBand_BH$source <- 'sales per band (building heights)'
propertiesPerBand_BH$source <- 'properties per band (building heights)'

countz <- do.call(rbind, list(salesPerBand,propertiesPerBand,salesPerBand_BH,propertiesPerBand_BH))

countz <- countz %>% dplyr::select(source,1:7)

#save!
write.csv(countz,'data/sampleSizes_windfarmProject.csv', row.names = F)

#~~~~~~~~~~~~~~
#Save a version for QGIS looking. Summarise in bands----
vizQGISsave <- properties3 %>% dplyr::select(title,eastingsfinal,northingsfinal,turb0:turb_vis_bldg14)

#Mark "can see" for terrain and buildings for three discs (not bands)
#DOn't worry about reducing BH: can mark in map with actual BH data footprints
#I've done this somewhere below haven't I? Err, no, not for each property (I don't think)


vizQGISsave$range0to5 <- ifelse(apply(vizQGISsave[,c(4:8)],1,sum) > 0,1,0)
vizQGISsave$range5to10 <- ifelse(apply(vizQGISsave[,c(9:13)],1,sum) > 0,1,0)
vizQGISsave$range10to15 <- ifelse(apply(vizQGISsave[,c(14:18)],1,sum) > 0,1,0)

vizQGISsave$visTerrain0to5 <- ifelse(apply(vizQGISsave[,c(19:23)],1,sum) > 0,1,0)
vizQGISsave$visTerrain0to10 <- ifelse(apply(vizQGISsave[,c(19:28)],1,sum) > 0,1,0)
vizQGISsave$visTerrain0to15 <- ifelse(apply(vizQGISsave[,c(19:33)],1,sum) > 0,1,0)

vizQGISsave$visBuildingHeights0to5 <- ifelse(apply(vizQGISsave[,c(34:38)],1,sum) > 0,1,0)
vizQGISsave$visBuildingHeights0to10 <- ifelse(apply(vizQGISsave[,c(34:43)],1,sum) > 0,1,0)
vizQGISsave$visBuildingHeights0to15 <- ifelse(apply(vizQGISsave[,c(34:48)],1,sum) > 0,1,0)

#save
write.csv(vizQGISsave %>% dplyr::select(1:3,49:57),'file:///C:/Data/WindFarmViewShed/QGIS/R_saves/properties_vizDiscs.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Probability approach to linking viz and SIMD-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SO the first job, again, is to table up

#~~~~~~~~~~~~~~~~
#Other section that I've lost track of. What's this doing again?----
#~~~~~~~~~~~~~~~~


#keep only those with crosses_bh, check the above again
#No, don't! Don't I want properties3 to remain all?
# properties3 <- properties3[properties3$crosses_bh == 1,]

#~~~~~~~~

properties3$anyVisibleTurbine <- ifelse(apply(properties3[,c(33:47)],1,sum) > 0,1,0)

#Stare at some sample columns
chk <- properties3 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine, anyVisibleTurbine)

#http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
# proportionz <- properties3 %>% group_by(Overall_SIMD16_rank, anyVisibleTurbine) %>% 
#   summarise(count = n())

#I think I need to add a zero in for ones where it's missing. 
#Oh, except I'd also need to add ones in for ones that are all zero. Hmm!
#Will need to do separately and re-join
SIMDsCountAnyVizTurbineIsOne <- properties3[properties3$anyVisibleTurbine == 1,]

summaryAnyVizIsOne <- SIMDsCountAnyVizTurbineIsOne %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount = n())

summarySIMDCount <- properties3 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(countz = n())

#merge together on SIMD number
mrg2 <- merge(summaryAnyVizIsOne, summarySIMDCount, by = 'Overall_SIMD16_rank', all.y = T)

#Any counts of viz = 1 that are NA, set to zero
mrg2$vizOneCount[is.na(mrg2$vizOneCount)] <- 0

#Right, finally! Can now work out proportion of properties
#In each SIMD zone that can see at least one turbine
mrg2$prop <- (mrg2$vizOneCount/mrg2$countz)*100

plot(mrg2$Overall_SIMD16_rank,mrg2$prop)

hist(mrg2$Overall_SIMD16_rank[mrg2$prop == 0])
hist(mrg2$Overall_SIMD16_rank[mrg2$prop >= 0 & mrg2$prop < 50])
hist(mrg2$Overall_SIMD16_rank[mrg2$prop > 50 & mrg2$prop <= 100])
hist(mrg2$Overall_SIMD16_rank[mrg2$prop > 30 & mrg2$prop <= 70])
hist(mrg2$Overall_SIMD16_rank[mrg2$prop > 80 & mrg2$prop <= 100])
hist(mrg2$Overall_SIMD16_rank[mrg2$prop==100])

#Yes, still there. Possibly even stronger.
#Though less so in the better off areas

#NOOO! All wrong. See below.

#~~~~~~~~~~~~~~~~
#REPEAT FOR BUILDING HEIGHT LINE OF SIGHT----
#~~~~~~~~~~~~~~~~

properties3$BH_anyVisibleTurbine <- ifelse(apply(properties3[,c(48:62)],1,sum) > 0,1,0)

#Stare at some sample columns
chk <- properties3 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine, 
                anyVisibleTurbine,
                BH_anyVisibleTurbine)

#Keerect
table(properties3$anyVisibleTurbine)
table(properties3$BH_anyVisibleTurbine)

#I think I need to add a zero in for ones where it's missing. 
#Oh, except I'd also need to add ones in for ones that are all zero. Hmm!
#Will need to do separately and re-join
SIMDsCountAnyVizTurbineIsOne <- properties3[properties3$BH_anyVisibleTurbine == 1,]

summaryAnyVizIsOne <- SIMDsCountAnyVizTurbineIsOne %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount = n())

summarySIMDCount <- properties3 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(countz = n())

#merge together on SIMD number
mrg3 <- merge(summaryAnyVizIsOne, summarySIMDCount, by = 'Overall_SIMD16_rank', all.y = T)

#Any counts of viz = 1 that are NA, set to zero
mrg3$vizOneCount[is.na(mrg3$vizOneCount)] <- 0

#Right, finally! Can now work out proportion of properties
#In each SIMD zone that can see at least one turbine
mrg3$prop <- (mrg3$vizOneCount/mrg3$countz)*100

plot(mrg3$Overall_SIMD16_rank,mrg3$prop)

hist(mrg3$Overall_SIMD16_rank[mrg3$prop == 0])
hist(mrg3$Overall_SIMD16_rank[mrg3$prop >= 0 & mrg3$prop < 50])
hist(mrg3$Overall_SIMD16_rank[mrg3$prop > 50 & mrg3$prop <= 100])
hist(mrg3$Overall_SIMD16_rank[mrg3$prop > 30 & mrg3$prop <= 70])
hist(mrg3$Overall_SIMD16_rank[mrg3$prop > 80 & mrg3$prop <= 100])
hist(mrg3$Overall_SIMD16_rank[mrg3$prop==100])

#Hang on...
hist(mrg2$Overall_SIMD16_rank)
hist(mrg3$Overall_SIMD16_rank)

#Doh. So have to know difference to original counts. Luckily...
#http://stackoverflow.com/questions/7740503/getting-frequency-values-from-histogram-in-r
#So yes, they're all there still.
(unique(mrg3$Overall_SIMD16_rank) %in% unique(properties3$Overall_SIMD16_rank)) %>% table

#Keep hist breaks the same so the counts can be compared
breakz <- c(seq(from = 0, to = 7000, by = 500))

orighist <- hist(mrg3$Overall_SIMD16_rank, breaks = breakz)

comphist <- hist(mrg3$Overall_SIMD16_rank[mrg3$prop==100],
                 breaks = breakz)
#BH
comphist <- hist(mrg3$Overall_SIMD16_rank[mrg3$prop > 80 & mrg3$prop <= 100],
                 breaks = breakz)
#Terrain
comphist <- hist(mrg2$Overall_SIMD16_rank[mrg2$prop > 80 & mrg2$prop <= 100],
                 breaks = breakz)

orighist$counts
comphist$counts

histprops <- (comphist$counts/orighist$counts)*100

barplot(histprops, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#REPEAT FOR 3 DISTANCE BINS FOR TERRAIN FIRST----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

properties3$anyVisibleTurbine0to5km <- ifelse(apply(properties3[,c(33:37)],1,sum) > 0,1,0)
properties3$anyVisibleTurbine5to10km <- ifelse(apply(properties3[,c(38:42)],1,sum) > 0,1,0)
properties3$anyVisibleTurbine10to15km <- ifelse(apply(properties3[,c(43:47)],1,sum) > 0,1,0)

chk <- properties3 %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine, 
                anyVisibleTurbine,
                BH_anyVisibleTurbine,
                anyVisibleTurbine0to5km,anyVisibleTurbine5to10km,anyVisibleTurbine10to15km)

#do separately and re-join
#Should probably function this up, but...
SIMDsCountAnyVizTurbineIsOne0to5 <- properties3[properties3$anyVisibleTurbine0to5km == 1,]
SIMDsCountAnyVizTurbineIsOne5to10 <- properties3[properties3$anyVisibleTurbine5to10km == 1,]
SIMDsCountAnyVizTurbineIsOne10to15 <- properties3[properties3$anyVisibleTurbine10to15km == 1,]

#Only need this once
summarySIMDCount <- properties3 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(countz = n())

summaryAnyVizIsOne0to5 <- SIMDsCountAnyVizTurbineIsOne0to5 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount0to5 = n())
summaryAnyVizIsOne5to10 <- SIMDsCountAnyVizTurbineIsOne5to10 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount5to10 = n())
summaryAnyVizIsOne10to15 <- SIMDsCountAnyVizTurbineIsOne10to15 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount10to15 = n())

#merge together on SIMD number
mrgterrain <- merge(summaryAnyVizIsOne0to5, summarySIMDCount, by = 'Overall_SIMD16_rank', all.y = T)
mrgterrain$vizOneCount0to5[is.na(mrgterrain$vizOneCount0to5)] <- 0
mrgterrain$prop0to5 <- (mrgterrain$vizOneCount0to5/mrgterrain$countz)*100

mrgterrain <- merge(summaryAnyVizIsOne5to10, mrgterrain, by = 'Overall_SIMD16_rank', all.y = T)
mrgterrain$vizOneCount5to10[is.na(mrgterrain$vizOneCount5to10)] <- 0
mrgterrain$prop5to10 <- (mrgterrain$vizOneCount5to10/mrgterrain$countz)*100

mrgterrain <- merge(summaryAnyVizIsOne10to15, mrgterrain, by = 'Overall_SIMD16_rank', all.y = T)
mrgterrain$vizOneCount10to15[is.na(mrgterrain$vizOneCount10to15)] <- 0
mrgterrain$prop10to15 <- (mrgterrain$vizOneCount10to15/mrgterrain$countz)*100

#~~~~~~~~~
#OK, presuming that worked...
breakz <- c(seq(from = 0, to = 7000, by = 500))

orighist <- hist(mrgterrain$Overall_SIMD16_rank, breaks = breakz)

#Terrain
comphist0to5 <- hist(mrgterrain$Overall_SIMD16_rank[mrgterrain$prop0to5 > 80 & mrgterrain$prop0to5 <= 100],
                     breaks = breakz)
comphist5to10 <- hist(mrgterrain$Overall_SIMD16_rank[mrgterrain$prop5to10 > 80 & mrgterrain$prop5to10 <= 100],
                      breaks = breakz)
comphist10to15 <- hist(mrgterrain$Overall_SIMD16_rank[mrgterrain$prop10to15 > 80 & mrgterrain$prop10to15 <= 100],
                       breaks = breakz)

comphist0to5 <- hist(mrgterrain$Overall_SIMD16_rank[mrgterrain$prop0to5 >= 0 & mrgterrain$prop0to5 <= 20],
                     breaks = breakz)
comphist5to10 <- hist(mrgterrain$Overall_SIMD16_rank[mrgterrain$prop5to10 >= 0 & mrgterrain$prop5to10 <= 20],
                      breaks = breakz)
comphist10to15 <- hist(mrgterrain$Overall_SIMD16_rank[mrgterrain$prop10to15 >= 0 & mrgterrain$prop10to15 <= 20],
                       breaks = breakz)

#orighist$counts
#comphist$counts
histprops0to5 <- (comphist0to5$counts/orighist$counts)*100
histprops5to10 <- (comphist5to10$counts/orighist$counts)*100
histprops10to15 <- (comphist10to15$counts/orighist$counts)*100

#Terrain dist bin bars... 
ylabz <- 'proportion in this bin with 0-20% visibility'
ylabz <- 'proportion in this bin with 80-100% visibility'

barplot(histprops0to5, xlab = 'SIMD bin, low to high', ylab = ylabz)
barplot(histprops5to10, xlab = 'SIMD bin, low to high', ylab = ylabz)
barplot(histprops10to15, xlab = 'SIMD bin, low to high', ylab = ylabz)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 DISTANCE BINS FOR TERRAIN BUT CROSSES-BUILDING-HEIGHT SAMPLE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

properties_crossesBH <- properties3 %>% filter(crosses_bh == 1)

properties_crossesBH$anyVisibleTurbine0to5km <- ifelse(apply(properties_crossesBH[,c(33:37)],1,sum) > 0,1,0)
properties_crossesBH$anyVisibleTurbine5to10km <- ifelse(apply(properties_crossesBH[,c(38:42)],1,sum) > 0,1,0)
properties_crossesBH$anyVisibleTurbine10to15km <- ifelse(apply(properties_crossesBH[,c(43:47)],1,sum) > 0,1,0)

chk <- properties_crossesBH %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine, 
                anyVisibleTurbine,
                BH_anyVisibleTurbine,
                anyVisibleTurbine0to5km,anyVisibleTurbine5to10km,anyVisibleTurbine10to15km)

#do separately and re-join
#Should probably function this up, but...
SIMDsCountAnyVizTurbineIsOne0to5 <- properties_crossesBH[properties_crossesBH$anyVisibleTurbine0to5km == 1,]
SIMDsCountAnyVizTurbineIsOne5to10 <- properties_crossesBH[properties_crossesBH$anyVisibleTurbine5to10km == 1,]
SIMDsCountAnyVizTurbineIsOne10to15 <- properties_crossesBH[properties_crossesBH$anyVisibleTurbine10to15km == 1,]

#Only need this once
summarySIMDCount <- properties_crossesBH %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(countz = n())

summaryAnyVizIsOne0to5 <- SIMDsCountAnyVizTurbineIsOne0to5 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount0to5 = n())
summaryAnyVizIsOne5to10 <- SIMDsCountAnyVizTurbineIsOne5to10 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount5to10 = n())
summaryAnyVizIsOne10to15 <- SIMDsCountAnyVizTurbineIsOne10to15 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount10to15 = n())

#merge together on SIMD number
mrgx <- merge(summaryAnyVizIsOne0to5, summarySIMDCount, by = 'Overall_SIMD16_rank', all.y = T)
mrgx$vizOneCount0to5[is.na(mrgx$vizOneCount0to5)] <- 0
mrgx$prop0to5 <- (mrgx$vizOneCount0to5/mrgx$countz)*100

mrgx <- merge(summaryAnyVizIsOne5to10, mrgx, by = 'Overall_SIMD16_rank', all.y = T)
mrgx$vizOneCount5to10[is.na(mrgx$vizOneCount5to10)] <- 0
mrgx$prop5to10 <- (mrgx$vizOneCount5to10/mrgx$countz)*100

mrgx <- merge(summaryAnyVizIsOne10to15, mrgx, by = 'Overall_SIMD16_rank', all.y = T)
mrgx$vizOneCount10to15[is.na(mrgx$vizOneCount10to15)] <- 0
mrgx$prop10to15 <- (mrgx$vizOneCount10to15/mrgx$countz)*100

#~~~~~~~~~
#OK, presuming that worked...
breakz <- c(seq(from = 0, to = 7000, by = 500))

orighist <- hist(mrgx$Overall_SIMD16_rank, breaks = breakz)

#Terrain
comphist0to5 <- hist(mrgx$Overall_SIMD16_rank[mrgx$prop0to5 > 80 & mrgx$prop0to5 <= 100],
                 breaks = breakz)
comphist5to10 <- hist(mrgx$Overall_SIMD16_rank[mrgx$prop5to10 > 80 & mrgx$prop5to10 <= 100],
                 breaks = breakz)
comphist10to15 <- hist(mrgx$Overall_SIMD16_rank[mrgx$prop10to15 > 80 & mrgx$prop10to15 <= 100],
                 breaks = breakz)

comphist0to5 <- hist(mrgx$Overall_SIMD16_rank[mrgx$prop0to5 >= 0 & mrgx$prop0to5 <= 20],
                 breaks = breakz)
comphist5to10 <- hist(mrgx$Overall_SIMD16_rank[mrgx$prop5to10 >= 0 & mrgx$prop5to10 <= 20],
                 breaks = breakz)
comphist10to15 <- hist(mrgx$Overall_SIMD16_rank[mrgx$prop10to15 >= 0 & mrgx$prop10to15 <= 20],
                 breaks = breakz)

#orighist$counts
#comphist$counts
histprops0to5 <- (comphist0to5$counts/orighist$counts)*100
histprops5to10 <- (comphist5to10$counts/orighist$counts)*100
histprops10to15 <- (comphist10to15$counts/orighist$counts)*100

#Terrain dist bin bars... 
barplot(histprops0to5, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')
barplot(histprops5to10, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')
barplot(histprops10to15, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 DISTANCE BINS FOR BUILDING HEIGHT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

properties_crossesBH$any_BH_VisibleTurbine0to5km <- ifelse(apply(properties_crossesBH[,c(48:52)],1,sum) > 0,1,0)
properties_crossesBH$any_BH_VisibleTurbine5to10km <- ifelse(apply(properties_crossesBH[,c(53:57)],1,sum) > 0,1,0)
properties_crossesBH$any_BH_VisibleTurbine10to15km <- ifelse(apply(properties_crossesBH[,c(58:62)],1,sum) > 0,1,0)

chk <- properties_crossesBH %>% 
  dplyr::select(title, turb0:turb14, 
                turb_vis_trn0:turb_vis_trn14, modalDistanceBand, 
                Overall_SIMD16_rank, SIMD_bins, nearestTurbine, nearestVisibleTurbine, 
                anyVisibleTurbine,
                BH_anyVisibleTurbine,
                anyVisibleTurbine0to5km,anyVisibleTurbine5to10km,anyVisibleTurbine10to15km,
                any_BH_VisibleTurbine0to5km,any_BH_VisibleTurbine5to10km,any_BH_VisibleTurbine10to15km)

#do separately and re-join
#Should probably function this up, but...
SIMDsCountAnyVizTurbineIsOne0to5 <- properties_crossesBH[properties_crossesBH$any_BH_VisibleTurbine0to5km == 1,]
SIMDsCountAnyVizTurbineIsOne5to10 <- properties_crossesBH[properties_crossesBH$any_BH_VisibleTurbine5to10km == 1,]
SIMDsCountAnyVizTurbineIsOne10to15 <- properties_crossesBH[properties_crossesBH$any_BH_VisibleTurbine10to15km == 1,]

#Only need this once
summarySIMDCount <- properties_crossesBH %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(countz = n())

summaryAnyVizIsOne0to5 <- SIMDsCountAnyVizTurbineIsOne0to5 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount0to5 = n())
summaryAnyVizIsOne5to10 <- SIMDsCountAnyVizTurbineIsOne5to10 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount5to10 = n())
summaryAnyVizIsOne10to15 <- SIMDsCountAnyVizTurbineIsOne10to15 %>% group_by(Overall_SIMD16_rank) %>% 
  summarise(vizOneCount10to15 = n())

#merge together on SIMD number
mrgz <- merge(summaryAnyVizIsOne0to5, summarySIMDCount, by = 'Overall_SIMD16_rank', all.y = T)
mrgz$vizOneCount0to5[is.na(mrgz$vizOneCount0to5)] <- 0
mrgz$prop0to5 <- (mrgz$vizOneCount0to5/mrgz$countz)*100

mrgz <- merge(summaryAnyVizIsOne5to10, mrgz, by = 'Overall_SIMD16_rank', all.y = T)
mrgz$vizOneCount5to10[is.na(mrgz$vizOneCount5to10)] <- 0
mrgz$prop5to10 <- (mrgz$vizOneCount5to10/mrgz$countz)*100

mrgz <- merge(summaryAnyVizIsOne10to15, mrgz, by = 'Overall_SIMD16_rank', all.y = T)
mrgz$vizOneCount10to15[is.na(mrgz$vizOneCount10to15)] <- 0
mrgz$prop10to15 <- (mrgz$vizOneCount10to15/mrgz$countz)*100

#~~~~~~~~~
#OK, presuming that worked...
breakz <- c(seq(from = 0, to = 7000, by = 500))

orighist <- hist(mrgz$Overall_SIMD16_rank, breaks = breakz)

#Terrain
comphist0to5_BH <- hist(mrgz$Overall_SIMD16_rank[mrgz$prop0to5 > 80 & mrgz$prop0to5 <= 100],
                     breaks = breakz)
comphist5to10_BH <- hist(mrgz$Overall_SIMD16_rank[mrgz$prop5to10 > 80 & mrgz$prop5to10 <= 100],
                      breaks = breakz)
comphist10to15_BH <- hist(mrgz$Overall_SIMD16_rank[mrgz$prop10to15 > 80 & mrgz$prop10to15 <= 100],
                       breaks = breakz)

comphist0to5_BH <- hist(mrgz$Overall_SIMD16_rank[mrgz$prop0to5 >= 0 & mrgz$prop0to5 <= 10],
                     breaks = breakz)
comphist5to10_BH <- hist(mrgz$Overall_SIMD16_rank[mrgz$prop5to10 >= 0 & mrgz$prop5to10 <= 10],
                      breaks = breakz)
comphist10to15_BH <- hist(mrgz$Overall_SIMD16_rank[mrgz$prop10to15 >= 0 & mrgz$prop10to15 <= 10],
                       breaks = breakz)

#orighist$counts
#comphist$counts
histprops0to5_BH <- (comphist0to5_BH$counts/orighist$counts)*100
histprops5to10_BH <- (comphist5to10_BH$counts/orighist$counts)*100
histprops10to15_BH <- (comphist10to15_BH$counts/orighist$counts)*100

#Terrain dist bin bars... 
barplot(histprops0to5_BH, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')
barplot(histprops5to10_BH, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')
barplot(histprops10to15_BH, xlab = 'SIMD bin, low to high', ylab = 'proportion in this bin with 80-100% visibility')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Av Prices in diff dist bands in diff council areas----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Loaded master above. Reduce to sensible range of columns
#Not using any time info just at the moment
master <- master %>% dplyr::select(-(id_house:ln_dist_coast_km),
                                   -(turb_vis_trn_after0:turb_vis_bldg_after14)
                                   -(ur8_indicator:ur2_indicator))

#Actually, to start with, just a count of numbers of properties with at least one turbine
#In distance bands would be a start.
#And not numbers: proportions given CA house number.
#So I can use properties3 again...

#check. Yup, all properties accounted for.
table(properties3$councilarea, useNA='always')

#Blimey, that worked...
terrainCountperCA <- properties3 %>% dplyr::select(turb_vis_trn0:turb_vis_trn14,councilarea) %>% 
  group_by(councilarea) %>% 
#  summarise_each(funs(sum(.!=0)/n()))
  summarise_each(funs(sum(.!=0)))

apply(terrainCountperCA[,c(2:16)],1,sum)

#working out if that worked. What's going on with the first CA?
chk <- properties3 %>% filter(councilarea == 'S12000005')
chk2 <- properties3 %>% filter(councilarea == 'S12000018')

#We are working with only properties within 15km of a turbine, right?
withinRangeCountperCA <- properties3 %>% dplyr::select(turb0:turb14,councilarea) %>% 
  group_by(councilarea) %>% 
  #  summarise_each(funs(sum(.!=0)/n()))
  summarise_each(funs(sum(.!=0)))

#Yup, think so. One of the council areas only has 19 properties...
apply(withinRangeCountperCA[,c(2:16)],1,sum)

#Might stick to the ones with the larger numbers.
#So just checking proportions are working...
#Yes it is. So make out of 100%...
withinRangeCountperCAprops <- properties3 %>% dplyr::select(turb0:turb14,councilarea) %>% 
  group_by(councilarea) %>% 
  summarise_each(funs( (sum(.!=0)/n())*100 ))
  #summarise_each(funs(sum(.!=0)))

#Only use CAs with enough properties to make sense.
#Oh and get their names... 
#Actually, might use NUTS3. Can I get the name plz?
nuts3 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/NUTS3_2008", 
                                layer="scotland_nuts3_2008")

nuts3 <- data.frame(nuts3)
(nuts3$code %in% properties3$nuts3) %>% table

#Err. Hmm, no properties in one of them. Oh well!
unique(nuts3$code)
unique(properties3$nuts3)

#Shouldn't change size...
properties3 <- merge(properties3,nuts3[,c('code','name')],by.x='nuts3',by.y='code')
properties_crossesBH <- merge(properties_crossesBH,nuts3[,c('code','name')],by.x='nuts3',by.y='code')

unique(properties3$name)
unique(properties_crossesBH$name)

#What's the property count for each NUTS3 region?
#Small numbers in the islands. Might drop those.
#UKM64 and 65
countz <- properties3 %>% group_by(nuts3) %>% 
  summarise(n())

countz_bh <- properties_crossesBH %>% group_by(nuts3) %>% 
  summarise(n())

#For BH, drop more...
#UKM64, 65, 24, 37

#TERRAIN VS BH VS RANGE----

#rangeCountperNUTS3props <- properties3 %>% filter(!(nuts3 %in% c('UKM64','UKM65'))) %>% 
rangeCountperNUTS3props <- properties_crossesBH %>% filter(!(nuts3 %in% c('UKM64','UKM65','UKM24','UKM37'))) %>% 
  dplyr::select(turb0:turb14,name) %>% 
  group_by(name) %>% 
  summarise_each(funs( (sum(.!=0)/n())*100 ))

#terrainCountperNUTS3props <- properties3 %>% filter(!(nuts3 %in% c('UKM64','UKM65'))) %>% 
terrainCountperNUTS3props <- properties_crossesBH %>% filter(!(nuts3 %in% c('UKM64','UKM65','UKM24','UKM37'))) %>% 
  dplyr::select(turb_vis_trn0:turb_vis_trn14,name) %>% 
  group_by(name) %>% 
  summarise_each(funs( (sum(.!=0)/n())*100 ))

#BH_CountperNUTS3props <- properties3 %>% filter(!(nuts3 %in% c('UKM64','UKM65'))) %>% 
BH_CountperNUTS3props <- properties_crossesBH %>% filter(!(nuts3 %in% c('UKM64','UKM65','UKM24','UKM37'))) %>% 
  dplyr::select(turb_vis_bldg0:turb_vis_bldg14,name) %>% 
  group_by(name) %>% 
  summarise_each(funs( (sum(.!=0)/n())*100 ))

#Combine those so I can produce overlapped bars.
#Longamanate them and label
rangeLong <- gather(rangeCountperNUTS3props,name,percent)
terrainLong <- gather(terrainCountperNUTS3props,name,percent)
BH_Long <- gather(BH_CountperNUTS3props,name,percent)

names(rangeLong) <- c('NUTS3','dist','percent')
names(terrainLong) <- c('NUTS3','dist','percent')
names(BH_Long) <- c('NUTS3','dist','percent')

rangeLong$source = 'range'
terrainLong$source = 'terrain'
BH_Long$source = 'building height'

#Join
nuts3_distBands <- do.call(rbind,list(rangeLong,terrainLong,BH_Long))

#Make dist band values the same
nuts3_distBands$dist <- gsub("[^0-9]", "", nuts3_distBands$dist)

subz <- nuts3_distBands[nuts3_distBands$NUTS3=='Glasgow City',]

#Plot? Yeah, maybe!
output <- ggplot(subz, 
  aes(x = as.numeric(dist), y = percent, fill = source)) + 
  geom_bar(stat = 'identity', position = "dodge")

output

#output all for a quick look
for(nuts3z in unique(nuts3_distBands$NUTS3)){

  subz <- nuts3_distBands[nuts3_distBands$NUTS3==nuts3z,]
  
  #Plot? Yeah, maybe!
  output <- ggplot(subz, 
    aes(x = as.numeric(dist), y = percent, fill = source)) + 
    geom_bar(stat = 'identity', position = "dodge") +
    coord_cartesian(ylim = c(0,100)) +
    xlab('distance to turbine/s (km)') +
    ggtitle(nuts3z) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  ggsave(paste0("saves/nuts3_vizBandBars_BH/",nuts3z,".png"), output,dpi=150, width = 7, height = 5)

}

#TERRAIN VS RANGE----
rangeCountperNUTS3props <- properties3 %>% filter(!(nuts3 %in% c('UKM64','UKM65'))) %>% 
#rangeCountperNUTS3props <- properties_crossesBH %>% filter(!(nuts3 %in% c('UKM64','UKM65','UKM24','UKM37'))) %>% 
  dplyr::select(turb0:turb14,name) %>% 
  group_by(name) %>% 
  summarise_each(funs( (sum(.!=0)/n())*100 ))

terrainCountperNUTS3props <- properties3 %>% filter(!(nuts3 %in% c('UKM64','UKM65'))) %>% 
#terrainCountperNUTS3props <- properties_crossesBH %>% filter(!(nuts3 %in% c('UKM64','UKM65','UKM24','UKM37'))) %>% 
  dplyr::select(turb_vis_trn0:turb_vis_trn14,name) %>% 
  group_by(name) %>% 
  summarise_each(funs( (sum(.!=0)/n())*100 ))

#Combine those so I can produce overlapped bars.
#Longamanate them and label
rangeLong <- gather(rangeCountperNUTS3props,name,percent)
terrainLong <- gather(terrainCountperNUTS3props,name,percent)

names(rangeLong) <- c('NUTS3','dist','percent')
names(terrainLong) <- c('NUTS3','dist','percent')

rangeLong$source = 'range'
terrainLong$source = 'terrain'

#Join
nuts3_distBands <- do.call(rbind,list(rangeLong,terrainLong))

#Make dist band values the same
nuts3_distBands$dist <- gsub("[^0-9]", "", nuts3_distBands$dist)

subz <- nuts3_distBands[nuts3_distBands$NUTS3=='Glasgow City',]

#Plot? Yeah, maybe!
output <- ggplot(subz, 
                 aes(x = as.numeric(dist), y = percent, fill = source)) + 
  geom_bar(stat = 'identity', position = "dodge")

output

#output all for a quick look
for(nuts3z in unique(nuts3_distBands$NUTS3)){
  
  subz <- nuts3_distBands[nuts3_distBands$NUTS3==nuts3z,]
  
  output <- ggplot(subz, 
                   aes(x = as.numeric(dist), y = percent, fill = source)) + 
    geom_bar(stat = 'identity', position = "dodge") +
    coord_cartesian(ylim = c(0,100)) +
    xlab('distance to turbine/s (km)') +
    ggtitle(nuts3z) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  output
  
  ggsave(paste0("saves/nuts3_vizBandBars_terrain/",nuts3z,".png"), output,dpi=150, width = 7, height = 5)
  
}









