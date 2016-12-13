#Some more checks on various bits of data
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
library(dplyr)

geolibs <- c("pryr","stringr","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1. Look at properties in NUTS3 zones that are producing huge price swings----

#We already have files for properties in the difference distance bands.
#But I do need to add an %in% check for the appropriate NUTS3 zones.
#And would be worth checking that does actually match those being used in Stata.
hs_turbDistBANDS <- readRDS("data/hses_plus_turbinesWithinAllDistanceBANDS_upTo15000.rds")

names(hs_turbDistBANDS)[2:ncol(hs_turbDistBANDS)] <- 
  sapply(seq(1:(ncol(hs_turbDistBANDS)-1)), function(x) paste0("tbs_",x-1,"to",x,"km"))

#sales_plus_turbBANDS <- readRDS("data/temp/sales_plus_turbBANDS.rds")

#counts in each band: binary, "at least one turbine in this band"
#distanceBANDSCounts <- hs_turbDistBANDS[,c(2:16)] %>% lapply(function(x) table(x!="")[[2]]) %>% unlist
hs_turbDistBANDS_counts <- hs_turbDistBANDS
hs_turbDistBANDS_counts[2:16] <- apply(hs_turbDistBANDS_counts[2:6],2,function(x) 0 + (x!=""))

#column for 0 to 2
hs_turbDistBANDS_counts$tbs_0to2km <- bitwOr(hs_turbDistBANDS_counts$tbs_0to1km,
                                             hs_turbDistBANDS_counts$tbs_1to2km)

#find one with both, just to check
#hs_turbDistBANDS_counts$Title[hs_turbDistBANDS_counts$tbs_0to1km + hs_turbDistBANDS_counts$tbs_1to2km==2]
hs_turbDistBANDS_counts$tbs_0to2km[hs_turbDistBANDS_counts$tbs_0to1km + hs_turbDistBANDS_counts$tbs_1to2km==2]

#and for 0 to 3
hs_turbDistBANDS_counts$tbs_0to3km <- bitwOr(bitwOr(hs_turbDistBANDS_counts$tbs_0to1km,
                                             hs_turbDistBANDS_counts$tbs_1to2km),
                                             hs_turbDistBANDS_counts$tbs_2to3km)

hs_turbDistBANDS_counts$tbs_0to4km <- bitwOr(bitwOr(bitwOr(hs_turbDistBANDS_counts$tbs_0to1km,
                                             hs_turbDistBANDS_counts$tbs_1to2km),
                                             hs_turbDistBANDS_counts$tbs_2to3km),
                                             hs_turbDistBANDS_counts$tbs_3to4km
)

#find one with both, just to check
#hs_turbDistBANDS_counts$Title[hs_turbDistBANDS_counts$tbs_0to1km + hs_turbDistBANDS_counts$tbs_1to2km==2]
hs_turbDistBANDS_counts$tbs_0to2km[hs_turbDistBANDS_counts$tbs_0to1km + hs_turbDistBANDS_counts$tbs_1to2km==2]
hs_turbDistBANDS_counts$tbs_0to3km[hs_turbDistBANDS_counts$tbs_0to1km + 
                                     hs_turbDistBANDS_counts$tbs_1to2km +
                                     hs_turbDistBANDS_counts$tbs_2to3km ==3]
hs_turbDistBANDS_counts$tbs_0to4km[hs_turbDistBANDS_counts$tbs_0to1km + 
                                     hs_turbDistBANDS_counts$tbs_1to2km +
                                     hs_turbDistBANDS_counts$tbs_2to3km +
                                     hs_turbDistBANDS_counts$tbs_3to4km ==4]


#Or actually, I could just use the master datasheet. That way, defo same regions.
#OK, exported from stata
#This is only 2 to 5 repeat sales
master_NUTS3 <- read.csv("data/dta_combined_NUTS3only.csv")

#So which ones are weird...?
#If they've been labelled correctly, it's:
#Inverness; Lochaber; Inverclyde; ...
#UKM62; UKM63; UKM35

#Merge in the marker for those with a turbine within 0-2km
master_NUTS3merge <- merge(master_NUTS3, 
                           hs_turbDistBANDS_counts[,c('Title','tbs_0to2km','tbs_0to3km','tbs_0to4km')],
                           by.x = 'title', by.y = 'Title', all.x = T)

#Look at only those weird NUTS3s...
master_NUTS3mergeLook <- master_NUTS3merge %>% filter(nuts3 %in% c('UKM62','UKM63','UKM35'))

#mean prices in the two distance bands (0-2 and rest)?
ggplot(master_NUTS3mergeLook, aes(x = factor(tbs_0to4km), y = pricefinal)) +
  geom_boxplot() 

#Count? Oh, that's sales. Wait.
table(master_NUTS3mergeLook$tbs_0to2km)

unique(master_NUTS3mergeLook$title[master_NUTS3mergeLook$tbs_0to2km==1]) %>% length
#And for 0-3?
unique(master_NUTS3mergeLook$title[master_NUTS3mergeLook$tbs_0to3km==1]) %>% length
#And for 0-4?
unique(master_NUTS3mergeLook$title[master_NUTS3mergeLook$tbs_0to4km==1]) %>% length

#OK, so raw prices are much higher. But why would that imply large price change?
#Can't plot over time...

#But can do standard deviation for each property's prices. I should already have filtered for this
#So shouldn't pick anything up.

#Let's look at prices a little more closely.
#If we log prices, then find the SD per property, that should give an indication of 
#the range of price change
#Could be very different dates - but really odd ones will stand out.
master_NUTS3mergeLook$lnPrice <- log(master_NUTS3mergeLook$pricefinal)

sds_pricePerProperty <- master_NUTS3mergeLook %>% group_by(title) %>% 
  summarise(mean = mean(lnPrice), sd = sd(lnPrice),
            tbs_0to2km = max(tbs_0to2km),
            tbs_0to3km = max(tbs_0to3km),
            tbs_0to4km = max(tbs_0to4km)
            )

#factor up 0-2, 0-3, 0-4 discs
#Take advantage of e.g. 0-2 being in all 3
sds_pricePerProperty$band <- 3 - (sds_pricePerProperty$tbs_0to2km + 
                                    sds_pricePerProperty$tbs_0to3km + 
                                    sds_pricePerProperty$tbs_0to4km)

#3 is "all other"
table(sds_pricePerProperty$band)  

sds_pricePerProperty$band2 <- factor(sds_pricePerProperty$band, levels = c(0,1,2,3),
                                     labels = c('0-2','0-3','0-4','other'))


ggplot(sds_pricePerProperty, aes(x = factor(band2), y = sd)) +
  geom_boxplot() 

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#Prepare data for synthetic test-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
#Load housing/turbine data
#Originals are backed up original/backup
#Alter these here.
#Changed this to backup without checking they actually load. Should do...
hse <- read.csv("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/original/backup/SingleSalesPlusRepeatSales_filtered_July16.csv")
  
tb <- read.csv("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/original/backup/turbinesFinal_reducedColumns_tipHeightsComplete.csv")
  
#Initial test: just two windfarms, just houses in range.
#See if the code will still run with that.

#Whitelee Eaglesham Moor (Part 1): 1st May 2009
#Earlseat (15th Oct 2014, near Kirkcaldy/Glenrothes).


tb_geo <- tb
#Keep those two windfarm sites
tb_geo <- tb_geo %>% filter(nameMinusTurbine %in% c('Whitelee Eaglesham Moor (Part 1)','Earlseat'))
coordinates(tb_geo) <- ~Feature.Easting+Feature.Northing

hse_geo <- hse
coordinates(hse_geo) <- ~eastingsFinal+northingsFinal

#get area of properties we want to make pretend prices for
tb_buffz <- gBuffer(tb_geo,byid = F, width = 15000)
# plot(tb_buffz)
# points(tb_geo)

#Keep only sales within that range
hse_subz <- hse_geo[tb_buffz,]
#points(hse_subz, col="red")

#Set all prices to stationary process
hse_subz@data$priceFinal <- rnorm(nrow(hse_subz),mean = 1000, sd = 20)
plot(density(hse_subz$priceFinal))

#Get buffers for the two turbine sites for changing the prices for each
#after the operation date
whitelee <- gBuffer(tb_geo[tb_geo@data$nameMinusTurbine == 'Whitelee Eaglesham Moor (Part 1)',],byid = F, width = 15000)
earlseat <- gBuffer(tb_geo[tb_geo@data$nameMinusTurbine == 'Earlseat',],byid = F, width = 15000)

hse_subz@data$windfarm <- 'Whitelee'
#hse_subz@data$windfarm[hse_subz[earlseat,''] %>% row.names %>% as.numeric] <- 'Earlseat'

#This should have been easier!
earlseat_houses <- hse_subz[earlseat,] %>% data.frame
hse_subz@data$windfarm[hse_subz@data$Title %in% earlseat_houses$Title] <- 'Earlseat'

#Phew
table(hse_subz@data$windfarm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#OK, so dates for those two?
tb_geo@data$statusDateFormatted <- as.Date(tb_geo@data$statusDateFormatted)
table(tb_geo@data$statusDateFormatted)

#whitelee, earlseat
#2009-05-01 2014-10-15
whitelee_op <- as.Date('2009-05-01')
earlseat_op <- as.Date('2014-10-15')

hse_subz@data$date <- as.Date(hse_subz@data$date)

#Hang on... we have no sales after earlseat, do we? Newp!
hse_subz@data$date %>% max

#Right, time to lower prices near windfarms! Only for Whitelee, it turns out
hse_subz@data$priceFinal[hse_subz@data$windfarm == 'Whitelee' 
                         & hse_subz@data$date > whitelee_op] <- 
  rnorm(nrow(hse_subz[hse_subz@data$windfarm == 'Whitelee' 
                                      & hse_subz@data$date > whitelee_op,]),
        mean = 800, sd = 20)

#Check
hse_chk <- data.frame(hse_subz)

ggplot(hse_chk, aes(x = date, y = priceFinal, colour = windfarm)) +
  geom_point()

#Save as the versions to use.
#tb_save <- tb_geo %>% data.frame()

#Change of plan: use original but change all but Eaglesham moor dates
#To after the sale period.
tb_changeDates <- tb

tb_changeDates$statusDateFormatted <- as.Date(tb_changeDates$statusDateFormatted)

#earlseat data is late enough
tb_changeDates$statusDateFormatted[tb_changeDates$nameMinusTurbine!='Whitelee Eaglesham Moor (Part 1)'] <- 
  earlseat_op

table(tb_changeDates$statusDateFormatted)

write.csv(hse_chk,"C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/original/SingleSalesPlusRepeatSales_filtered_July16.csv", row.names = F)

write.csv(tb_changeDates,"C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/original/turbinesFinal_reducedColumns_tipHeightsComplete.csv", row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#Synthetic test attempt two: work on final master file-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

master <- read.dta13("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/work/backups/master_combined_001_original.dta")

#Poking around to work out what's what
#All the "after" fields contain the count of turbines where the sale date is after the operational date
#just to look at a couple in excel...
#write.csv(master[1:100,], "C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/temp/master_sample.csv")

#Set all "after" turbines to zero
#apart from those properties within 15km of eaglesham moor part 1


#Simple test: leave everything else the same, make all prices the same
master$p

#Run code in above section to get list of properties within range
#or reload...
hse_chk <- read.csv("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/original/SingleSalesPlusRepeatSales_filtered_July16.csv")

nr_whitelee <- hse_chk %>% filter(windfarm == 'Whitelee')
nr_earlseat <- hse_chk %>% filter(windfarm == 'Earlseat')

#What's range of quarters in the master file?
#I don't need to match - can just pick arbitrary point for before/after
#120 to 216
hist(master$house_date_q)
range(master$house_date_q)

#This may be easier to do by splitting the two then re-joining
unique(nr_whitelee$Title) %>% length
#Less in master. Some must already have been filtered...?
#153934 vs 152473
unique(master$title) %in% unique(nr_whitelee$Title) %>% table

names(master)
#Ones to change (set to zero for all / for dates before)
#turb_vis_trn_after: 71 to 85
#turb_after0: 86 to 100
#turb_vis_bldg_after: 101 to 115

#test 2
#set all houses to have 1 turbine (visible too) at each distance band
#At some point - though most will not have op date within range (setting to zero below)
#Double-check names
# master[,c(41:55)] %>% names
# master[,c(26:40)] %>% names
# master[,c(56:70)] %>% names
# 
# #Set to one
# master[,c(41:55)] <- 1
# master[,c(26:40)] <- 1
# master[,c(56:70)] <- 1

#Split, change separately
#mostz_master <- master[!(master$title) %in% unique(nr_whitelee$Title),]

#reduce mostz to just earlseat
mostz_master <- master[(master$title) %in% unique(nr_earlseat$Title),]
whitelee_master <- master[(master$title) %in% unique(nr_whitelee$Title),]

#check we're def targeting right columns
mostz_master[,c(71:85)] %>% names
mostz_master[,c(86:100)] %>% names
mostz_master[,c(101:115)] %>% names

#set all those to zero: no sales after turbine operational dates
mostz_master[,c(71:85)] <- 0
mostz_master[,c(86:100)] <- 0
mostz_master[,c(101:115)] <- 0

#For the whitelee ones, first copy across the pre-date-filtered in their entirety
#Before wiping those before our arbitrary date
#So:
#turb_vis_trn_after: 71 to 85, copy over with 41:55
#turb_after0: 86 to 100, copy over with 26:40
#turb_vis_bldg_after: 101 to 115, copy over with 56:70
#Again, let's just make sure...
mostz_master[,c(41:55)] %>% names
mostz_master[,c(26:40)] %>% names
mostz_master[,c(56:70)] %>% names

#Copy over (which should all be 1 for the second test)
whitelee_master[,c(71:85)] <- whitelee_master[,c(41:55)]
whitelee_master[,c(86:100)] <- whitelee_master[,c(26:40)]
whitelee_master[,c(101:115)] <- whitelee_master[,c(56:70)]

#Same stationary prices for both initially
#(Could have done this above in one line, ah well)
mostz_master$pricefinal <- rnorm(nrow(mostz_master),mean = 1000, sd = 20)
whitelee_master$pricefinal <- rnorm(nrow(whitelee_master),mean = 1000, sd = 20)

#Now set prices for whitelee after certain date to lower
#And set to zero all trb_afters before that
table(0 + whitelee_master$house_date_q > 170)

whitelee_master$pricefinal[whitelee_master$house_date_q > 170] <- 
  rnorm(nrow(whitelee_master[whitelee_master$house_date_q > 170,]),mean = 500, sd = 20)

#Only leave turbine after markers in after that quarter
whitelee_master[whitelee_master$house_date_q < 170,c(71:85)] <- 0
whitelee_master[whitelee_master$house_date_q < 170,c(86:100)] <- 0
whitelee_master[whitelee_master$house_date_q < 170,c(101:115)] <- 0

#OK, that's looking correct
#Check prices
ggplot() +
  geom_point(data = whitelee_master, aes(x = house_date_q, y = pricefinal)) +
  geom_point(data = mostz_master, aes(x = house_date_q, y = pricefinal), colour = 'blue') 

#Check turbine after/before dates
hist(whitelee_master$house_date_q[whitelee_master$turb_after0!=0])
hist(whitelee_master$house_date_q[whitelee_master$turb_after0==0])
#Tick
hist(mostz_master$house_date_q[mostz_master$turb_after0!=0])
hist(mostz_master$house_date_q[mostz_master$turb_after0==0])

#OK. Rejoin, save as CSV, import to Stata to save as dta
finalz <- rbind(mostz_master,whitelee_master)

#Then: IMPORTANT STEP! LOG PRICE FINAL!
#It's log values that's used...
finalz$ln_price <- log(finalz$pricefinal)
#plot(finalz$ln_price, log(finalz$pricefinal))

write.csv(finalz,"C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/temp/master_combined_syntheticCheck.csv", row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#Synthetic test attempt three: master file again, play with existing dates/prices-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

#Plan: take existing dates of turbines becoming operational
#All already encoded in the "after" columns
#Start with even prices
#Use those columns to lower them proportionally to both distance
#number and visibility
master <- read.dta13("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/work/backups/master_combined_001_original.dta")

#These are the columns we need to work with
master[,c(71:85)] %>% names#terrain
master[,c(86:100)] %>% names#distance
master[,c(101:115)] %>% names#building

#Actually, I don't think we need terrain columns, it's not used.

#Sanity check:
#number of turbines in distance should go up over time
master$rowsumcheck <- rowSums(master[,c(86:100)])
#master$rowsumcheck <- rowSums(master[,c(71:85)])
#head(master[,c(71:85,149)])

#titleSample <- unique(master$title) %>% sample(1000)

#Yup, they go up!
ggplot(master[master$title %in% titleSample,], aes(x = house_date_q, y = rowsumcheck, colour = title)) +
  geom_point() +
  geom_line() +
  guides(colour = F)
  
#OK, now: set all prices equal to begin with
#Let's avoid randomness for now, see how it looks
master$pricefinal = 10000

master$priceFinalOriginal = master$pricefinal

#Now, reduce prices for each turbine in distance
#And for each visible (terrain only)
#In proportion to distance and turbine number

#for each distance band
for(i in seq(from = 0, to = 14)){
  
 #Adjust price down in proportion to distance and turbine number
  #distance adjustment:
  distAdj = (15 - i)/1000
  print(paste0("distAdj: ",distAdj))
  
  #Multiply by number of turbines
  turbAdjDist = distAdj * master[,(86+i)]#distance only
  print(paste0("max number of turbines in this band: ",max(master[,(86+i)]), 
               ", max adjustment val: ", max(turbAdjDist) ))
  
  #Adjust prices. turbAdjDist will mostly be zero
  #multiplied by zero turbines, so...
  priceAdjust = 1 - (turbAdjDist)
  
  master$pricefinal <- master$pricefinal * priceAdjust
  
}

#Repeat for visible
for(i in seq(from = 0, to = 14)){
  
  #Adjust price down in proportion to distance and turbine number
  #distance adjustment:
  distAdj = (15 - i)/1000
  print(paste0("distAdj: ",distAdj))
  
  #Multiply by number of turbines
  turbAdjDist = distAdj * master[,(71+i)]#distance only
  print(paste0("max number of turbines in this band: ",max(master[,(71+i)]), 
               ", max adjustment val: ", max(turbAdjDist) ))
  
  #Adjust prices. turbAdjDist will mostly be zero
  #multiplied by zero turbines, so...
  priceAdjust = 1 - (turbAdjDist)
  
  master$pricefinal <- master$pricefinal * priceAdjust
  
}

#master[sample(100),c('priceFinalOriginal','pricefinal')]

#OK, looks like it works. Can we check those prices decline over time?
#Yup, pretty dramatically...

titleSample <- unique(master$title) %>% sample(1000)

ggplot(master[master$title %in% titleSample,], aes(x = house_date_q, y = pricefinal, colour = title)) +
  geom_point() +
  geom_line() +
  guides(colour = F)

#Save and see...
master$ln_price <- log(master$pricefinal)
#plot(finalz$ln_price, log(finalz$pricefinal))

write.csv(master[,c(1:148)],"C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/temp/master_combined_syntheticCheck.csv", row.names = F)
#reload cos of crash
master <- read.csv("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/data/temp/master_combined_syntheticCheck.csv")

#I should probably just check there's a rough correlation...
master$rowsumcheck <- rowSums(master[,c(86:100)])
master$rowsumcheckViz <- rowSums(master[,c(71:85)])
master$rowsumcheckVizNDist <- rowSums(master[,c(71:100)])

pairs(master[sample(nrow(master),10000),c('rowsumcheck','rowsumcheckViz','rowsumcheckVizNDist','pricefinal')])

plot(master[sample(10000),c('ln_price','pricefinal')])

#By actual distance band count?
pairs(master[sample(nrow(master),10000),c(71:78,22)])

#compare one distance band to its visible pairing
pairs(master[sample(nrow(master),100000),c(93,78,22)])
pairs(master[sample(nrow(master),100000),c(88,73,22)])

pairs(master[sample(nrow(master),100000),c(86,71,22)])
pairs(master[sample(nrow(master),100000),c(87,72,22)])

#Create non-viz (opposite of viz: those within range that *cannot* be seen)
nviz <- master[,c(86:100)] - master[,c(71:85)]
#Which should never be less than zero? Tick.
range(nviz)
names(nviz) <- sapply(seq(from = 0, to = 14), function(x) paste0("nviz_turb_after",x))
master <- cbind(master,nviz)

#nviz vs viz 0-1
pairs(master[sample(nrow(master),100000),c(152,71,22)])
pairs(master[sample(nrow(master),100000),c(153,72,22)])
pairs(master[sample(nrow(master),100000),c(155,74,22)])
pairs(master[sample(nrow(master),100000),c(156,75,22)])

#how many in each quarter are unaffected by a turbine?
affected <- master[,c('pricefinal','house_date_q')] %>% 
  mutate(priceUnchanged = 0 + (pricefinal == 10000)) %>% 
  group_by(house_date_q) %>% 
  summarise(survived = sum(priceUnchanged == 1), didnt = sum(priceUnchanged == 0))

#we need the proportion of those sales...
affected$proportion = (affected$survived/(affected$survived+affected$didnt))*100

plot(affected$house_date_q,affected$proportion)  

#~~~~~~~~~~~~~~~~~
#Check distance is doing what I wanted it to.
#Ggplot x axis to be distance. Pick out only properties with 1,2,3 turbines 
#in one band only so I can check it's doing what I think it is (and same for visible).

#Subset of cols we actually need to work with, so I can look at them
mastersub <- master %>% dplyr::select(title,house_date_q,pricefinal,ln_price,turb_vis_trn_after0:turb_after14)

#just have to check 14 of the columns have a zero in
#And then pick the remaining value.
zeroesMask14cols <- mastersub %>% dplyr::select(turb_after0:turb_after14)
#Convert any positive values to one
zeroesMask14cols <- ifelse(zeroesMask14cols == 0, 0, 1) %>% data.frame
#Now if it sums to one, we know there are fourteen zeros
hasFourteenZeros <- ifelse(rowSums(zeroesMask14cols)==1, 1, 0)
#And should still be in correct order...

mastersub <- cbind(mastersub,hasFourteenZeros)

#Now we can actually use that for all values
#If hasfourteenzeros just pick the one value (which rowsum will pick up)
mastersub$singletonValue <- ifelse(mastersub$hasFourteenZeros == 1, 
                                   rowSums(mastersub %>% dplyr::select(turb_after0:turb_after14)),
                                   -1)

#REPEAT FOR VIZ
zeroesMask14cols <- mastersub %>% dplyr::select(turb_vis_trn_after0:turb_vis_trn_after14)
#Convert any positive values to one
zeroesMask14cols <- ifelse(zeroesMask14cols == 0, 0, 1) %>% data.frame
#Now if it sums to one, we know there are fourteen zeros
hasFourteenZerosViz <- ifelse(rowSums(zeroesMask14cols)==1, 1, 0)
#And should still be in correct order...

mastersub <- cbind(mastersub,hasFourteenZerosViz)

#Now we can actually use that for all values
#If hasfourteenzeros just pick the one value (which rowsum will pick up)
mastersub$singletonValueViz <- ifelse(mastersub$hasFourteenZerosViz == 1, 
                                   rowSums(mastersub %>% dplyr::select(turb_vis_trn_after0:turb_vis_trn_after14)),
                                   -1)

#OK, so now what we got? How to make long?
#keep only the ones with sales with single turbines (either in range or visible)
#And the distance band columns, of course! Oops...
singleTurbinesAfterSales <- mastersub %>% filter(singletonValue!=-1 | singletonValueViz!=-1) %>% 
  dplyr::select(title,house_date_q,pricefinal,ln_price,singletonValue,singletonValueViz,
                turb_after0:turb_after14,
                turb_vis_trn_after0:turb_vis_trn_after14)

gatherzy <- singleTurbinesAfterSales %>% dplyr::select(pricefinal,singletonValue:turb_vis_trn_after14) %>% 
  gather(distband,turbcount,turb_after0:turb_vis_trn_after14)

#If I keep only those with positive turb count, we should be able to get which distance band
#That count was in, right?
# gatherz <- gatherz %>% filter(turbcount!=0 & singletonValue!=-1 & singletonValueViz!=-1)
#Actually, we want to keep e.g. in distance band but not visible..
#gatherz <- gatherz %>% filter(turbcount!=0 & (singletonValue!=-1 | singletonValueViz!=-1))

#checking one at a time...
gatherz_nviz <- gatherzy %>% filter(grepl("turb_after",distband) & turbcount!=0 & singletonValue!=-1 & singletonValueViz==-1)
#The logic for this is fiddly: read windfarms notes section "moar checks"
gatherz_viz <- gatherzy %>% filter(grepl("turb_vis",distband) & turbcount!=0 & singletonValue!=-1 & singletonValueViz!=-1)

gatherz_viz$viz = 'viz'
gatherz_nviz$viz = 'nviz'

gatherz <- rbind(gatherz_viz,gatherz_nviz)


#Think so! Looking promising
#Then: gather the singleValues again, which will give one column
#indicating if just distance band or viz (err, but not both)
#But that's OK I think...
# gatherz2 <- gatherz %>% gather(singletonValue,)

#Actually, hold off on that: I think I may already have that value from the cols themselves
#in turbcount.
#I do need to mark which is viz and which not
#And then mark actual distance bands
# gatherz$viz <- 0
# gatherz$viz[grepl("vis",gatherz$distband)] <- 1

#mark distance bands
gatherz$distband_numeric = 0

for(i in seq(from = 0, to = 14)){
    gatherz$distband_numeric[grepl(as.character(i),gatherz$distband)] = i
}

#Actually, we want to separate the two out
#vizonly should be one if unique
# gatherz$vizexclusive <- 'both'
# gatherz$vizexclusive[gatherz$singletonValueViz!=-1 & gatherz$singletonValue==-1] <- 'vizonly'
# gatherz$vizexclusive[gatherz$singletonValueViz==-1 & gatherz$singletonValue!=-1] <- 'bandonly'

#<- ifelse(gatherz$singletonValueViz!=-1 & gatherz$singletonValue==-1,1,0)

#In theory, that's everything we need right?
#Just exclusive viz and band for starters
#ggplot(gatherz %>% filter(vizexclusive %in% c('vizonly','bandonly')),
ggplot(gatherz,
       aes(x = distband_numeric, y = pricefinal, colour = factor(turbcount))) +
  geom_point()  +
 facet_wrap(~viz)




