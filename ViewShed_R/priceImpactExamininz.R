#Quick digging around into some price impact stuff / getting head around DiD.
library(dplyr)
library(tidyr)
library(pryr)
library(zoo)
library(ggplot2)
library(modeest)

geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#So just to help think through simple DiD, let's pick a simple 2x2 grouping out of the data
areacodes <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/UniqueAddressesOnly_repeatSales_areacodes2.csv")

hseprices <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/repeatSales_22_5_16.csv")

#Err. Is that different to the bulk sales filtered one? Shouldn't be but I can't see where I saved that...
#hseprices_bulkrm <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

#Yeeeeh, looking like the same data... the former has the id in, though, which we'll need. Where did I put that in?
#Oh, filter_update_22_05.R
#And apparently I checked everything, so that's good...

#Results so I can grab a subset e.g. Kathkin Braes. Non building heights to start with.
results <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/allHouses_CEDArun.csv")

#Graph again...
hseprices$date = as.Date(hseprices$date)
hseprices$yearmon <- as.yearmon(hseprices$date)
hseprices$quarters <- as.yearqtr(hseprices$date)
hseprices$years <- cut(hseprices$date, breaks='year')

hs_monthlyavprice <- hseprices %>% group_by(yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), monthlymedian = median(priceFinal), numsales = n())

ggplot(hs_monthlyavprice, aes(x = as.Date(yearmon), y = monthlyavprice)) +
  geom_line()

#Doesn't matter about accuracy here, I just want to look.
#Oh - I've only got visible obs. I need the full distance matrix to pull out the right ones.
#Hmm. 3.5 gig. Hmm.
#matr <- read.csv("data/distanceMatrix/distanceMatrixALL.csv")

#Let's just do it manually here instead
turbs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns_tipHeightsComplete.csv")

coordinates(turbs) <- ~Feature.Easting+Feature.Northing

turbs_df <- data.frame(turbs)

#Do the subsetting on the properties... oh I can't, there's no coordinates in there!
coordinates(hseprices) <- ~eastingsFinal+northingsFinal

#This is sounding weird!
#turbBuffer <- gBuffer(turbs[grepl("Cathekin", turbs@data$nameMinusTurbine),], width = 15000)

#whitelee turbine, much earlier date of operation (may 2009) 
turbindex = 2344

#turbBuffer <- gBuffer(turbs[grepl("Cathekin", turbs@data$nameMinusTurbine),], width = 15000)
turbBuffer <- gBuffer(turbs[turbs@data$index==turbindex,], width = 15000)

#houses within 15km of Cathekin Braes turbine
hses_ThisTurbine <- hseprices[turbBuffer,]

plot(hses_ThisTurbine)

#merge results into those house prices. Should stay 410425
hses_ThisTurbine <- merge(hses_ThisTurbine, 
                       results %>% dplyr::select(Title,canISeeAnyObs,visibleObs),
                       by  = 'Title')

#cathekin is 672
#hses_ThisTurbine@data$canSeeThisTurbine <- ifelse(grepl("672",hses_ThisTurbine@data$visibleObs),1,0)
hses_ThisTurbine@data$canSeeThisTurbine <- ifelse(grepl(as.character(turbindex),hses_ThisTurbine@data$visibleObs),1,0)

#check
hses_ThisTurbine_df <- data.frame(hses_ThisTurbine)

plot(hses_ThisTurbine, col = ifelse(hses_ThisTurbine@data$canSeeThisTurbine==1,'red','green'))

#OK, that's enough for a simple check
#Split into before/after operational
#Cathekin's actually very recent, so we might not see much...
#Dummy: after as 1
#both dates need dating
hses_ThisTurbine@data$date <- as.Date(hses_ThisTurbine@data$date)
turbs_df$statusDateFormatted <- as.Date(turbs_df$statusDateFormatted)

hses_ThisTurbine@data$beforeOperational <- 
  ifelse(hses_ThisTurbine@data$date < turbs_df[turbs_df$index==turbindex,c('statusDateFormatted')],
         1,0)

#check
hses_ThisTurbine_df <- data.frame(hses_ThisTurbine)

#OK, some...
table(hses_ThisTurbine@data$beforeOperational)
#This is our diff in diff group: before/after, visible/not
table(hses_ThisTurbine@data$beforeOperational, hses_ThisTurbine@data$canSeeThisTurbine)

#Pare down to just what we need to look at before reshaping
hse_kath_min <- data.frame(hses_ThisTurbine)
hse_kath_min <- hse_kath_min %>% dplyr::select(date,priceFinal,canSeeThisTurbine,beforeOperational)

#Smooth prices... for which we'll need the right order
#Actually smoothing won't make sense without means.

#Weekly mean? But it needs to be per group...
#rollingmean = rollmean(monthlyavprice, 12, na.pad = T))
hse_kath_min$yearmon <- as.yearmon(hse_kath_min$date)

hs_monthlyavprice <- hse_kath_min %>% group_by(canSeeThisTurbine, yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal))

ggplot(hs_monthlyavprice, aes(x = as.Date(yearmon), y = monthlyavprice, colour = factor(canSeeThisTurbine))) +
  geom_line() +
  scale_y_log10()

#quarters
hse_kath_min$quarters <- as.yearqtr(hse_kath_min$date)

hs_quarterlyavprice <- hse_kath_min %>% group_by(canSeeThisTurbine,beforeOperational,quarters) %>% 
  summarise(quarterlyavprice = mean(priceFinal), quarterlymedian = median(priceFinal), quarterNumsales = n())

ggplot(hs_quarterlyavprice) +
  geom_line(data = subset(hs_quarterlyavprice, hs_quarterlyavprice$beforeOperational == 0), 
            aes(x = as.Date(quarters), y = quarterlyavprice, colour = factor(canSeeThisTurbine))) +
  geom_line(data = subset(hs_quarterlyavprice, hs_quarterlyavprice$beforeOperational == 1), 
            aes(x = as.Date(quarters), y = quarterlyavprice, colour = factor(canSeeThisTurbine))) 


#what's the date range for after operational?
max(hse_kath_min$date[hse_kath_min$beforeOperational==0]) - 
  min(hse_kath_min$date[hse_kath_min$beforeOperational==0])

#Time difference of 1795 days
#Take same period prior to operational date, find means in both periods
meangroup <- hse_kath_min[hse_kath_min$date > min(hse_kath_min$date[hse_kath_min$beforeOperational==0])-1795,]

hs_quarterlyavprice <- meangroup %>% group_by(canSeeThisTurbine,beforeOperational,quarters) %>% 
  summarise(quarterlyavprice = mean(priceFinal), quarterlymedian = median(priceFinal), quarterNumsales = n())

ggplot(hs_quarterlyavprice) +
  geom_line(data = subset(hs_quarterlyavprice, hs_quarterlyavprice$beforeOperational == 0), 
            aes(x = as.Date(quarters), y = quarterlyavprice, colour = factor(canSeeThisTurbine))) +
  geom_line(data = subset(hs_quarterlyavprice, hs_quarterlyavprice$beforeOperational == 1), 
            aes(x = as.Date(quarters), y = quarterlyavprice, colour = factor(canSeeThisTurbine))) 

#Means in each time period/viewable
meanz <- meangroup %>% group_by(canSeeThisTurbine,beforeOperational) %>% 
  summarise(mean = mean(priceFinal))

#Diff between time periods for can-see
diff1 = ((meanz[1,3] - meanz[2,3])/meanz[1,3])*100
diff1[2,] = ((meanz[3,3] - meanz[4,3])/meanz[4,3])*100

diff1
diff1[2,1] - diff1[1,1]

#~~~~~~~~~~~~~~
#General stats!----

#I wanna know some of the spread of % changes. Bit of bootsrapping mefinks.

#Updating: get latest housing data... oh, same one apparently
#set to laptop dropbox ref currently...
hseprices <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/repeatSales_22_5_16.csv")

hseprices$date = as.Date(hseprices$date)
hseprices$yearmon <- as.yearmon(hseprices$date)
hseprices$quarters <- as.yearqtr(hseprices$date)
hseprices$years <- cut(hseprices$date, breaks='year')

results <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/allHouses_CEDArun.csv")
results_bh <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/allHouses_buildingHeights_CEDArun.csv")


#What are the differences in price (for different time periods) 
#for those in places where at some point a turbine became visible 
#and ones where it never did?
hseprices_df <- data.frame(hseprices)

#keep only useful cols
hseprices_df <- hseprices_df %>% dplyr::select(Title,date,yearmon,quarters,years,priceFinal,eastingsFinal:northingsFinal)

#Merge in results
#merge results into those house prices. Should stay 410425
hseprices_df <- merge(hseprices_df, 
                          results %>% dplyr::select(Title,canISeeAnyObs,visibleObs),
                          by  = 'Title')

#merge in BH version too
results_bh <- results_bh %>% rename(canISeeAnyObs_BH = canISeeAnyObs,visibleObs_BH = visibleObs)

hseprices_df <- merge(hseprices_df, 
                      results_bh %>% dplyr::select(Title,canISeeAnyObs_BH,visibleObs_BH),
                      by  = 'Title')

#Merge in flag for which are (a) within 15km and (b) do cross building height data
flagz <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/doHousesCross_BuildingHeightData.csv")

hseprices_df <- merge(hseprices_df,flagz %>% dplyr::select(Title,distanceToNearest,crosses_BH),by = 'Title')

#Flag for 'within 15km'
hseprices_df$within15km <- 0 + (hseprices_df$distanceToNearest!=-1)

#numbers for those two?
table(hseprices_df$crosses_BH)
table(hseprices_df$within15km)

hseprices_df <- hseprices_df[order(as.Date(hseprices_df$years)),]

yrz = unique(hseprices_df$years)

#bootstrap sample from each of can and can't see a turbine
densities <- data.frame(means = as.numeric(), canSee = as.numeric(), bh_flag = as.numeric(), year = as.character())

#for each year's prices...
for(i in yrz) {
  
  #cansees first, for terrain
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs == 1
                                            & hseprices_df$within15km == 1]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 1,
    bh_flag =0,
    year = i)
  
  densities <- rbind(densities,add)
  
  #then can'ts
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs == 0
                                            & hseprices_df$within15km == 1]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 0,
    bh_flag =0,
    year = i)
  
  densities <- rbind(densities,add)
  
  #Then repeat both for building height can/can't see
  #cansees first
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs_BH == 1
                                            & hseprices_df$within15km == 1
                                            & hseprices_df$crosses_BH == 1]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 1,
    bh_flag =1,
    year = i)
  
  densities <- rbind(densities,add)
  
  #then can'ts
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs_BH == 0
                                            & hseprices_df$within15km == 1
                                            & hseprices_df$crosses_BH == 1]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 0,
    bh_flag =1,
    year = i)
  
  densities <- rbind(densities,add)
  
  print(paste0("year ",i))
  
}


#REPEAT WHOLE THING FOR ALL, NOT JUST <15KM / BH CROSSING SUBSET
densities2 <- data.frame(means = as.numeric(), canSee = as.numeric(), bh_flag = as.numeric(), year = as.character())

#for each year's prices...
for(i in yrz) {
  
  #cansees first, for terrain
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs == 1]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 1,
    bh_flag =0,
    year = i)
  
  densities2 <- rbind(densities2,add)
  
  #then can'ts
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs == 0]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 0,
    bh_flag =0,
    year = i)
  
  densities2 <- rbind(densities2,add)
  
  #Then repeat both for building height can/can't see
  #cansees first
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs_BH == 1]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 1,
    bh_flag =1,
    year = i)
  
  densities2 <- rbind(densities2,add)
  
  #then can'ts
  year_n_canIsee <- hseprices_df$priceFinal[hseprices_df$years==i 
                                            & hseprices_df$canISeeAnyObs_BH == 0]
  
  add <- data.frame(
    means = sapply(seq(1:25000), function(x) 
      mean(sample(year_n_canIsee, 1000, replace = T))),
    canSee = 0,
    bh_flag =1,
    year = i)
  
  densities2 <- rbind(densities2,add)
  
  print(paste0("year ",i))
  
}

#Save both of those for ease of loading later
saveRDS(densities,"saves/densities.rds")
saveRDS(densities2,"saves/densities2.rds")
#~~~
densities <- readRDS("saves/densities.rds")
densities2 <- readRDS("saves/densities2.rds")

#cansee <- sapply(seq(1:10000), function(x) mean(sample(hseprices_df$priceFinal,1000, replace = T)))
#plot(density(densities))

output <- ggplot(densities, aes(x = means)) +
  geom_density(aes(group = canSee,colour = factor(canSee))) +
  facet_wrap(~year, scales = 'free_y')
  # facet_wrap(~year, scales = 'free_y',ncol = 1)
#  facet_wrap(~year, scales = 'free')
#  facet_wrap(~year)
output

ggsave(filename = "saves/densityplots_yearlybootstraps.png", output, dpi=200, width = 17,height = 8)

#ggplot(densities, aes(x = means)) +
#geom_area(alpha = 0.3, stat="bin", position="identity", colour="black",binwidth=100)

#Can I do confidence intervals for those?
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_%28ggplot2%29/#Helper%20functions

summaryz <- densities %>% group_by(year, canSee, bh_flag) %>% 
  summarise(mean = mean(means), 
            min = quantile(means,c(0.025,0.975))[[1]],
            max = quantile(means,c(0.025,0.975))[[2]])

dodge <- position_dodge(width=0.1)

summaryz$bh_flagf <- factor(summaryz$bh_flag, labels = c('terrain','buildings'))
summaryz$visible <- factor(summaryz$canSee, labels = c('no','yes'))

ggplot(summaryz, aes(x=as.Date(year), y=mean, colour=visible)) + 
  geom_errorbar(width = 0.1, alpha = 0.5, aes(ymin=min, ymax=max), position = dodge) +
  geom_line() +
  geom_point() +
  facet_wrap(~bh_flagf) +
  # facet_wrap(~factor(bh_flag, labels = c('terrain','buildings'))) +
  scale_y_log10()

#save after widening
summaryz_terrain <- summaryz %>% filter(bh_flagf=='terrain') 

sumwide <- summaryz_terrain[,c('year','mean','visible')] %>% 
  spread(visible,mean)

write.csv(sumwide,'data/turbs_visibleInTimePeriod_avs.csv',row.names = F)

#Repeat for both densities, the subset and all sales
densities$subset <- '<15km / crosses BH'
densities2$subset <- 'all'

joinz <- rbind(densities,densities2)

summaryz2 <- joinz %>% group_by(year, canSee, bh_flag,subset) %>% 
  summarise(mean = mean(means), 
            min = quantile(means,c(0.025,0.975))[[1]],
            max = quantile(means,c(0.025,0.975))[[2]])

summaryz2$bh_flagf <- factor(summaryz2$bh_flag, labels = c('terrain','buildings'))
summaryz2$visible <- factor(summaryz2$canSee, labels = c('no','yes'))

ggplot(summaryz2, aes(x=as.Date(year), y=mean, colour=visible)) + 
  geom_errorbar(width = 0.1, alpha = 0.5, aes(ymin=min, ymax=max), position = dodge) +
  geom_line() +
  geom_point() +
  facet_wrap(~bh_flagf+subset) +
  # facet_wrap(~factor(bh_flag, labels  =fc('terrain','buildings'))) +
  scale_y_log10()

#Just the one...
subz <- summaryz2 %>% filter(subset=='all',bh_flagf == 'terrain')

breakz = c(50000,100000,150000)
#labelz = as.character(breakz) 


output <- ggplot(subz, aes(x=as.Date(year), y=mean, colour=visible)) + 
  geom_errorbar(width = 0.1, alpha = 0.5, aes(ymin=min, ymax=max), position = dodge) +
  geom_line() +
  geom_point() +
  # scale_y_log10(breaks = c(1000,10000,20000),labels=c(.01,.1,1)) +
  scale_y_log10(breaks = breakz) +
  ylab("price (log)") + 
  xlab("year")
  
output

ggsave(filename = "saves/pricelog_viznon.png", output, dpi=200, width = 6,height = 4)




