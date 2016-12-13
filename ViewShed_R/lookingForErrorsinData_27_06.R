#Doing checks on data to see why values are being odd.
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

geolibs <- c("pryr","stringr","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#Get the fully processed data
hse_final <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

#confirm that's the same obs we used?
repeats <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/repeatSales_22_5_16.csv")

#Yes, apart from I added ID back in again. 
hse_final <- hse_final[order(hse_final$Title),]

#linked file before filtering down to repeats only. (So no extra price filtering done.)
all_linked <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge3.rds")

#Load original new RoS file before merge
#Except price field hasn't been tidied yet
#mdsi_plus_address <- readRDS("C:/Data/Housing/JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mdsi_plus_address.rds")

#So is there a version with prices done?
#Yes: housing / section 'combining price fields'
new_pricesOrgd <- readRDS("C:/Data/Housing/JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne.rds")

#That's considerably smaller than mdsi...?
#Nearly 170K less. Is all that filtering out >4 million???
nrow(mdsi_plus_address) - nrow(new_pricesOrgd)

#Let's get a version prior to that filtering and check difference

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat sales versus all 1. geocoded vs not-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Problem: prices not processed in all-sales.
#Let's use median to look at this, to hopefully deal with that.
#As a ballpark
mrg_allSales <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge3.rds")

#missing geocodes in this one? Yup.
table(0 + is.na(mrg_allSales$eastingsFinal))

#So first-up: check for any differences between geocoded and not
mrg_allSales$quarters <- as.yearqtr(mrg_allSales$date)

#mark geocoded or not 
mrg_allSales$hasLocation = factor(0 + !is.na(mrg_allSales$eastingsFinal))

#function for outputting quarterly means...
#http://stackoverflow.com/questions/19826352/pass-character-strings-to-ggplot2-within-a-function
#facetz not working...
lookAtQuarterlyMeans <- function(data, groupz, facetz = NULL) {
  
  print(groupz)
  
  quarterlyMeans <- data %>% group_by_('quarters', groupz) %>% 
    summarise(mean = mean(priceFinal), sd = sd(priceFinal))
  
  output <- ggplot(quarterlyMeans, aes_string(x = "as.Date(quarters)", y = 'mean', colour = groupz)) +
    geom_line() 
  
#   if(!is.null(facetz)) {
#     
#     print(facetz)
#     
#     #http://stackoverflow.com/questions/11028353/passing-string-variable-facet-wrap-in-ggplot-using-r
#     output + facet_wrap(as.formula(paste("~", facetz)))
#     #output + facet_wrap(reformulate(facetz))
#     
#   }
  
  output
  
}

lookAtQuarterlyMeans(mrg_allSales, 'hasLocation')

#Well, something odd! 
#Let's check it's not a sample size thing. Same for both...
sampleChk <- rbind(mrg_allSales %>% filter(hasLocation == 0), 
                   mrg_allSales %>% filter(hasLocation == 1) %>% 
                     sample_n(390036))

lookAtQuarterlyMeans(sampleChk)

#Newp. 
#What's it look like in the odd period? Say, 08/09 for no-location?
oh89noloc <- mrg_allSales %>% filter(date > "2008-01-01", date < "2009-12-31") %>% 
  arrange(-priceFinal)

#Hmm. Something with old? Look just at new
#Yup, new is fine.
mrg_allSales %>% filter(oldneworboth %in% c('newonly','both')) %>% lookAtQuarterlyMeans

mrg_allSales %>% filter(oldneworboth %in% c('oldonly')) %>% lookAtQuarterlyMeans
#mrg_allSales %>% filter(oldneworboth %in% c('oldonly', 'both')) %>% lookAtQuarterlyMeans

#So what's the crack with old? This is the same problem of bulk but...
oh89noloc_justold <- oh89noloc %>% filter(oldneworboth == 'oldonly') %>% arrange(-priceFinal)


#Massive quantities of huge bulk sales. Why do they peak in that period???

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat sales versus all 2. for geocoded, geographical difference?-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hs_geo <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes_oldnewboth.rds")

all_justgeoc <- mrg_allSales %>% filter(hasLocation == 1)

#zone that up.
ca <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2010/ScottishCouncilAreas2010_Derivedbyaggregating2011OAs_usingNRSexactfitCensusIndex", 
              layer="scotland_ca_2010")

coordinates(all_justgeoc) = ~eastingsFinal+northingsFinal
proj4string(all_justgeoc) <- proj4string(ca)

overz <- (all_justgeoc %over% ca)

#Long time. Save.
saveRDS(overz,"data/temp/overz.rds")

all_justgeoc@data$councilArea <- overz$code

all_justgeoc_df <- data.frame(all_justgeoc)

#check
plot(ca)
points(all_justgeoc[all_justgeoc@data$councilArea=='S12000006' 
                    & !is.na(all_justgeoc@data$councilArea),])

#So: for those that *do* have geocoded, 
#is there a difference between "all sales" and "repeat sales"?
#Make into one dataframe
#Which includes Title for some reason.
all_n_rpts <- hs_geo %>% dplyr::select(Title,date,priceFinal,oldneworboth,councilArea)

all_n_rpts$quarters <- as.yearqtr(all_n_rpts$date)

#mark source
all_n_rpts$sourcez <- 'repeat sales'

all_n_rpts <- rbind(all_n_rpts, all_justgeoc_df %>% 
                      dplyr::select(Title,date,priceFinal,oldneworboth,councilArea,quarters) %>% 
                      mutate(sourcez = 'all sales'))

all_n_rpts %>% lookAtQuarterlyMeans('sourcez')  

#new
chk <- all_n_rpts[all_n_rpts$oldneworboth %in% c('newonly','both'),]
chk %>% lookAtQuarterlyMeans('sourcez')  

#can I get the ggplot as output then add to it?
#output <- chk %>% lookAtQuarterlyMeans(c('source','councilArea'),'councilArea')

#just for new
quarterlyMeans <- chk %>% group_by(quarters, sourcez, councilArea) %>% 
  summarise(mean = mean(priceFinal), median = median(priceFinal), sd = sd(priceFinal))

#names! 
ca_names <- ca@data[,c(2:3)]

quarterlyMeans <- merge(quarterlyMeans, ca_names, by.x = "councilArea", by.y = "code")

output <- ggplot(quarterlyMeans[!is.na(quarterlyMeans$councilArea),], aes(x = as.Date(quarters), y = mean, colour = sourcez)) +
# output <- ggplot(quarterlyMeans[!is.na(quarterlyMeans$councilArea),], aes(x = as.Date(quarters), y = median, colour = sourcez)) +
  geom_line() +
  facet_wrap(~name, ncol = 4)

output

ggsave("saves/council_area_allvsrepeatsales_mean.png", dpi=150, width = 10, height = 12)

#all dates
quarterlyMeans <- all_n_rpts %>% group_by(quarters, sourcez, councilArea) %>% 
  summarise(mean = mean(priceFinal), median = median(priceFinal), sd = sd(priceFinal))

#names! 
ca_names <- ca@data[,c(2:3)]

quarterlyMeans <- merge(quarterlyMeans, ca_names, by.x = "councilArea", by.y = "code")

#output <- ggplot(quarterlyMeans[!is.na(quarterlyMeans$councilArea),], aes(x = as.Date(quarters), y = mean, colour = sourcez)) +
output <- ggplot(quarterlyMeans[!is.na(quarterlyMeans$councilArea),], aes(x = as.Date(quarters), y = median, colour = sourcez)) +
  geom_line() +
  facet_wrap(~name, ncol = 4, scales = 'free_y')

output

#ggsave("saves/council_area_allvsrepeatsales_mean.png", dpi=150, width = 10, height = 12)
ggsave("saves/council_area_allvsrepeatsales_median.png", dpi=150, width = 10, height = 12)

#zoom in on CAs
for(i in ca_names[,1]) {

output <- ggplot(quarterlyMeans[!is.na(quarterlyMeans$councilArea) & quarterlyMeans$name == i,], 
                 aes(x = as.Date(quarters), y = median, colour = sourcez)) +
# output <- ggplot(quarterlyMeans[!is.na(quarterlyMeans$councilArea) & quarterlyMeans$name == i,], 
#                  aes(x = as.Date(quarters), y = mean, colour = sourcez)) +
  geom_line() +
  ggtitle(i) + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

ggsave(paste0("saves/all_vs_repeats_CA/MEDIAN_council_area_allvsrepeatsales_mean_",i,".png"), output, 
              dpi=150, width = 10, height = 6)
# ggsave(paste0("saves/all_vs_repeats_CA/council_area_allvsrepeatsales_mean_",i,".png"), output, 
#               dpi=150, width = 10, height = 6)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat sales versus all 3. Geographical coverage-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Plan: proportions per council area. Then diff in proportions.
#Use all_n_rpts from above

#count per council area per all/repeats, proportions each group
#http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
countz <- all_n_rpts %>% group_by(sourcez, councilArea) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100)

#check. Tick.
sum(countz$freq)

countz <- merge(countz, ca_names, by.x = "councilArea", by.y = "code")

countz$name <- reorder(countz$name, countz$freq)

output <- ggplot(countz, aes(x = name, y = n, colour = sourcez)) +
# output <- ggplot(countz, aes(x = name, y = freq, colour = sourcez)) +
  geom_point(size = 4) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0))

output

ggsave("saves/all_vs_repeat_councilArea_counts.png", dpi=150, width = 8, height = 7)

#









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Old-RoS: compare original to my version----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get the original old-RoS housing data
orig_old_geo <- read.dta13("C:/Users/SMI2/Dropbox/WindFarms/Address Matching/data_geocodes_matched.dta")
#I'm not sure exactly what that is. Unique sales?

#these geocodes have an extra decimal...
orig_old_geo$Eastings <- orig_old_geo$Eastings/10
orig_old_geo$Northings <- orig_old_geo$Northings/10

#Make a single date field
orig_old_geo$date <- paste0(orig_old_geo$month,"/",orig_old_geo$year_txt)

#save that in a form we can load into QGIS to compare.
write.csv(orig_old_geo %>% dplyr::select(27,1,2,11,12,13,14),"data/old_geoTest.csv", row.names = F)

#Save version of merged file with less fields
hs <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

write.csv(hs %>% dplyr::select(34,5,9,33,38,39),"data/both_geoTest.csv", row.names = F)

#How much difference between means for olds in both?
#(In the original old there appear to be many duplicates, though more obs overall)

#Overall by date? Old needs its own proper date field for that
orig_old_geo$dateFormatted <- paste0("01/",orig_old_geo$date)
orig_old_geo$dateFormatted <- as.Date(orig_old_geo$dateFormatted, format='%d/%m/%Y')

orig_old_geo$quarters <- as.yearqtr(orig_old_geo$dateFormatted)

hs$quarters <- as.yearqtr(hs$date)

old_means <- orig_old_geo %>% group_by(quarters) %>% 
  summarise(mean = mean(price), median = median(price), sd = sd(price))

oldnew_means <- hs %>% filter(oldneworboth %in% c('oldonly','both')) %>% 
  group_by(quarters) %>% 
  summarise(mean = mean(priceFinal), median = median(priceFinal), sd = sd(priceFinal))

#Odd: orig old only goes up to 2004
#Anyway, let's match matching
together <- merge(old_means,oldnew_means,by="quarters")

names(together) <- c('quarters','mean_orig','median_orig','sd_orig',
                     'mean_latest', 'median_latest','sd_latest')

#graph em
togetherlong <- together %>% gather(thing,value,mean_orig:sd_latest)

ggplot(togetherlong, aes(x = as.Date(quarters), y = value, colour = thing)) +
  geom_line()

#check that for just the means...
togetherlongmeans <- together %>% dplyr::select(quarters,mean_orig,mean_latest) %>% 
  gather(thing,value,mean_orig:mean_latest)

#And the mean for the original is consistently higher - which I'm putting down to
#duplicates, but will need to check.
ggplot(togetherlongmeans, aes(x = as.Date(quarters), y = value, colour = thing)) +
  geom_line()

#Let's look at one of those quarters with silly high SD, see what's going on.
#1994 Q2
chk <- orig_old_geo %>% filter(quarters == '1994 Q2') %>% arrange(-price)

#Oh, turns out they're not all geocoded either. Fort dey wer
#Anyway, price = same problem I spotted before: price for entire bulk sale is included.

#Can I find any other original olds to compare to? 
#Just for price, ignoring location for now. (One that goes past 2004??)
#Feel like I've done this before but... 

#Possible files:
#C:\Users\SMI2\Dropbox\windfarms_duke\MatchData\ROSCleanData_Matched.zip
#C:\Users\SMI2\Dropbox\WindFarms\Address Matching\original.zip

#Actually, let's leave that until we can ask which file to check against. 
#Main point: data is basically the same shape (though we haven't compared repeats-only yet)

#But let's just check the geographies. For which I'll need to bin into council areas for orig
ca <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2010/ScottishCouncilAreas2010_Derivedbyaggregating2011OAs_usingNRSexactfitCensusIndex", 
              layer="scotland_ca_2010")

orig_coordz <- orig_old_geo

#Need to filter down just to those with geocodes...
orig_coordz <- orig_coordz %>% filter(!is.na(Eastings))
coordinates(orig_coordz) = ~Eastings+Northings

proj4string(orig_coordz) <- proj4string(ca)

orig_coordz@data$council_area <- (orig_coordz %over% ca) %>% dplyr::select(code)

#Quick! Save!
saveRDS(orig_coordz,"data/original_old_councilAreaPiP.rds")
orig_coordz <- readRDS("data/original_old_councilAreaPiP.rds")

#check... doesn't like yearmon classes (inc yearqtr). Drop those.
orig_coordz@data <- orig_coordz@data[,-27]
orig_df_ca <- data.frame(orig_coordz@data)

#council area column still data.frame. To vector plz?
orig_df_ca$council_area <- as.character(orig_df_ca$council_area$code)

#Add quarters back in!
orig_df_ca$quarters <- as.yearqtr(orig_df_ca$dateFormatted)

#Need to get housing data with geo stuff...
hs_geo <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes_oldnewboth.rds")

hs_geo$yearmon <- as.yearmon(hs_geo$date)
hs_geo$quarters <- as.yearqtr(hs_geo$date)

#Now. Quarterly price means for each council area. Facet on council area. 
orig_ca_quarterlyavprice <- orig_df_ca %>% group_by(council_area, quarters) %>% 
  summarise(quarterlyavprice = mean(price), 
            quarterlySD = sd(price),
            numsales = n(),
            source = "original")

latest_ca_quarterlyavprice <- hs_geo %>% group_by(council_area = councilArea, quarters) %>% 
  summarise(quarterlyavprice = mean(priceFinal), 
            quarterlySD = sd(priceFinal),
            numsales = n(),
            source = "latest")

both <- rbind(orig_ca_quarterlyavprice,latest_ca_quarterlyavprice)

#need to keep only common dates
both2 <- both %>% filter( quarters %in% both$quarters[both$source == 'original'] )

#merge in council area name
#These are already unique
ca_names <- ca@data[,c(2:3)]

both2 <- merge(both2, ca_names, by.x = "council_area", by.y = "code")


output <- ggplot(both2, aes(x = as.Date(quarters), y = quarterlyavprice, colour = source)) +
  geom_line() +
  facet_wrap(~name, scales = 'free_y', ncol = 4)

output

ggsave("saves/council_area_orig_vs_latest.png", dpi=150, width = 10, height = 12)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quick look at properties within 2km of turbine----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Do again to get more fields
hs <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

#hs$yearmon[hs$oldneworboth=="oldonly" & grepl("Dec", as.character(hs$yearmon))] %>% table

#Geographies each property sits in
#First column is not original ID. Ignore, match on title.
geogs <- read.csv("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/UniqueAddressesOnly_repeatSales_areacodes2.csv")

#names(hs)

#Don't need to be keeping all those
hs <- hs %>% dplyr::select(Title,date,priceFinal,eastingsFinal,northingsFinal,oldneworboth)
#Adding addresses back in, briefly
#hs <- hs %>% dplyr::select(Title,date,priceFinal,Subjects...FULL,eastingsFinal,northingsFinal)

#Merge in the geogs
hs_geo <- left_join(hs,geogs,by='Title')
hs_geo <- hs_geo %>% dplyr::select(-X)

#save that, it's useful in that form: all the basics
#saveRDS(hs, "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes.rds")
saveRDS(hs_geo, "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes_oldnewboth.rds")
hs_geo <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes_oldnewboth.rds")

#Saved with yearmon and quarters in
# hs_geo$yearmon <- as.yearmon(hs_geo$date)
# hs_geo$quarters <- as.yearqtr(hs_geo$date)

#
# ca_quarterlyavprice <- hs_geo %>% group_by(councilArea, quarters) %>% 
#   summarise(quarterlyavprice = mean(priceFinal), 
#             quarterlySD = sd(priceFinal),
#             quarterlymedian = median(priceFinal), 
#             numsales = n())
# 
# output <- ggplot(ca_quarterlyavprice) +
#     geom_line(data = ca_quarterlyavprice, 
#               aes(x = as.Date(quarters), y = quarterlyavprice, colour = councilArea)) +
#     #ylim(0,300000) +
#     guides(colour = F) 
# 
# output

#Get (some test) info on turbines within certain distance radii of properties
#Via attachInfoAboutTurbineToHouses.R
resultcombo2 <- read.csv("data/hses_plus_turbinesWithin5KM_n_2km.csv")

#Via Stephan: "Generate Indicator for sales after a turbine was built"
#So get turbine data
tb <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns.csv")

tb$quarters <- as.yearqtr(as.Date(tb$statusDateFormatted))

#Do for "within 2km" first
hs_geo_2km <- hs_geo
hs_geo_2km <- merge(hs_geo_2km, resultcombo2[,c('Title','turbinesWithin2km')], by = "Title")

#reduce sample to only those with turbines within 2km
hs_geo_2km <- hs_geo_2km %>% filter(turbinesWithin2km!="")

#vector of dates for the turbines. I want the modal date
#for places with many turbines within 2km
returnModalDate <- function(turbs){
  
  #turn turbs into list
  turblist <- strsplit(as.character(turbs),"\\|")[[1]]
  #print(turblist)
  
  datez <- tb$quarters[tb$index %in% turblist]
  
  #via NCmisc
  modez <- Mode(as.numeric(datez))
  
}

#sample to work on, for speed while testing
sz <- hs_geo_2km[sample(1:nrow(hs_geo_2km),2000),]
sz$turbQuarters <- lapply(sz$turbinesWithin2km, function(x) returnModalDate(x))
#Slight speed up of modal check
sz$turbQuarters <- as.yearqtr(sz$turbQuarters)
#Mark sales that happen after date of operation for model turbine
sz$saleAfter <- ifelse(sz$quarters > sz$turbQuarters, 1,0)


#Get running for whole thing
hs_geo_2km$turbQuarters <- lapply(hs_geo_2km$turbinesWithin2km, function(x) returnModalDate(x))
#Slight speed up of modal check
hs_geo_2km$turbQuarters <- as.yearqtr(hs_geo_2km$turbQuarters)
#Mark sales that happen after date of operation for model turbine
hs_geo_2km$saleAfter <- ifelse(hs_geo_2km$quarters > hs_geo_2km$turbQuarters, 1,0)

#Oh of course: it's only 68K to start with. It'll get bigger for bigger distances
#(Is that one of the possible problems? How do those samples change?)
saveRDS(hs_geo_2km,"data/hs_geo_2km_w_dates.rds")
hs_geo_2km <- readRDS("data/hs_geo_2km_w_dates.rds")

#look at smaller... keep council areas for faceting
sm <- hs_geo_2km %>% dplyr::select(Title,date,councilArea,priceFinal,quarters,turbinesWithin2km,turbQuarters,saleAfter)
sm <- sm %>% arrange(Title,date)

#how many do we have that never had a sale after turbine?
sm <- sm %>% group_by(Title) %>% 
  mutate(noSalesAfterTurb = ifelse(mean(saleAfter)==0, 1, 0))

#I need no sales after turb to have a better name!
names(sm)[names(sm)=='noSalesAfterTurb'] <- 'noTurbineBuiltinSalePeriod'
sm$noTurbineBuiltinSalePeriod <- factor(sm$noTurbineBuiltinSalePeriod)

#names again plz!
sm2 <-merge(sm, ca_names, by.x = "councilArea", by.y = "code")




#Let's just have a look at "no turbine built in sale period" on a map plz
#just one value per property
mapz <- sm %>% group_by(Title) %>% 
  summarise(noTurbineBuiltInSalePeriod = max(as.numeric(as.character(noTurbineBuiltinSalePeriod))))

#reattach geocodes and save
geocodes <- unique(hs_geo[,c('Title','eastingsFinal','northingsFinal')])

mapz <- merge(mapz,geocodes, by = 'Title')

#So only 25K properties...
write.csv(mapz,"data/check2km_noTurbInSalePeriod.csv")



output <- ggplot(sm2, aes(x = as.Date(quarters), y = priceFinal, colour = noTurbineBuiltinSalePeriod)) +
  #geom_point() +
  #scale_y_log10() +
  stat_summary(fun.y = mean, aes(fill = noTurbineBuiltinSalePeriod), geom="point",size=2) +
  #stat_summary(fun.y = mean, aes(fill = noTurbineBuiltinSalePeriod), geom="line",size=1) +
  facet_wrap(~name, scales = 'free_y', ncol = 4)
  
output

ggsave("saves/hses_2km_onesWithSaleBeforeAfterTurb_vs_notMean2.png", output, dpi=150, width = 15, height = 15)
#ggsave("saves/hses_2km_onesWithSaleBeforeAfterTurb_vs_notMedian.png", output, dpi=150, width = 14, height = 12)

#~~~~~~
#Subset 
output <- ggplot(sm2 %>% filter(name %in% c('North Lanarkshire','South Lanarkshire')), 
                 aes(x = as.Date(quarters), y = priceFinal, colour = noTurbineBuiltinSalePeriod)) +
  #geom_point() +
  #scale_y_log10() +
  stat_summary(fun.y = mean, aes(fill = noTurbineBuiltinSalePeriod), geom="point",size=2) +
  #stat_summary(fun.y = mean, aes(fill = noTurbineBuiltinSalePeriod), geom="line",size=1) +
  facet_wrap(~name, scales = 'free_y')

output

ggsave("saves/hses_2km_onesWithSaleBeforeAfterTurb_vs_notMean_subset1.png", output, dpi=150, width = 11, height = 4)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PRICE CHANGE COMPARISONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Various functions for this section

#FUNCTION
#vector of dates for the turbines. I want the modal date
#for places with many turbines within 2km
returnModalDate <- function(turbs){
  
  if(turbs==""){
    
    #print("returning NA")
    modez = NA
    
  } else {
  
  #turn turbs into list
  turblist <- strsplit(as.character(turbs),"\\|")[[1]]
  #print(turblist)
  
  datez <- tb$quarters[tb$index %in% turblist]
  
  #via NCmisc
  #If no single mode, select earliest date
  modez <- Mode(as.numeric(datez), multi = T)
  
  }
  
  if(length(modez) > 1) {
    #print("returning min date")
    return(min(modez))
  } else {
    #print("returning mode")
    return(modez)
  }
  
}

#FUNCTION
#Bootstrap sample a vector. Return means and sample size
#Return list containing the means and a 95% CI summary
#sampleSizes: vector of sample sizes to get means for 
#repeats: number of times to get mean from sample of size sampleSize
#replace: false is default for standard bootstrap but true useful also
bootstrap <- function(vectr, sampleSizes, repeats = 5000, repl = F) {
  
meanz <- data.frame(means = as.numeric(), sampleSize = as.numeric())

  #for range of sample sizes
  for(i in sampleSizes) {
    
    print(i)
    
    #sample size i, get that sample 5000 times
    add <- data.frame(
      means = sapply(seq(1:repeats), function(x) 
        mean(sample(vectr, i, replace = repl))),
      sampleSize = i)
    
    meanz <- rbind(meanz,add)
    
  }

  summaryz <- meanz %>% group_by(sampleSize) %>% 
    summarise(mean = mean(means), 
    min = quantile(means,c(0.025,0.975))[[1]],
    max = quantile(means,c(0.025,0.975))[[2]])

  return(list(meanz,summaryz))

}



#Data
#Includes yearmon and quarters
hs_geo <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes_oldnewboth.rds")

tb <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns.csv")
tb$quarters <- as.yearqtr(as.Date(tb$statusDateFormatted))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Process turbines in distance radii----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#turbines within distance radii (loading as results come in...)
hs_turbDistradii <- readRDS("data/hses_plus_turbinesWithinAllDistanceRADII_upTo15000.rds")

#add turbine modal dates to the hs_turb file, so can then merge directly

#for each turb col
turbcols <- names(hs_turbDistradii)[grepl("tbswithin", names(hs_turbDistradii))]

#sample test
#samplz <- hs_turbDistradii[sample(1:nrow(hs_turbDistradii),1000),]

#running for later data. Alligator. Done up to 6K already
#for(i in turbcols){
for(i in turbcols[7:15]){
  
  print(i)

  #Slightly inefficient esp at lower distances, applying to all rows even if empty...
  hs_turbDistradii$temp <- lapply(hs_turbDistradii[,c(i)], function(x) returnModalDate(x))
  names(hs_turbDistradii)[names(hs_turbDistradii) == 'temp'] <- paste0('modalDate_',i)
  
  #sample test
  #samplz$temp <- lapply(samplz[,c(i)], function(x) returnModalDate(x))
  #names(samplz)[names(samplz) == 'temp'] <- paste0('modalDate_',i)
  
   
}

#save!
#saveRDS(hs_turbDistradii, "data/houses_withModalDate_turbDistRADII2.rds")

#reload, add columns already done to hs_turbDistradii
#Done... 
hs_turbDistradii <- readRDS("data/houses_withModalDate_turbDistRADII2.rds")
#hs_turbDistradii <- cbind(hs_turbDistradii,addz[,c(8:13)])

#convert quarters to quarters...
datecols <- names(hs_turbDistradii)[grepl("modalDate", names(hs_turbDistradii))]

#uh oh: list...
hs_turbDistradii[,c(datecols)] <- apply(hs_turbDistradii[,c(datecols)],2, function(x) unlist(x))
#hs_turbDistradii[,c(datecols)] <- apply(hs_turbDistradii[,c(datecols)],2, function(x) as.yearqtr(x))
#hs_turbDistradii[,c(datecols[1])] <- as.yearqtr(hs_turbDistradii[,c(datecols[1])])

#Let's just wait on that. Might not be necessary for comparison
#(and fills NAs with orrible NA QNAs)
#These two disagree. Hmm. Oh well.
apply(hs_turbDistradii, 2, class)
hs_turbDistradii[,c('modalDate_tbswithin_6000km')] %>% class

#save again!
#saveRDS(hs_turbDistradii, "data/houses_withModalDate_turbDistradii2.rds")

#~~~~~~~~~
#Mean prices by year by distance radii----

#Do for all dates first (easier working on single property list)
distanceRadiiCounts <- hs_turbDistradii[,c(datecols)] %>% lapply(function(x) table(!is.na(x))[[2]]) %>% unlist

plot(distanceRadiiCounts)

#Then: how many per distance Radii per quarter? (Or possibly year too)
#For that: merge modal turb dates into the sales
sales_plus_turbradii <- merge(hs_geo,hs_turbDistradii %>% dplyr::select(Title:tbswithin_15000km),
                              by = 'Title', all.x = T)

#change turb cols to plain ol' flags
sales_plus_turbradii[,c(grepl("tbswithin",names(sales_plus_turbradii)))] <- 
  sales_plus_turbradii[,c(grepl("tbswithin",names(sales_plus_turbradii)))] %>% 
  apply(2,function(x) 0 + (x!=""))

#Use year for averaging price first... let's see what that looks like
sales_plus_turbradii$year <- format(sales_plus_turbradii$yearmon, "%Y")

#That's taken a while - save:
saveRDS(sales_plus_turbradii,"data/temp/sales_plus_turbRADII.rds")
sales_plus_turbradii <- readRDS("data/temp/sales_plus_turbRADII.rds")

rndPal <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
  "#8A7C64", "#599861"
)

#See about making this long for summarising. Reduce columns:
avPrice_perYear_perDistRadiiYesNo <- sales_plus_turbradii %>% 
  dplyr::select(priceFinal,year,tbswithin_1000km:tbswithin_15000km) %>% 
  gather(distanceRadii,yesNo,tbswithin_1000km:tbswithin_15000km)

#Keep only yeses (for now)
avPrice_perYear_perDistRadiiYesNo <- avPrice_perYear_perDistRadiiYesNo %>% filter(yesNo == 1)

avPrice_perYear_perDistRadiiYesNoSummary <- avPrice_perYear_perDistRadiiYesNo %>% 
  group_by(year,distanceRadii) %>% 
  summarise(mean = mean(priceFinal))

avPrice_perYear_perDistRadiiYesNoSummary$lowest <- "1-5km" 
# avPrice_perYear_perDistRadiiYesNoSummary$lowest[avPrice_perYear_perDistRadiiYesNoSummary$distanceRadii %in% 
#                                           c('tbswithin_1000km','tbswithin_2000km','tbswithin_3000km')] <- 1
avPrice_perYear_perDistRadiiYesNoSummary$lowest[avPrice_perYear_perDistRadiiYesNoSummary$distanceRadii %in% 
                                          c('tbswithin_6000km','tbswithin_7000km','tbswithin_8000km',
                                            'tbswithin_9000km','tbswithin_10000km')] <- "6-10km"
avPrice_perYear_perDistRadiiYesNoSummary$lowest[avPrice_perYear_perDistRadiiYesNoSummary$distanceRadii %in% 
                                          c('tbswithin_11000km','tbswithin_12000km','tbswithin_13000km',
                                            'tbswithin_14000km','tbswithin_15000km')] <- "11-15km"

avPrice_perYear_perDistRadiiYesNoSummary$lowest <- factor(avPrice_perYear_perDistRadiiYesNoSummary$lowest,
                                                         levels = c('1-5km','6-10km','11-15km'))

#break into time chunks and facet
#avPrice_perYear_perDistRadiiYesNoSummary$period <- 1
# avPrice_perYear_perDistRadiiYesNoSummary$period[avPrice_perYear_perDistRadiiYesNoSummary$year < 2001] <- 1
# avPrice_perYear_perDistRadiiYesNoSummary$period[avPrice_perYear_perDistRadiiYesNoSummary$year > 2000
#                                                & avPrice_perYear_perDistRadiiYesNoSummary$year < 2008] <- 2
# avPrice_perYear_perDistRadiiYesNoSummary$period[avPrice_perYear_perDistRadiiYesNoSummary$year > 2007
#                                                & avPrice_perYear_perDistRadiiYesNoSummary$year < 2016] <- 3

avPrice_perYear_perDistRadiiYesNoSummary$period <- 
  as.numeric(cut_number(as.numeric(avPrice_perYear_perDistRadiiYesNoSummary$year),3))


output <- ggplot(avPrice_perYear_perDistRadiiYesNoSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                                                       y = mean, colour = distanceRadii, 
                                                       linetype = factor(lowest))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~
#Year diffs----
diffs_years <- avPrice_perYear_perDistRadiiYesNoSummary %>% group_by(distanceRadii) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

chk2 <- diffs_years %>% filter(distanceRadii == 'tbswithin_1000km')

#just plot n see?
output <- ggplot(diffs_years, aes(x = as.Date(paste0(year,'-01-01')),
                                     y = diffperc, colour = distanceRadii,
                                     #y = diff, colour = distanceRadii,
                                     linetype = factor(lowest))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  scale_colour_manual(values=rndPal)
#facet_wrap(~period + lowest, scales = 'free', ncol = 3)

output


#~~~~~~~~~~~~~~~~~~~~~~~~
#count per year Radii-----
countz <- avPrice_perYear_perDistRadiiYesNo %>% group_by(year,distanceRadii) %>% 
  summarise(count = n())

output <- ggplot(countz, aes(x = as.Date(paste0(year,'-01-01')), 
                             y = count, colour = distanceRadii)) +
  geom_point()

output



#XXXFor a given distance Radii, get a before/after flag for property sale dates
#XXXhs_geo_2km$saleAfter <- ifelse(hs_geo_2km$quarters > hs_geo_2km$turbQuarters, 1,0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat all that for quarters-----
avPrice_perQ_perDistRadiiYesNo2 <- sales_plus_turbradii %>% 
  dplyr::select(priceFinal,quarters,tbswithin_1000km:tbswithin_15000km) %>% 
  gather(distanceRadii,yesNo,tbswithin_1000km:tbswithin_15000km)

#Keep only yeses (for now)
avPrice_perQ_perDistRadiiYesNo2 <- avPrice_perQ_perDistRadiiYesNo2 %>% filter(yesNo == 1)

avPrice_perQ_perDistRadiiYesNo2Summary <- avPrice_perQ_perDistRadiiYesNo2 %>% 
  group_by(quarters,distanceRadii) %>% 
  summarise(mean = mean(priceFinal))

avPrice_perQ_perDistRadiiYesNo2Summary$lowest <- "1-5km" 
# avPrice_perQ_perDistRadiiYesNo2Summary$lowest[avPrice_perQ_perDistRadiiYesNo2Summary$distanceRadii %in% 
#                                           c('tbswithin_1000km','tbswithin_2000km','tbswithin_3000km')] <- 1
avPrice_perQ_perDistRadiiYesNo2Summary$lowest[avPrice_perQ_perDistRadiiYesNo2Summary$distanceRadii %in% 
                                                 c('tbswithin_6000km','tbswithin_7000km','tbswithin_8000km',
                                                   'tbswithin_9000km','tbswithin_10000km')] <- "6-10km"
avPrice_perQ_perDistRadiiYesNo2Summary$lowest[avPrice_perQ_perDistRadiiYesNo2Summary$distanceRadii %in% 
                                                 c('tbswithin_11000km','tbswithin_12000km','tbswithin_13000km',
                                                   'tbswithin_14000km','tbswithin_15000km')] <- "11-15km"

avPrice_perQ_perDistRadiiYesNo2Summary$lowest <- factor(avPrice_perQ_perDistRadiiYesNo2Summary$lowest,
                                                         levels = c('1-5km','6-10km','11-15km'))

#break into time chunks and facet
avPrice_perQ_perDistRadiiYesNo2Summary$period <- 
  as.numeric(cut_number(as.numeric(avPrice_perQ_perDistRadiiYesNo2Summary$quarters),3))

output <- ggplot(avPrice_perQ_perDistRadiiYesNo2Summary, aes(x = as.Date(quarters), 
                                                              y = mean, colour = distanceRadii, 
                                                              linetype = factor(lowest))) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  facet_wrap(~period, scales = 'free', ncol = 1)
  #facet_wrap(~period + lowest, scales = 'free', ncol = 3)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Diffs for quarters / turb distance radii----

diffs_quarters <- avPrice_perQ_perDistRadiiYesNo2Summary %>% group_by(distanceRadii) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

chk <- diffs_quarters %>% filter(distanceRadii == 'tbswithin_1000km')

#just plot n see?
output <- ggplot(diffs_quarters, aes(x = as.Date(quarters),
                                     y = diffperc, colour = distanceRadii,
                                     #y = diff, colour = distanceRadii,
                                     linetype = factor(lowest))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  scale_colour_manual(values=rndPal)
#facet_wrap(~period + lowest, scales = 'free', ncol = 3)

output


#~~~~~~~~~~~~~~~~~~~~~~
#count per quarter Radii----
countz_q <- avPrice_perQ_perDistRadiiYesNo2 %>% group_by(quarters,distanceRadii) %>% 
  summarise(count = n())

output <- ggplot(countz_q, aes(x = as.Date(quarters), 
                             y = count, colour = distanceRadii)) +
  geom_point()

output

#lowest values?
just1000 <- countz_q[countz_q$distanceRadii == 'tbswithin_1000km',3]

output <- ggplot(countz_q, aes(x = distanceRadii, 
                               y = count)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0))

output

#~~~~~~~~~~~~~~~~~~~
#What's number of sales per Radii, as a proportion of the number of properties?----
thing <- sales_plus_turbradii %>% 
  dplyr::select(Title,priceFinal,tbswithin_1000km:tbswithin_15000km) %>% 
  gather(distanceRadii,yesNo,tbswithin_1000km:tbswithin_15000km)

thing <- thing %>% dplyr::filter(yesNo == 1)

#number of properties per distance vs number of sales
numberz <- merge(
  thing %>% distinct(Title, distanceRadii) %>% 
    group_by(distanceRadii) %>% 
    summarise(countProperties = n()),
  thing %>% group_by(distanceRadii) %>% 
    summarise(countSales = n()),
  by = 'distanceRadii'
)

#handily, this gives correct order for cumsum... 
numberz$sales_over_properties <- (numberz$countSales/numberz$countProperties)
numberz <- numberz %>% arrange(sales_over_properties)

#to order by distance
numberz$db_numeric <- as.numeric(gsub("[^0-9]", "", numberz$distanceRadii))
numberz <- numberz %>% arrange(db_numeric)

plot(numberz$sales_over_properties)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat all that for distance BANDS (not radii)-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#turbines within distance radii (loading as results come in...)
hs_turbDistBANDS <- readRDS("data/hses_plus_turbinesWithinAllDistanceBANDS_upTo15000.rds")

#Forgot to rename cols to make it clear they're bands. Let's do that here.
names(hs_turbDistBANDS)[2:ncol(hs_turbDistBANDS)] <- 
  sapply(seq(1:(ncol(hs_turbDistBANDS)-1)), function(x) paste0("tbs_",x-1,"to",x,"km"))


#add turbine modal dates to the hs_turb file, so can then merge directly
#for each turb col
turbcols2 <- names(hs_turbDistBANDS)[grepl("tbs_", names(hs_turbDistBANDS))]

#sample test
#samplz <- hs_turbDistradii[sample(1:nrow(hs_turbDistradii),1000),]

#running for later data. Alligator. Done up to 6K already
for(i in turbcols2){
# for(i in turbcols2[7:15]){
  
  print(i)
  
  #Slightly inefficient esp at lower distances, applying to all rows even if empty...
  hs_turbDistBANDS$temp <- lapply(hs_turbDistBANDS[,c(i)], function(x) returnModalDate(x))
  names(hs_turbDistBANDS)[names(hs_turbDistBANDS) == 'temp'] <- paste0('modalDate_',i)
  
  #sample test
  #samplz$temp <- lapply(samplz[,c(i)], function(x) returnModalDate(x))
  #names(samplz)[names(samplz) == 'temp'] <- paste0('modalDate_',i)
  
  
}

#convert quarters to quarters...
datecols2 <- names(hs_turbDistBANDS)[grepl("modalDate", names(hs_turbDistBANDS))]

#uh oh: list...
hs_turbDistBANDS2 <- hs_turbDistBANDS

hs_turbDistBANDS2[,c(datecols2)] <- apply(hs_turbDistBANDS2[,c(datecols2)],2, function(x) unlist(x))

#save!
saveRDS(hs_turbDistBANDS2, "data/houses_withModalDate_turbDistBANDS.rds")
hs_turbDistBANDS <- readRDS("data/houses_withModalDate_turbDistBANDS.rds")

#count of properties in each dist band. Think I have this already, don't know where.
countProps <- hs_turbDistBANDS %>% dplyr::select(tbs_0to1km:tbs_14to15km) %>% 
  mutate_each(funs(.!="")) %>% 
  gather(distanceBand, flag, tbs_0to1km:tbs_14to15km) %>% 
  filter(flag == T) %>% 
  group_by(distanceBand) %>% 
  summarise(count = n())
  
sum(countProps$count)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean prices by year by distance BAND----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Do for all dates first (easier working on single property list)
distanceBANDSCounts <- hs_turbDistBANDS[,c(turbcols2)] %>% lapply(function(x) table(x!="")[[2]]) %>% unlist

plot(distanceBANDSCounts)


#For distance bands, what's the growth in the number of their bands that have at least one turbine in?
#Actually, that's rather more faff than I can do with right now.
#distanceBANDSCounts_by_quarter <- 


#Then: how many per distance BANDS per year...?
#For that: merge modal turb dates into the sales
sales_plus_turbBANDS <- merge(hs_geo,hs_turbDistBANDS %>% dplyr::select(Title:tbs_14to15km),
                              by = 'Title', all.x = T)

#change turb cols to plain ol' flags
sales_plus_turbBANDS[,c(grepl("tbs",names(sales_plus_turbBANDS)))] <- 
  sales_plus_turbBANDS[,c(grepl("tbs",names(sales_plus_turbBANDS)))] %>% 
  apply(2,function(x) 0 + (x!=""))

#Use year for averaging price first... let's see what that looks like
sales_plus_turbBANDS$year <- format(sales_plus_turbBANDS$yearmon, "%Y")

#That's taken a while - save:
saveRDS(sales_plus_turbBANDS,"data/temp/sales_plus_turbBANDS.rds")
sales_plus_turbBANDS <- readRDS("data/temp/sales_plus_turbBANDS.rds")

rndPal <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
  "#8A7C64", "#599861"
)

#See about making this long for summarising. Reduce columns:
avPrice_perYear_perDistBANDSYesNo <- sales_plus_turbBANDS %>% 
  dplyr::select(priceFinal,year,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBANDS,yesNo,tbs_0to1km:tbs_14to15km)

#Keep only yeses (for now)
avPrice_perYear_perDistBANDSYesNo <- avPrice_perYear_perDistBANDSYesNo %>% filter(yesNo == 1)

avPrice_perYear_perDistBANDSYesNoSummary <- avPrice_perYear_perDistBANDSYesNo %>% 
  group_by(year,distanceBANDS) %>% 
  summarise(mean = mean(priceFinal))

avPrice_perYear_perDistBANDSYesNoSummary$lowest <- "1-5km" 
# avPrice_perYear_perDistBANDSYesNoSummary$lowest[avPrice_perYear_perDistBANDSYesNoSummary$distanceBANDS %in% 
#                                           c('tbswithin_1000km','tbswithin_2000km','tbswithin_3000km')] <- 1
avPrice_perYear_perDistBANDSYesNoSummary$lowest[avPrice_perYear_perDistBANDSYesNoSummary$distanceBANDS %in% 
                                                  turbcols2[6:10]] <- "6-10km"
avPrice_perYear_perDistBANDSYesNoSummary$lowest[avPrice_perYear_perDistBANDSYesNoSummary$distanceBANDS %in% 
                                                  turbcols2[11:15]] <- "11-15km"

avPrice_perYear_perDistBANDSYesNoSummary$lowest <- factor(avPrice_perYear_perDistBANDSYesNoSummary$lowest,
                                                          levels = c('1-5km','6-10km','11-15km'))



avPrice_perYear_perDistBANDSYesNoSummary$period <- 
  as.numeric(cut_number(as.numeric(avPrice_perYear_perDistBANDSYesNoSummary$year),3))


#add an overall mean too, for comparison across separated facet
#Naw - can't add full facet vars, obv.
meanz <- avPrice_perYear_perDistBANDSYesNoSummary %>% group_by(year) %>% 
  summarise(mean = mean(mean), period = max(period))

output <- ggplot() +
  geom_line(data = avPrice_perYear_perDistBANDSYesNoSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                  y = mean, colour = distanceBANDS, 
                  linetype = factor(lowest)),
    alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  scale_color_manual(values = rndPal)

output

output <- ggplot() +
  geom_line(data = avPrice_perYear_perDistBANDSYesNoSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                  y = mean, colour = distanceBANDS, 
                  linetype = factor(lowest)),
    alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~period+lowest, scales = 'free', ncol = 3) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~
#Year diffs----
diffs_yearsBANDS <- avPrice_perYear_perDistBANDSYesNoSummary %>% group_by(distanceBANDS) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

chk3 <- diffs_yearsBANDS %>% filter(distanceBANDS == 'tbswithin_1000km')

#just plot n see?
output <- ggplot(diffs_yearsBANDS, aes(x = as.Date(paste0(year,'-01-01')),
                                  y = diffperc, colour = distanceBANDS,
                                  #y = diff, colour = distanceBANDS,
                                  linetype = factor(lowest))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  #facet_wrap(~period + lowest, scales = 'free', ncol = 3) +
  scale_colour_manual(values=rndPal)

output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean prices by year by distance BAND, comparing yes/no version----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reduce columns:
avPrice_perYear_perDistBANDSYesNoKEEP <- sales_plus_turbBANDS %>% 
  dplyr::select(priceFinal,year,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBANDS,yesNo,tbs_0to1km:tbs_14to15km)


avPrice_perYear_perDistBANDSYesNoKEEPSummary <- avPrice_perYear_perDistBANDSYesNoKEEP %>% 
  group_by(year,distanceBANDS, yesNo) %>% 
  summarise(mean = mean(priceFinal))


output <- ggplot() +
  geom_line(data = avPrice_perYear_perDistBANDSYesNoKEEPSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                                                                 y = mean, colour = factor(yesNo)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, scales = 'free', ncol = 3) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Year diffs, per distance band----
diffs_yearsBAND_YESNO <- avPrice_perYear_perDistBANDSYesNoKEEPSummary %>% group_by(distanceBANDS,yesNo) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

head(diffs_yearsBAND_YESNO)

#add period marker for later
diffs_yearsBAND_YESNO$period <- 
  as.numeric(cut_number(as.numeric(diffs_yearsBAND_YESNO$year),3))

#just plot n see?
output <- ggplot(diffs_yearsBAND_YESNO, aes(x = as.Date(paste0(year,'-01-01')),
                                       y = diffperc, colour = factor(yesNo))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, scales = 'free', ncol = 3) +
  scale_colour_manual(values=rndPal)

output

#hmm. What's mean diff for those two groups?
meanDiff <- diffs_yearsBAND_YESNO %>% filter(!is.na(diff)) %>% 
  group_by(distanceBANDS,yesNo) %>% 
  summarise(mean = mean(diffperc))

head(meanDiff)

output <- ggplot(meanDiff, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
  geom_point(size = 2)
  
output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Year mean diff for three time periods----
meanDiff_z <- diffs_yearsBAND_YESNO %>% filter(!is.na(diff)) %>% 
  group_by(distanceBANDS,yesNo,period) %>% 
  summarise(mean = mean(diffperc), minyear = min(year), maxyear = max(year))

head(meanDiff_z)

output <- ggplot(meanDiff_z, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
  geom_point(size = 2) +
  facet_wrap(~period)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quick explore of houses in distance bands----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Is there anything odd about sales in the 0-1km band?
sales_plus_turbBANDS <- readRDS("data/temp/sales_plus_turbBANDS.rds")

bandOne <- sales_plus_turbBANDS %>% filter(tbs_0to1km == 1) %>% arrange(-priceFinal)

#Nothing I can see. Let's look in map...
#Only need properties
hs_forlook <- hs_turbDistBANDS %>% dplyr::select(tbs_0to1km:tbs_14to15km) %>% 
  mutate_each(funs(ifelse(.=="",0,1))) %>% cbind(hs_turbDistBANDS[,1, drop = F])

#Merge in geocodes... err, why did that shrink?
hs_forlook <- merge(hs_forlook,unique(sales_plus_turbBANDS[,c(1,4,5)]), by = 'Title')

#get geoc
#geoc <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/original/allHouses_CEDArun.csv")
#hs_forlook <- merge(hs_forlook,geoc[,c(2,3,4)],by = 'Title')

#Actually... thinking about it again, the sales number was reduced by filtering.
#Should use them.
#write.csv(hs_forlook,"data/temp/hs_forLook.csv")

#Actually let's write separate CSVs for houses in each band
for(i in turbcols2[1:15]){
  write.csv(hs_forlook[hs_forlook[,c(i)]==1,c(1,17,18)], 
            paste0("data/temp/hs_tb_distbands/",i,".csv"))
}


#Ah, wait. THE DATES.
dateChecks <- sales_plus_turbBANDS %>% dplyr::select(date,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBAND,yesNo,tbs_0to1km:tbs_14to15km)

#Keep only those in the distance bands
dateChecks <- dateChecks %>% filter(yesNo == 1)

#distance band date bias?
output <- ggplot(dateChecks, aes(x = distanceBAND, y = date)) +
  geom_boxplot()

output

#I've done prices, but...
priceChecks <- sales_plus_turbBANDS %>% dplyr::select(priceFinal,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBAND,yesNo,tbs_0to1km:tbs_14to15km)

#Keep only those in the distance bands
priceChecks <- priceChecks %>% filter(yesNo == 1)

#distance band date bias?
output <- ggplot(priceChecks, aes(x = distanceBAND, y = priceFinal)) +
  geom_boxplot() +
  scale_y_log10() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = F)
  

output

#Just check I got the right data there. Count in each distance band group is...?
#Yip.
countz <- dateChecks %>% group_by(distanceBAND) %>% 
  summarise(count = n())

plot(countz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean prices by QUARTER by distance BAND----

#Reduce columns:
avPrice_perQTR_perDistBANDSYesNo <- sales_plus_turbBANDS %>% 
  dplyr::select(priceFinal,quarters,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBANDS,yesNo,tbs_0to1km:tbs_14to15km)

#Keep only yeses (for now)
avPrice_perQTR_perDistBANDSYesNo <- avPrice_perQTR_perDistBANDSYesNo %>% filter(yesNo == 1)

avPrice_perQTR_perDistBANDSYesNoSummary <- avPrice_perQTR_perDistBANDSYesNo %>% 
  group_by(quarters,distanceBANDS) %>% 
  summarise(mean = mean(priceFinal))

avPrice_perQTR_perDistBANDSYesNoSummary$lowest <- "1-5km" 
# avPrice_perQTR_perDistBANDSYesNoSummary$lowest[avPrice_perQTR_perDistBANDSYesNoSummary$distanceBANDS %in% 
#                                           c('tbswithin_1000km','tbswithin_2000km','tbswithin_3000km')] <- 1
avPrice_perQTR_perDistBANDSYesNoSummary$lowest[avPrice_perQTR_perDistBANDSYesNoSummary$distanceBANDS %in% 
                                                  turbcols2[6:10]] <- "6-10km"
avPrice_perQTR_perDistBANDSYesNoSummary$lowest[avPrice_perQTR_perDistBANDSYesNoSummary$distanceBANDS %in% 
                                                  turbcols2[11:15]] <- "11-15km"

avPrice_perQTR_perDistBANDSYesNoSummary$lowest <- factor(avPrice_perQTR_perDistBANDSYesNoSummary$lowest,
                                                          levels = c('1-5km','6-10km','11-15km'))



avPrice_perQTR_perDistBANDSYesNoSummary$period <- 
  as.numeric(cut_number(as.numeric(avPrice_perQTR_perDistBANDSYesNoSummary$quarters),3))


#add an overall mean too, for comparison across separated facet
#Naw - can't add full facet vars, obv.
meanz <- avPrice_perQTR_perDistBANDSYesNoSummary %>% group_by(quarters) %>% 
  summarise(mean = mean(mean), period = max(period))

output <- ggplot() +
  geom_line(data = avPrice_perQTR_perDistBANDSYesNoSummary, aes(x = as.Date(quarters), 
                  y = mean, colour = distanceBANDS),
                  #linetype = factor(lowest)),
    alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(quarters), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  scale_color_manual(values = rndPal)

output

output <- ggplot() +
  geom_line(data = avPrice_perQTR_perDistBANDSYesNoSummary, aes(x = as.Date(quarters), 
                  y = mean, colour = distanceBANDS),
                  #linetype = factor(lowest)),
    alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(quarters), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~period+lowest, scales = 'free', ncol = 3) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~
#Quarter diffs----
diffs_yearsBANDS_qtrs <- avPrice_perQTR_perDistBANDSYesNoSummary %>% group_by(distanceBANDS) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

chk3 <- diffs_yearsBANDS_qtrs %>% filter(distanceBANDS == 'tbswithin_1000km')

output <- ggplot(diffs_yearsBANDS_qtrs, aes(x = as.Date(quarters),
                                  y = diffperc, colour = distanceBANDS)) +
                                  #y = diff, colour = distanceBANDS,
                                  #linetype = factor(lowest))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  #facet_wrap(~period + lowest, scales = 'free', ncol = 3) +
  scale_colour_manual(values=rndPal)

output



#~~~~~~~~~~~~~~~~~~~~~~
#count per quarter band again----
countz_q <- avPrice_perQTR_perDistBANDSYesNo %>% group_by(quarters,distanceBANDS) %>% 
  summarise(count = n())

countz_q$group <- as.numeric(cut_interval(as.numeric(countz_q$distanceBANDS),4))

output <- ggplot(countz_q, aes(x = as.Date(quarters), 
                               y = count, colour = distanceBANDS)) +
  geom_point()

output

#lowest band
output <- ggplot(countz_q %>% filter(distanceBANDS == 'tbs_0to1km'), aes(x = as.Date(quarters), 
                               y = count, colour = distanceBANDS)) +
  geom_point() +
  expand_limits(y = 0)

output

output <- ggplot(countz_q, aes(x = as.Date(quarters), 
                               y = count, colour = distanceBANDS)) +
  geom_point() +
  facet_wrap(~group, scales = 'free')

output

#lowest values?
just1000 <- countz_q[countz_q$distanceBANDS == 'tbs_0to1km',3]

output <- ggplot(countz_q, aes(x = distanceBANDS, 
                               y = count)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0))

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mean prices by QUARTER by distance BAND, comparing yes/no version----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reduce columns:
avPrice_perQTR_perDistBANDSYesNoKEEP <- sales_plus_turbBANDS %>% 
  dplyr::select(priceFinal,quarters,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBANDS,yesNo,tbs_0to1km:tbs_14to15km)


avPrice_perQTR_perDistBANDSYesNoKEEPSummary <- avPrice_perQTR_perDistBANDSYesNoKEEP %>% 
  group_by(quarters,distanceBANDS, yesNo) %>% 
  summarise(mean = mean(priceFinal))


output <- ggplot() +
  geom_line(data = avPrice_perQTR_perDistBANDSYesNoKEEPSummary, aes(x = as.Date(quarters), 
                                                                     y = mean, colour = factor(yesNo)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, scales = 'free', ncol = 3) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#QUARTER diffs, per distance band----
diffs_QTRS_BAND_YESNO <- avPrice_perQTR_perDistBANDSYesNoKEEPSummary %>% group_by(distanceBANDS,yesNo) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

head(diffs_QTRS_BAND_YESNO)

#add period marker for later
diffs_QTRS_BAND_YESNO$period <- 
  as.numeric(cut_number(as.numeric(diffs_QTRS_BAND_YESNO$quarters),3))

#just plot n see?
output <- ggplot(diffs_QTRS_BAND_YESNO, aes(x = as.Date(quarters),
                                            y = diffperc, colour = factor(yesNo))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, scales = 'free', ncol = 3) +
  scale_colour_manual(values=rndPal)

output

#hmm. What's mean diff for those two groups?
meanDiff <- diffs_QTRS_BAND_YESNO %>% filter(!is.na(diff)) %>% 
  group_by(distanceBANDS,yesNo) %>% 
  summarise(mean = mean(diffperc))

head(meanDiff)

output <- ggplot(meanDiff, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
  geom_point(size = 2)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#QUARTER mean diff for three time periods----
meanDiff_z <- diffs_QTRS_BAND_YESNO %>% filter(!is.na(diff)) %>% 
  group_by(distanceBANDS,yesNo,period) %>% 
  summarise(mean = mean(diffperc), minqtr = min(quarters), maxqtr = max(quarters))

head(meanDiff_z)

output <- ggplot(meanDiff_z, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
  geom_point(size = 2) +
  facet_wrap(~period)

output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BOOTSTRAP: % diff between quarters, compare km BANDS to same-sample-size random-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#First: get the means for the % diffs as we did before.
#That's: the mean % diff for all timesteps.

#function for getting mean % diffs for different samples
meanDiffs_distanceBANDS <- function(df, timeField, filt){
  
  one <- df %>% 
    dplyr::select_('priceFinal',timeField,'tbs_0to1km:tbs_14to15km') %>% 
    gather(distanceBANDS,flag,tbs_0to1km:tbs_14to15km) %>% 
    filter_(filt) %>% 
    dplyr::select(1:3)
  
  two <- one %>% 
    group_by_(timeField,'distanceBANDS') %>% 
    summarise(mean = mean(priceFinal))
  
  three <- two %>% group_by(distanceBANDS) %>% 
    mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)
  
  #test filtering first two years out
#   if(timeField == 'year') {
#     three <- three %>% filter(year > 1991)
#   } else {
#     three <- three %>% filter(quarters > as.yearqtr(1991))
#   }
  
  four <- three %>% filter(!is.na(diff)) %>% 
    group_by(distanceBANDS) %>% 
    summarise(mean = mean(diffperc))
  
  return(four)
  
}

#time field as second arg, filter condition as third
meanDiffQTR_year <- meanDiffs_distanceBANDS(sales_plus_turbBANDS, 'year', 'flag == 1')

#Set a flag column selecting a random sample from each
#In place of "yes, this is in this band".
#So an equivalent-sized random sample
#And, yes, this is a bit convoluted to fit the same structure.
sales_plus_turbBANDScopy <- sales_plus_turbBANDS

#Test: filter down to one season
sales_plus_turbBANDScopy <- sales_plus_turbBANDScopy %>% 
   filter((as.numeric(sales_plus_turbBANDScopy$quarters) %% 1) == 0)#winter

meanDiff_qtr_winter <- meanDiffs_distanceBANDS(sales_plus_turbBANDScopy, 'year', 'flag == 1')

#First: counts per distance band
countz <- sales_plus_turbBANDScopy %>% 
  dplyr::select(priceFinal,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBANDS,flag,tbs_0to1km:tbs_14to15km) %>% 
  filter(flag == 1) %>% 
  group_by(distanceBANDS) %>% 
  summarise(count = n()) %>% 
  data.frame


#OK, seems to work. Let's get a bunch. 
results_qtrtest3 <- data.frame(distanceBands = as.character(),mean = as.numeric())

for(j in 1:50) {

  sales_plus_turbBANDScopy <- sales_plus_turbBANDScopy %>% 
    dplyr::select(Title:priceFinal,quarters,year)

  #new column range flagging random picks
  for(i in seq(1:15)){
    
    pick <- sample(1:nrow(sales_plus_turbBANDScopy),countz[i,2])
    
    sales_plus_turbBANDScopy$temp <- 0
    sales_plus_turbBANDScopy$temp[pick] <- 1
    
    names(sales_plus_turbBANDScopy)[names(sales_plus_turbBANDScopy)=='temp'] <- turbcols2[i]
    
  }
  
  #Should match band count. Tick!
  #apply(sales_plus_turbBANDScopy[,c(6:20)],2,table)
  
  
  #Use that to get new diffs from those samples
  meanDiffQTR2 <- meanDiffs_distanceBANDS(sales_plus_turbBANDScopy, 'year', 'flag == 1')
  
  results_qtrtest3 <- rbind(results_qtrtest3,meanDiffQTR2)
  
  print(j)

}

#quarter, no adjustment
#Not the same data so don't use this!
# output <- ggplot() +
#   geom_point(data = results, aes(x = distanceBANDS, y = mean), size = 2) +
#   geom_point(data = meanDiffQTR, aes(x = distanceBANDS, y = mean), size = 8, colour = "red", alpha = .7) 
# 
# output

#quarter, removing first two years
output <- ggplot() +
  geom_point(data = results_qtrtest, aes(x = distanceBANDS, y = mean), size = 2) +
  geom_point(data = meanDiffQTR, aes(x = distanceBANDS, y = mean), size = 8, colour = "red", alpha = .7) 
  geom_point(size = 2) 
  
output

ggsave("saves/diffs/quarters_diff_minus19901991.png", output, dpi=150, width = 7, height = 5)

#quarter, just for summer, removing first two years
output <- ggplot() +
  geom_point(data = results_qtrtest2, aes(x = distanceBANDS, y = mean), size = 2) +
  geom_point(data = meanDiff_qtr_summer, aes(x = distanceBANDS, y = mean), size = 8, colour = "red", alpha = .7) 
geom_point(size = 2)

output

ggsave("saves/diffs/quarters_diff_minus19901991_summer.png", output, dpi=150, width = 7, height = 5)

#quarter, just for WINTER, removing first two years
output <- ggplot() +
  geom_point(data = results_qtrtest3, aes(x = distanceBANDS, y = mean), size = 2) +
  geom_point(data = meanDiff_qtr_winter, aes(x = distanceBANDS, y = mean), size = 8, colour = "red", alpha = .7) 
geom_point(size = 2)

output

ggsave("saves/diffs/quarters_diff_minus19901991_winter.png", output, dpi=150, width = 7, height = 5)

#year, no adjustment
# output <- ggplot(results3, aes(x = distanceBANDS, y = mean)) +
#   geom_point(size = 2)

output

#year, only after 1992
output <- ggplot() +
  geom_point(data = results_yeartest2, aes(x = distanceBANDS, y = mean), size = 2) +
  geom_point(data = meanDiffQTR_year, aes(x = distanceBANDS, y = mean), size = 8, colour = "red", alpha = .7) 
geom_point(size = 2)

output

ggsave("saves/diffs/years_diff_minus9091.png", output, dpi=150, width = 7, height = 5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat diff bootstraps for three time periods, save-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(dateType in c('quarters','year')) {

  #the three time periods (handily splits crash off in the last one)
  for(periodz in seq(1:3)) {
  
    sales_plus_turbBANDSperiod <- sales_plus_turbBANDS
    
    #reduce to each period
    sales_plus_turbBANDSperiod$period <- as.numeric(cut_interval(sales_plus_turbBANDSperiod$quarters %>% as.numeric,3))
    
    #check periods, use in titles
    datez <- sales_plus_turbBANDSperiod %>% group_by(period) %>% 
      summarise(minz = min(quarters), max = max(quarters))
    
    sales_plus_turbBANDSperiod <- sales_plus_turbBANDSperiod %>% 
      filter(period == periodz)
    
    actualDistBandMeans <- meanDiffs_distanceBANDS(sales_plus_turbBANDSperiod, dateType, 'flag == 1')
    
    #First: counts per distance band
    countz <- sales_plus_turbBANDSperiod %>% 
      dplyr::select(priceFinal,tbs_0to1km:tbs_14to15km) %>% 
      gather(distanceBANDS,flag,tbs_0to1km:tbs_14to15km) %>% 
      filter(flag == 1) %>% 
      group_by(distanceBANDS) %>% 
      summarise(count = n()) %>% 
      data.frame
    
    #OK, seems to work. Let's get a bunch. 
    results <- data.frame(distanceBands = as.character(),mean = as.numeric())
    
    for(j in 1:50) {
    
      sales_plus_turbBANDSperiod <- sales_plus_turbBANDSperiod %>% 
        dplyr::select(Title:priceFinal,quarters,year)
    
      #new column range flagging random picks
      for(i in seq(1:15)){
        
        pick <- sample(1:nrow(sales_plus_turbBANDSperiod),countz[i,2])
        
        sales_plus_turbBANDSperiod$temp <- 0
        sales_plus_turbBANDSperiod$temp[pick] <- 1
        
        names(sales_plus_turbBANDSperiod)[names(sales_plus_turbBANDSperiod)=='temp'] <- turbcols2[i]
        
      }
      
      #Should match band count. Tick!
      #apply(sales_plus_turbBANDSperiod[,c(6:20)],2,table)
      
      
      #Use that to get new diffs from those samples
      meanDiffQTR2 <- meanDiffs_distanceBANDS(sales_plus_turbBANDSperiod, dateType, 'flag == 1')
      
      results <- rbind(results,meanDiffQTR2)
      
      print(j)
    
    }
    
    #quarter, removing first two years
    output <- ggplot() +
      geom_point(data = results, aes(x = distanceBANDS, y = mean), size = 2) +
      geom_point(data = actualDistBandMeans, aes(x = distanceBANDS, y = mean), size = 8, colour = "red", alpha = .7) +
      ggtitle(paste0(datez[periodz,2]," to ",datez[periodz,3]," using ",dateType)) +
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    
    output
    
    ggsave(paste0("saves/diffs/",dateType,"_period",periodz,"diffx.png"), output, dpi=150, width = 10, height = 4)
    
  }#end for periodz
  
}#end for dateType
  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Distance bands: diffs for council areas-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sales_plus_turbBANDS <- readRDS("data/temp/sales_plus_turbBANDS.rds")
names(sales_plus_turbBANDS)

#See about making this long for summarising. Reduce columns:
avPrice_perYear_perDistBANDSYesNo_CA <- sales_plus_turbBANDS %>% 
  dplyr::select(priceFinal,year,councilArea,tbs_0to1km:tbs_14to15km) %>% 
  gather(distanceBANDS,yesNo,tbs_0to1km:tbs_14to15km)

head(avPrice_perYear_perDistBANDSYesNo_CA)
View(avPrice_perYear_perDistBANDSYesNo_CA)
nrow(avPrice_perYear_perDistBANDSYesNo_CA)

avPrice_perYear_perDistBANDSYesNoSummary_CA <- avPrice_perYear_perDistBANDSYesNo_CA %>% 
  group_by(year,councilArea,distanceBANDS) %>% 
  summarise(mean = mean(priceFinal))

head(avPrice_perYear_perDistBANDSYesNoSummary_CA)

avPrice_perYear_perDistBANDSYesNoSummary$lowest <- "1-5km" 
# avPrice_perYear_perDistBANDSYesNoSummary$lowest[avPrice_perYear_perDistBANDSYesNoSummary$distanceBANDS %in% 
#                                           c('tbswithin_1000km','tbswithin_2000km','tbswithin_3000km')] <- 1
avPrice_perYear_perDistBANDSYesNoSummary$lowest[avPrice_perYear_perDistBANDSYesNoSummary$distanceBANDS %in% 
                                                  turbcols2[6:10]] <- "6-10km"
avPrice_perYear_perDistBANDSYesNoSummary$lowest[avPrice_perYear_perDistBANDSYesNoSummary$distanceBANDS %in% 
                                                  turbcols2[11:15]] <- "11-15km"

avPrice_perYear_perDistBANDSYesNoSummary$lowest <- factor(avPrice_perYear_perDistBANDSYesNoSummary$lowest,
                                                          levels = c('1-5km','6-10km','11-15km'))



avPrice_perYear_perDistBANDSYesNoSummary$period <- 
  as.numeric(cut_number(as.numeric(avPrice_perYear_perDistBANDSYesNoSummary$year),3))


#add an overall mean too, for comparison across separated facet
#Naw - can't add full facet vars, obv.
meanz <- avPrice_perYear_perDistBANDSYesNoSummary %>% group_by(year) %>% 
  summarise(mean = mean(mean), period = max(period))

output <- ggplot() +
  geom_line(data = avPrice_perYear_perDistBANDSYesNoSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                                                                 y = mean, colour = distanceBANDS, 
                                                                 linetype = factor(lowest)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~period, scales = 'free', ncol = 1) +
  scale_color_manual(values = rndPal)

output

output <- ggplot() +
  geom_line(data = avPrice_perYear_perDistBANDSYesNoSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                                                                 y = mean, colour = distanceBANDS, 
                                                                 linetype = factor(lowest)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~period+lowest, scales = 'free', ncol = 3) +
  scale_color_manual(values = rndPal)

output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bootstrap: is the variability due to sample size?-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#get bootstrap means
#Returns list with means first then summary
b_meanz <- bootstrap(vectr = hs_geo$priceFinal, 
  sampleSizes = c(seq(from = 100, to = 1000, by = 100),seq(from = 1000, to = 20000, by = 500)),
  #sampleSizes = seq(from = 100, to = 1000, by = 100),
  repeats = 5000,
  repl = T)


ggplot(b_meanz[[2]], aes(x=sampleSize, y=mean/1000)) + 
  geom_errorbar(width = 0.1, colour = "blue", aes(ymin=min/1000, ymax=max/1000)) +
  geom_line() +
  ylab("thousands")
  #scale_y_log10() +
  geom_point() 

#plot all the points with summary over top
ggplot() + 
    geom_point(data = b_meanz[[1]], aes(x=sampleSize, y=means/1000), alpha = 0.01, size = 2) +
    geom_errorbar(data = b_meanz[[2]], 
                  width = 0.4, colour = "red", 
                  aes(x=sampleSize, ymin=min/1000, ymax=max/1000)) +
    ylab("thousands") +
    theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0))

  
#Repeat without replacement
#So to represent any random sample of sales from the full dataset
b_meanz2 <- bootstrap(vectr = hs_geo$priceFinal, 
                     sampleSizes = c(seq(from = 100, to = 1000, by = 100),seq(from = 1000, to = 20000, by = 500)),
                     repeats = 5000,
                     repl = F)



ggplot(b_meanz2[[2]], aes(x=sampleSize, y=mean/1000)) + 
  geom_errorbar(width = 0.1, colour = "blue", aes(ymin=min/1000, ymax=max/1000)) +
  geom_line() +
  ylab("thousands")
#scale_y_log10() +
geom_point()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Can-sees: years-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load output data. Can only use this directly to separate look at can-sees overall
#Can't split by date
#non-building-height
resultz <- read.csv("C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/output/allHouses_CEDArun.csv")

#recombine with price data
sales_plus_canSeeBands <- merge(hs_geo,resultz %>% 
                                  dplyr::select(Title,visibleInRadius.0to1km:visibleInRadius.14to15km),
                              by = 'Title', all.x = T)

#change turb cols to plain ol' flags
sales_plus_canSeeBands[,c(grepl("visible",names(sales_plus_canSeeBands)))] <- 
  sales_plus_canSeeBands[,c(grepl("visible",names(sales_plus_canSeeBands)))] %>% 
  apply(2,function(x) 0 + (x!=0))

#Use year for averaging price first... let's see what that looks like
sales_plus_canSeeBands$year <- format(sales_plus_canSeeBands$yearmon, "%Y")

#That's taken a while - save:
saveRDS(sales_plus_canSeeBands,"data/temp/sales_plus_canSeeBands.rds")
sales_plus_canSeeBands <- readRDS("data/temp/sales_plus_canSeeBands.rds")

#See about making this long for summarising. Reduce columns:
avPrice_perYear_perDist_CANSEEBANDS_YesNo <- sales_plus_canSeeBands %>% 
  dplyr::select(priceFinal,year,visibleInRadius.0to1km:visibleInRadius.14to15km) %>% 
  gather(distanceBANDS,yesNo,visibleInRadius.0to1km:visibleInRadius.14to15km)

#head(avPrice_perYear_perDist_CANSEEBANDS_YesNo)

avPrice_perYear_perDist_CANSEEBANDS_YesNoSummary <- avPrice_perYear_perDist_CANSEEBANDS_YesNo %>% 
  group_by(year,distanceBANDS,yesNo) %>% 
  summarise(mean = mean(priceFinal))

head(avPrice_perYear_perDist_CANSEEBANDS_YesNoSummary)


output <- ggplot() +
  geom_line(data = avPrice_perYear_perDist_CANSEEBANDS_YesNoSummary, aes(x = as.Date(paste0(year,'-01-01')), 
                                                                 y = mean, colour = factor(yesNo)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, scales = 'free', ncol = 3) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Can-sees: years, diff-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diffs_viz_yearsBANDS <- avPrice_perYear_perDist_CANSEEBANDS_YesNoSummary %>% group_by(distanceBANDS, yesNo) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

#chk3 <- diffs_viz_yearsBANDS %>% filter(distanceBANDS == 'tbswithin_1000km')

#just plot n see?
output <- ggplot(diffs_viz_yearsBANDS, aes(x = as.Date(paste0(year,'-01-01')),
                                       y = diffperc, colour = factor(yesNo))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, scales = 'free', ncol = 3) +
  scale_colour_manual(values=rndPal)

output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Can-sees: quarters-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#See about making this long for summarising. Reduce columns:
avPrice_perQTR_perDist_CANSEEBANDS_YesNo <- sales_plus_canSeeBands %>% 
  dplyr::select(priceFinal,quarters,visibleInRadius.0to1km:visibleInRadius.14to15km) %>% 
  gather(distanceBANDS,yesNo,visibleInRadius.0to1km:visibleInRadius.14to15km)

head(avPrice_perQTR_perDist_CANSEEBANDS_YesNo)

avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary <- avPrice_perQTR_perDist_CANSEEBANDS_YesNo %>% 
  group_by(quarters,distanceBANDS,yesNo) %>% 
  summarise(mean = mean(priceFinal))

head(avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary)


output <- ggplot() +
  geom_line(data = avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary, aes(x = as.Date(quarters), 
                                                                 y = mean, colour = factor(yesNo)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, ncol = 3) +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Can-sees: quarters, diff-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diffs_viz_QTR_BANDS <- avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary %>% group_by(distanceBANDS, yesNo) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

head(diffs_viz_QTR_BANDS)

#chk3 <- diffs_viz_QTR_BANDS %>% filter(distanceBANDS == 'tbswithin_1000km')

#diff
output <- ggplot(diffs_viz_QTR_BANDS, aes(x = as.Date(quarters),
                                       y = diffperc, colour = factor(yesNo))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS, ncol = 3) +
  scale_colour_manual(values=rndPal)

output



#diff between can/can't see?
meanDiff_canseez <- diffs_viz_QTR_BANDS %>% filter(!is.na(diff)) %>% 
  group_by(distanceBANDS,yesNo) %>% 
  summarise(mean = mean(diffperc))

head(meanDiff_canseez)

output <- ggplot(meanDiff_canseez, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
  geom_point(size = 2)

output

#absolute mean
# meanDiff_canseez <- diffs_viz_QTR_BANDS %>% filter(!is.na(diff)) %>% 
#   group_by(distanceBANDS,yesNo) %>% 
#   summarise(mean = mean(mean))
# 
# head(meanDiff_canseez)
# 
# output <- ggplot(meanDiff_canseez, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
#   geom_point(size = 2)
# 
# output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BY TIME PERIOD: Can-sees: quarters-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#See about making this long for summarising. Reduce columns:
avPrice_perQTR_perDist_CANSEEBANDS_YesNo <- sales_plus_canSeeBands %>% 
  dplyr::select(priceFinal,quarters,visibleInRadius.0to1km:visibleInRadius.14to15km) %>% 
  gather(distanceBANDS,yesNo,visibleInRadius.0to1km:visibleInRadius.14to15km)

head(avPrice_perQTR_perDist_CANSEEBANDS_YesNo)

avPrice_perQTR_perDist_CANSEEBANDS_YesNo$period <- 
  as.numeric(cut_number(as.numeric(avPrice_perQTR_perDist_CANSEEBANDS_YesNo$quarters),3))

avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary2 <- avPrice_perQTR_perDist_CANSEEBANDS_YesNo %>% 
  group_by(quarters,distanceBANDS,yesNo,period) %>% 
  summarise(mean = mean(priceFinal))

head(avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary2)


output <- ggplot() +
  geom_line(data = avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary2, aes(x = as.Date(quarters), 
                                                                        y = mean, colour = factor(yesNo),
                                                                        linetype = factor(period)),
            alpha = 0.75, size = 1) +
  #geom_line(data = meanz, aes(x = as.Date(paste0(year,'-01-01')), y = mean), size = 2, alpha = .5) + 
  #geom_point(alpha = .8) +
  scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS+period, scales = 'free') +
  scale_color_manual(values = rndPal)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BY TIME PERIOD: Can-sees: quarters, diff-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

diffs_viz_QTR_BANDS2 <- avPrice_perQTR_perDist_CANSEEBANDS_YesNoSummary2 %>% group_by(distanceBANDS, yesNo, period) %>% 
  mutate(diff = mean - lag(mean), diffperc = ((mean - lag(mean))/lag(mean))*100)

head(diffs_viz_QTR_BANDS2)

#chk3 <- diffs_viz_QTR_BANDS %>% filter(distanceBANDS == 'tbswithin_1000km')

#diff
output <- ggplot(diffs_viz_QTR_BANDS2, aes(x = as.Date(quarters),
                                          y = diffperc, colour = factor(yesNo),
                                          linetype = factor(period))) +
  geom_line(alpha = 0.75, size = 1) +
  geom_point(alpha = .8) +
  #scale_y_log10() +
  #facet_wrap(~period, scales = 'free', ncol = 1) +
  facet_wrap(~distanceBANDS+period, scales = 'free') +
  scale_colour_manual(values=rndPal)

output



#diff between can/can't see?
meanDiff_canseez2 <- diffs_viz_QTR_BANDS2 %>% filter(!is.na(diff)) %>% 
  group_by(distanceBANDS,yesNo,period) %>% 
  summarise(mean = mean(diffperc))

head(meanDiff_canseez2)

output <- ggplot(meanDiff_canseez2, aes(x = distanceBANDS, y = mean, colour = factor(yesNo))) +
  geom_point(size = 2) +
  facet_wrap(~period)

output




