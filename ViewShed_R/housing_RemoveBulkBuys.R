#Looking at repeat-sale housing data
#And attaching to the various other geogs, outputs etc.
#Start making some summary graphs
#For report and for me to understand its dynamics
library(dplyr)
library(tidyr)
library(pryr)
library(zoo)
library(ggplot2)
library(lubridate)
#library(sp)
geolibs <- c("pryr","stringr","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#Original repeat sales
hs <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal.rds")

#Geographies each property sits in
#First column is not original ID. Ignore, match on title.
geogs <- read.csv("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/UniqueAddressesOnly_repeatSales_areacodes2.csv")

names(hs)

#Don't need to be keeping all those
hs <- hs %>% dplyr::select(Title,date,priceFinal,eastingsFinal,northingsFinal)
#Adding addresses back in, briefly
hs <- hs %>% dplyr::select(Title,date,priceFinal,Subjects...FULL,eastingsFinal,northingsFinal)

#Just to reassure myself... Tick.
(unique(geogs$Title) %in% unique(hs$Title)) %>% length

#Merge in the geogs
hs <- left_join(hs,geogs,by='Title')
hs <- hs %>% dplyr::select(-X)

#save that, it's useful in that form: all the basics
#saveRDS(hs, "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes.rds")
saveRDS(hs, "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSales_priceDateGeocodesAreacodes_briefAddress.rds")

#Overall price averages
#Monthly
#Ha, didn't I just drop this?
hs$yearmon <- as.yearmon(hs$date)

hs_monthlyavprice <- hs %>% group_by(yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), monthlymedian = median(priceFinal), numsales = n())

ggplot(hs_monthlyavprice, aes(x = as.Date(yearmon), y = monthlymedian)) +
  geom_line()

ggplot(hs_monthlyavprice, aes(x = as.Date(yearmon), y = monthlyavprice)) +
  geom_line()

#Nov 2010 price spike. Data error?
nov10 <- hs[hs$yearmon == 'Nov 2010',]
nov10 <- nov10[order(-nov10$priceFinal),]

#I need the addresses back!
#Look at original
hs_orig <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal.rds")

hs_origSample <- hs_orig[hs_orig$Title %in% nov10$Title[1:10],]
hs_origSample <- hs_orig[hs_orig$priceFinal==13804947,]
hs_origSample <- hs_origSample[order(-hs_origSample$priceFinal),]

#OK, what I've learned:
#A single vastly high price repeats. 14 million. 32 obs.
#They're all in old, right at the end of the file.

#So need to look at original RoS old...
#old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles_finalRegistSet.rds")
#Let's try original first...
RoS_old <- readRDS("C:/Data/Housing/JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#smp <- RoS_old[RoS_old$regist %in% hs_origSample$Title,]
smp <- RoS_old[RoS_old$price == 13804947,]

#Ah ha: they were all one sale.
#Which begs the question: how many more like this in the old file? They'd be filtered out of new via Master already. 
#So need to check that.
#Load the updated-field one in case I need to remove any from the final list
RoS_old <- readRDS("C:/Data/Housing/JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles_finalRegistSet.rds")

#So we want ones where price and buyer are identical and have duplicates
pricebuyerdups <- subset(RoS_old, 
                    duplicated(RoS_old[,c('price','buyer')])
                    |duplicated(RoS_old[,c('price','buyer')],fromLast = T))


#Some close-together dates are probably wholesales too.
#Let's exclude those with dates over 4 months apart
pricebuyerdups <- pricebuyerdups %>% group_by(buyer,price) %>% 
  mutate(count =n(), dateDiff = abs(max(selldate_formatted)-min(selldate_formatted)))

pricebuyerdups <- data.frame(pricebuyerdups[order(-pricebuyerdups$count, 
                                                  pricebuyerdups$buyer,
                                                  pricebuyerdups$price),])

#save a sample
write.csv(pricebuyerdups[1:500,c(4:12,14,17:18)], "data/bulkSample.csv")

pbdcheck <- pricebuyerdups[pricebuyerdups$price == 1475000,]
#pricebuyerdups[1,]

writeClipboard( as.character(table(pricebuyerdups$registType) ))

countcounts <- pricebuyerdups %>% distinct(price, buyer) %>% 
  group_by(count) %>% 
  summarise(countnum = max(count), countOfCounts = n())

#How many of the 'duplicate price/buyer pair' titles are there in the *rest* of old?
pricebuyerORPHANS <- subset(RoS_old, 
                            !(duplicated(RoS_old[,c('price','buyer')])
                            |duplicated(RoS_old[,c('price','buyer')],fromLast = T)))


#Total in old orphans: 1,452,392
unique(pricebuyerORPHANS$newRegist) %>% length

#Total in old price/buyer pair duplicates: 121,875
unique(pricebuyerdups$newRegist) %>% length

#FALSE    TRUE 
#1369410  82982
(unique(pricebuyerORPHANS$newRegist) %in% unique(pricebuyerdups$newRegist)) %>% table

#So ~83K do appear elsewhere in old.
#And as individual sales.

#Let's compare mean quarterly prices for the two-sale dups only
#Just to see if - despite being years apart - they're consistently double the price they should be
#May have to break down by geog slightly

#Just old-without-dups and old-two-dups
pricebuyerdups$type <- "twodup"
pricebuyerORPHANS$type <- "nodup"

oldz <- rbind(pricebuyerdups[pricebuyerdups$count == 2,c(1:24,27)],pricebuyerORPHANS)

oldz$quarters <- cut(oldz$selldate_formatted, breaks="quarters") %>% as.numeric

prices <- oldz %>% group_by(type, quarters) %>% 
  summarise(meanprice = mean(price))

ggplot(prices, aes(x = as.Date(quarters),y = meanprice, colour=type)) +
  geom_line()

#Duplicate price/buyer pairs are actually looking like the right prices...
#So that's good. Let's filter out ones where the date is different and check again.
oldz <- rbind(pricebuyerdups[(pricebuyerdups$count == 2 & pricebuyerdups$dateDiff < 61),c(1:24,27)],pricebuyerORPHANS)

oldz$quarters <- cut(oldz$selldate_formatted, breaks="years") %>% as.numeric

prices <- oldz %>% group_by(type, quarters) %>% 
  summarise(meanprice = mean(price))

ggplot(prices, aes(x = as.Date(quarters),y = meanprice, colour=type)) +
  geom_line()

#A little more suspect, though there's not very much data for this. How many?
#12385 properties
pricebuyerdups[(pricebuyerdups$count == 2 & pricebuyerdups$dateDiff < 61),] %>% 
  distinct(newRegist) %>% nrow

#How many do we lose if we just filter out all the large multiple sales
#Even if dates are very different?
#Those prices are too similar...
#9098 sales, 7591 Titles
pricebuyerdups[(pricebuyerdups$count > 9),] %>% 
  distinct(newRegist) %>% nrow
pricebuyerdups[(pricebuyerdups$count > 9),] %>% nrow

#OK, somewhat arbitrary but here's the plan:
#1. Drop ANY duplicate buyer/price pairs with over 9 duplicates. Prices too suspicious
#For those with 9 or less duplicates:
#2. Drop all those where buyer/price match and date diff < 60

# pricebuyerdups_drops <- pricebuyerdups[pricebuyerdups$count > 9,]
# pricebuyerdups_drops <- rbind(pricebuyerdups_drops, pricebuyerdups[pricebuyerdups$count < 10
#                                            & pricebuyerdups$dateDiff < 60,])

#Gonna try bigger cutoff. What are the numbers?
pricebuyerdups_drops <- pricebuyerdups[pricebuyerdups$count > 4,]
pricebuyerdups_drops <- rbind(pricebuyerdups_drops, pricebuyerdups[pricebuyerdups$count < 5
                                                                   & pricebuyerdups$dateDiff < 60,])

#Does that still have those values in that got through?
#Yup.
pbdcheck2 <- pricebuyerdups_drops[pricebuyerdups_drops$price == 1475000,]


#30K obs, ~25k properties. (That may still have sales elsewhere...)
unique(pricebuyerdups_drops$newRegist) %>% length

#Load merge file
mrg <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal.rds")
#mrg <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge3.rds")

#Checking on non-matching december dates
chkz <- mrg[mrg$priceFinal==1475000,]

#Right, so: final merge old december dates are ONE YEAR BEFORE
#The december dates from the original file (although processed. Decembers not fixed.)
#Hence the lack of match. I should be able to drop the 5000000 thing below too now.
#So fixing carefully...
decs <- pricebuyerdups_drops[grepl("-12-",as.character(pricebuyerdups_drops$selldate_formatted)),]

#Yup, all need taking back a year
#pricebuyerdups_drops$selldate_formatted2 <- pricebuyerdups_drops$selldate_formatted

# pricebuyerdups_drops$selldate_formatted2[grepl("-12-",as.character(pricebuyerdups_drops$selldate_formatted))] <- 
#   (pricebuyerdups_drops$selldate_formatted[grepl("-12-",as.character(pricebuyerdups_drops$selldate_formatted))] - years(1))
pricebuyerdups_drops$selldate_formatted[grepl("-12-",as.character(pricebuyerdups_drops$selldate_formatted))] <- 
  (pricebuyerdups_drops$selldate_formatted[grepl("-12-",as.character(pricebuyerdups_drops$selldate_formatted))] - years(1))

#Yup, that worked.
unique(mrg$Title) %>% length

#mark the drops as drops
pricebuyerdups_drops$drops = 1

pricebuyerdups_drops <- data.frame(pricebuyerdups_drops)

#For one-pass matching
pricebuyerdups_drops <- pricebuyerdups_drops %>% rename(Title = newRegist)
pricebuyerdups_drops <- pricebuyerdups_drops %>% rename(date = selldate_formatted)
pricebuyerdups_drops <- pricebuyerdups_drops %>% rename(priceFinal = price)

#Noooow... we *should be able to match on a bunch of old-RoS fields
#Given I didn't think to leave in a fecking ob ID to match to.
mrg2 <- merge(mrg,pricebuyerdups_drops %>% dplyr::select(strno:street,pcode,priceFinal,drops),
              by  = c('strno','flatpos','street','pcode','priceFinal'), all.x = T)

#Probably doesn't matter that it grew a bit given we're going to drop those...

table(mrg2$drops, useNA = 'always')

#Let's check on 'em.
#Not so many matches but maybe that's to be expected - 
#this is just repeat sales we matched to.
chk <- mrg2[!is.na(mrg2$drops),]

table(chk$oldneworboth)

#Let's drop those 15K and see if we got what we wanted
chk <- mrg2[is.na(mrg2$drops),]

#Only 566 properties lost
unique(chk$Title) %>% length

#Crazy prices gone?
chk_monthlyavprice <- chk %>% group_by(yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), monthlymedian = median(priceFinal), numsales = n())

# ggplot(chk_monthlyavprice, aes(x = as.Date(yearmon), y = monthlymedian)) +
#   geom_line()

ggplot(chk_monthlyavprice, aes(x = as.Date(yearmon), y = monthlyavprice)) +
  geom_line()



# #Better! One more suspicious spike, late 2004. Wossat?
# dec04 <- chk[chk$yearmon=="Dec 2004",]
# dec04 <- dec04[order(-dec04$priceFinal),]
# 
# #Oh ah-ha. It's the year/date diff thing. 
# #Seem to have plenty of matches on december! And I'm pretty sure I did fix this.
# looks <- chk %>% filter(oldneworboth == "both")
# 
# #Note in the old, the december dates are still wrong...
# dec04orig <- pricebuyerdups[pricebuyerdups$price == 5000000 
#                              & pricebuyerdups$selldate_formatted==as.Date("2005-12-01", format="%Y-%m-%d"),]
# 
# #OK, that's 41 obs, again a large bulk company buy
# #How many fitting that date/price match? 27. Think we got em then.
# chk[(chk$date == as.Date("2004-12-01", format="%Y-%m-%d") & chk$priceFinal == 5000000),] %>% nrow
# 
# #remove, check on price graph again
# chk2 <- chk[!(chk$date == as.Date("2004-12-01", format="%Y-%m-%d") & chk$priceFinal == 5000000),]
# 
# chk_monthlyavprice <- chk2 %>% group_by(yearmon) %>% 
#   summarise(monthlyavprice = mean(priceFinal), monthlymedian = median(priceFinal), numsales = n())
# 
# # ggplot(chk_monthlyavprice, aes(x = as.Date(yearmon), y = monthlymedian)) +
# #   geom_line()
# 
# ggplot(chk_monthlyavprice, aes(x = as.Date(yearmon), y = monthlyavprice)) +
#   geom_line()
# 
# #OK, that'll do then. Drop drops!
# chk2 <- chk2 %>% dplyr::select(-drops)
# 
# #Oh, I also need to check on any orphans... and expunge them. Nice.
# #i.e. keep only repeat sales still.
# chk2 <- chk2 %>% group_by(Title) %>% 
#   mutate(rpts = n())
# 
# #6850 new orphans. Boo.
# table(0 + (chk2$rpts == 1))
# 
# chk2 <- chk2[chk2$rpts!=1,]
# 
# chk2 <- chk2[order(chk2$Title),]

#There's also a stupid stupid price in there. I thought I'd filtered out the stupid prices!
chk <- chk[chk$priceFinal != 81964000, ]

#~~~~~~~~~~~~~~~~~~~~~~~
# Checking on other awry prices

#Is it possible to see which are too wrong compared to their other repeat sales?
#Wrong ones will generally be wrong by an order of magnitude

chk <- chk %>% group_by(Title) %>% 
  mutate(titleCount = n())

table(chk$titleCount)

#Have not removed the orphans!
chk <- chk[chk$titleCount!=1,]

#How much bigger is a repeat sales' max value from its min?
chk <- chk %>% group_by(Title) %>% 
  mutate(priceDiff = max(priceFinal)/min(priceFinal))
  
ooneqs <- chk %>% distinct(Title)

#price difference, min/max repeat sale price for all
boxplot(ooneqs$priceDiff)

ooneqs <- ooneqs[order(-ooneqs$priceDiff),]
plot(ooneqs$priceDiff)

#So yes: some bonkers differences. Let's actually look at some of those sales before deciding what to filter
chk <- chk[order(-chk$priceDiff),]

lookdiff <- chk %>% dplyr::select(Title,priceFinal,pcode,date,titleCount,priceDiff)
lookdiff <- lookdiff[order(-lookdiff$priceDiff,lookdiff$Title,lookdiff$date),]

#3.4. So anything getting into the hundreds is obviously stupid...
mean(lookdiff$priceDiff)

#11 times diff is plausible. A date normalised version of the price diff would help.
#How many lost for different filters?
#How many properties?
#3109
ooneqs %>% filter(priceDiff > 20) %>% nrow
#5577
ooneqs %>% filter(priceDiff > 15) %>% nrow
#9785
ooneqs %>% filter(priceDiff > 12) %>% nrow

#####
#Have a go at normalising prices a little over the timeframe
#Do this by council area
geogs <- read.csv("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/UniqueAddressesOnly_repeatSales_areacodes2.csv")

chk_g <- left_join(chk,geogs,by='Title')

#Two obs w no council area geog
table(0 + (is.na(chk_g$councilArea)))
chk_g <- chk_g[!is.na(chk_g$councilArea),]

#Mark years
chk_g$year <- cut(chk_g$date,breaks="year")

#average price per council area per year
chk_g <- chk_g %>% group_by(councilArea,year) %>% 
  mutate(meanPrice = mean(priceFinal))

#check that looks sensible
meanPrices <- chk_g %>% distinct(councilArea,year)

#Whoa. Yes.
ggplot(meanPrices,aes(x = as.Date(year), y = meanPrice, colour = councilArea)) +
  geom_line() +
  guides(colour=F)
  
#So using the first year as a normalising value
#Adjust other prices to make a better min/max comparison
#So e.g. for council area x in the following year
#So we need that index first...
#For each council area
#Min won't be perfect but will do.
chk_g <- chk_g %>% group_by(councilArea) %>% 
    mutate(councilAreaMeanPrice_1stYear = min(meanPrice))

#Divide other years to get an index to multiply prices by
#e.g. 2001 = 1, 2010 = 2, index = 1/2 so halve 2010 prices in that council area for that year
chk_g <- chk_g %>% group_by(councilArea,year) %>% 
  mutate(CAyear_priceIndex = councilAreaMeanPrice_1stYear/meanPrice)

#That looks like it worked...
#check that looks sensible
meanPrices_normd <- chk_g %>% distinct(councilArea,year)

ggplot(meanPrices_normd,aes(x = as.Date(year), y = CAyear_priceIndex, colour = councilArea)) +
  geom_line() +
  guides(colour=F)

#Yup, looks good.

#Now just multiply all prices through by index to get them adjusted 
chk_g$priceAdjusted <- chk_g$priceFinal * chk_g$CAyear_priceIndex

#Those should now look fairly flat over the period...
#average ADJUSTED price per council area per year
chk_g <- chk_g %>% group_by(councilArea,year) %>% 
  mutate(adjustedMeanPrice = mean(priceAdjusted))

#check that looks sensible
adjMeanPrices <- chk_g %>% distinct(councilArea,year)

#Err, yup. That worked! 
ggplot(adjMeanPrices,aes(x = as.Date(year), y = adjustedMeanPrice, colour = councilArea)) +
  geom_line() +
  guides(colour=F)

#Let's just check with monthly mean
chk_g <- chk_g %>% group_by(councilArea,yearmon) %>% 
  mutate(adjustedMeanPriceMonthly = mean(priceAdjusted))

adjMeanPricesMon <- chk_g %>% distinct(councilArea,yearmon)

ggplot(adjMeanPricesMon,aes(x = as.Date(yearmon), y = adjustedMeanPriceMonthly, colour = councilArea)) +
  geom_line() +
  guides(colour=F)

#And non-adjusted for comparison
chk_g <- chk_g %>% group_by(councilArea,yearmon) %>% 
  mutate(meanPriceMonthly = mean(priceFinal))

meanPricesMon <- chk_g %>% distinct(councilArea,yearmon)

ggplot(meanPricesMon,aes(x = as.Date(yearmon), y = meanPriceMonthly, colour = councilArea)) +
  geom_line() +
  guides(colour=F)


#Yes, more or less, with our spikes back.

#################
#Right! That will now allow us to produce a much more comparable min/max price difference.

#Let's just save that...
saveRDS(chk_g, "C:/Data/temp/chk_g.rds")

chk_g <- chk_g %>% group_by(Title) %>% 
  mutate(adjPriceDiff = max(priceAdjusted)/min(priceAdjusted))

ooneqs <- chk_g %>% distinct(Title)

#price difference, min/max repeat sale price for all
#boxplot(ooneqs$priceDiff)

ooneqs <- ooneqs[order(-ooneqs$adjPriceDiff),]
#plot(ooneqs$priceDiff)

lookdiff <- chk_g %>% dplyr::select(Title,priceFinal,priceAdjusted,pcode,date,titleCount,priceDiff,adjPriceDiff)
lookdiff <- lookdiff[order(-lookdiff$adjPriceDiff,lookdiff$Title,lookdiff$date),]

#Mean diff down to 2.16 from 3.4. Good good. Let's look.
mean(lookdiff$adjPriceDiff)

#Just checking there's no problem with that...
lookdiff2 <- chk_g %>% dplyr::select(Title,priceFinal,priceAdjusted,pcode,date,titleCount,priceDiff,adjPriceDiff)
lookdiff2 <- lookdiff2[order(-lookdiff2$priceDiff,lookdiff2$Title,lookdiff2$date),]

looksee <- lookdiff %>% dplyr::select(priceDiff,adjPriceDiff)

looksee <- looksee %>% distinct(Title)

#Give index for graphing shortly
looksee$index = 1:nrow(looksee)

looksee <- looksee %>% gather(priceDiffs,allPriceDiffs,priceDiff,adjPriceDiff)

ggplot(looksee,aes(x = index, y = allPriceDiffs, colour = priceDiffs)) +
  geom_line() +
#  xlim(0,2000) +
  scale_y_log10()

# #And for initial price diffs for slightly better comparison...
# looksee2 <- lookdiff %>% dplyr::select(adjPriceDiff,priceDiff)
# looksee2 <- looksee2[order(-looksee2$priceDiff),]
# 
# #Give index for graphing shortly
# looksee2$index = 1:nrow(looksee2)
# 
# looksee2 <- looksee2 %>% gather(priceDiffs,allPriceDiffs,adjPriceDiff,priceDiff)
# 
# ggplot(looksee2,aes(x = index, y = allPriceDiffs, colour = priceDiffs)) +
#   geom_line() +
#   scale_y_log10()
  
#So how many lost for each cutoff from the adjusted version?
#How many properties?
#2652
ooneqs %>% filter(adjPriceDiff > 20) %>% nrow
#3984
ooneqs %>% filter(adjPriceDiff > 15) %>% nrow
#5546
ooneqs %>% filter(adjPriceDiff > 12) %>% nrow
#8589. And I'm tempted to do this one...
ooneqs %>% filter(adjPriceDiff > 9) %>% nrow

#Save lookdiff to illustrate for others...
saveRDS(lookdiff,"C:/Data/temp/RoS_priceAdjusted_minMaxDiffs.rds")
write.csv(lookdiff,"C:/Data/temp/RoS_priceAdjusted_minMaxDiffs.csv")

lookdiff <- readRDS("C:/Data/temp/RoS_priceAdjusted_minMaxDiffs.rds")

#My conclusion: I'm going to drop adjusted diffs of more than *9 min/max diff
conc <- chk_g[chk_g$adjPriceDiff < 10,]

#637034
unique(conc$Title) %>% length

conc <- conc %>% dplyr::select(-(drops:adjPriceDiff))

#And a last tag-on removal in Orkney that doesn't fit with the local pattern / the postcode
conc[conc$priceFinal==431150,]
conc <- conc[conc$priceFinal!=431150,]
  
saveRDS(conc,"C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

conc <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Filter all-sales (build on filtered repeats)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get the previously filtered repeats. 
#Work out what properties are left over from the original list of all properties.
#Check on filtering out any non-geocodes first.
#Then check the single-sales for any stoopid prices.
rpts <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/old_new_repeatSalesFinal_bulkSales_minmaxDiffMoreThan9_Removed.rds")

allz <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge.rds")

#Sanity checks
#rpts titles should be a subset of allz titles 
#Tick.
#unique(rpts$Title) %in% unique(allz$Title) %>% table
#FALSE   TRUE 
#964676 637034
unique(allz$Title)%in% unique(rpts$Title)%>% table

#So nearly a million single sales? OK then. It's those we need to be looking at.
#1112273 obs.
singlez <- allz[!(allz$Title) %in% unique(rpts$Title),]

#964676, as above
unique(singlez$Title) %>% length

#So some more that were probably repeats that for various reasons we dropped (including awry prices)
#So keep only those that are definitely single sales
#878316 obs, 233957 less than full "not in rpts" list
singlez <- subset(singlez, !duplicated(singlez$Title) & !duplicated(singlez$Title, fromLast = T))

#Geocoding?
#678719 with geocodes, 199597 without
table(0 + !is.na(singlez$eastingsFinal)) 

#Can only use geocoded
singlez <- singlez[!is.na(singlez$eastingsFinal),]

#What split across old/new we got? A mix, nowt surprising.
singlez$oldneworboth %>% table

#OK, so now to check on price sanity in the singlez file.
#Again, probably best to look at council area level so any spikes show up clearly
#zone that up.
ca <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2010/ScottishCouncilAreas2010_Derivedbyaggregating2011OAs_usingNRSexactfitCensusIndex", 
              layer="scotland_ca_2010")

singlez_geo <- singlez

coordinates(singlez_geo) = ~eastingsFinal+northingsFinal
proj4string(singlez_geo) <- proj4string(ca)

overz <- (singlez_geo %over% ca)

#Long time. Save.
saveRDS(overz,"data/temp/overz.rds")

singlez_geo@data$councilArea <- overz$code

#and back to non-geo...
singlez <- data.frame(singlez_geo)

#save
saveRDS(singlez,"data/temp/singlez_w_CAs.rds")

#And now to sanity-check that mofo. Monthly mean per CA
singlez$yearmon <- as.yearmon(singlez$date)

hs_monthlyavprice <- singlez %>% group_by(councilArea,yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), monthlymedian = median(priceFinal), numsales = n())

# ggplot(hs_monthlyavprice, aes(x = as.Date(yearmon), y = monthlymedian)) +
#   geom_line()

ggplot(hs_monthlyavprice, aes(x = as.Date(yearmon), y = monthlyavprice, colour = councilArea)) +
  geom_line()

#1 is minimum count... 
min(hs_monthlyavprice$numsales)

#Try quarterly to get bigger count. Does it pick up awry values?
singlez$quarters <- as.yearqtr(singlez$date)

singlez %>% group_by(councilArea,quarters) %>% 
  summarise(monthlyavprice = mean(priceFinal), monthlymedian = median(priceFinal), numsales = n()) %>% 
  ggplot(aes(x = as.Date(quarters), y = monthlyavprice, colour = councilArea)) +
  geom_line()

#Some of them. So could do a pass on quarters first then check monthly.
#There appears to be one CA in particular that's weird... 
#... and only starts with the new data?
#Are those NAs?
#ggplot wouldn't show NAs, would it?
#11 NAs.
table(0 + is.na(singlez$councilArea))

#Look more closely
#Look at groups of council areas ordered by their average price
singlez <- singlez %>% group_by(councilArea) %>% 
  mutate(CA_mean = mean(priceFinal))

#singlez$groups <- as.numeric(cut_number(as.numeric(singlez$councilArea), 4))
singlez$groups <- as.numeric(cut_interval(singlez$CA_mean, 9))

singlez %>% group_by(councilArea,quarters) %>% 
  summarise(monthlyavprice = mean(priceFinal), 
            group = max(groups), numsales = n()) %>% 
  ggplot(aes(x = as.Date(quarters), y = monthlyavprice, colour = councilArea)) +
  geom_line() +
  facet_wrap(~group)

#Ah yes, it is NAs. Easy nuff to remove then. 
#Facetting quite useful for looking for problems generally.
#Dump the 11 NAs
singlez <- singlez[!is.na(singlez$councilArea),]

singlez %>% group_by(councilArea,yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), 
            group = max(groups), numsales = n()) %>% 
  ggplot(aes(x = as.Date(yearmon), y = monthlyavprice, colour = councilArea)) +
  geom_line() +
  facet_wrap(~group, scales='free_y') +
  guides(colour = F)

#Or maybe just look at all council areas
singlez %>% group_by(councilArea,yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), 
            group = max(groups), numsales = n()) %>% 
  ggplot(aes(x = as.Date(yearmon), y = monthlyavprice, colour = councilArea)) +
  geom_line() +
  facet_wrap(~councilArea, scales='free_y', ncol = 3) +
  guides(colour = F)

#Awry values for quarters may be way beyond sensible SDs
#But might not warp monthly means so much
#Let's see if an SD filter on quarterlies per council area does the job

singlez_q <- singlez %>% group_by(councilArea,quarters) %>% 
  mutate(q_avprice = mean(priceFinal), q_sd = sd(priceFinal), numsales = n())

#How many obs beyond 4-sigma? (And should we be using log values? This should still get completely wrong prices)
singlez_q$beyond4sigma <- 0 + (singlez_q$priceFinal > (singlez_q$q_avprice + (singlez_q$q_sd * 4)) |
                                 singlez_q$priceFinal < (singlez_q$q_avprice - (singlez_q$q_sd * 4)) )

#0      1 
#674399 4304
singlez_q$beyond4sigma %>% table

#OK, if we filter those, what do things look like?
singlez_q %>% 
  filter(beyond4sigma != 1) %>% 
  group_by(councilArea,yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), 
            group = max(groups), numsales = n()) %>% 
  ggplot(aes(x = as.Date(yearmon), y = monthlyavprice, colour = councilArea)) +
  geom_line() +
  facet_wrap(~councilArea, scales='free_y', ncol = 3) +
  guides(colour = F)

#All but one value, which I suspect is a single value with no SD.
#Council area S12000046
lookz <- singlez_q %>% filter(beyond4sigma != 1 & councilArea == 'S12000046') %>% 
  group_by(councilArea,yearmon) %>% 
  summarise(monthlyavprice = mean(priceFinal), 
            group = max(groups), numsales = n()) 

#And that's Jun 1994. Let's look at the original prices
jun94 <- singlez_q %>% filter(councilArea == 'S12000046' & yearmon == 'Jun 1994') %>% 
  arrange(-priceFinal)

#It's a couple of old-RoS bulk-sales-with-one-price again.
#Let's just remove them. The 4-sigma filter will have got most of the rest.
#Check the filter price is only for those I can see.
#Yup.
singlez_q %>% filter(priceFinal %in% c(1272247,1230535)) %>% nrow
jun94 %>% filter(priceFinal %in% c(1272247,1230535)) %>% nrow

singlez_q <- singlez_q %>% filter(!priceFinal %in% c(1272247,1230535))

#OK, we have a winner. Save.
saveRDS(singlez_q, "data/temp/finalSingleSales.rds")
singlez_q <- readRDS("data/temp/finalSingleSales.rds")

#Now we just need to combine this with the filtered repeat sales we already have
#Might as well add a flag just for convenience
singlez_q$isRepeatSale <- 0
rpts$isRepeatSale <- 1

names(singlez_q)[names(singlez_q) %in% names(rpts)]
names(singlez_q)[!names(singlez_q) %in% names(rpts)]

#Might as well not keep council area - need to re-run all the geog assignments anyway.

#Hmm. Or alternatively, do it here (since these are all single Title numbers)
#And just add to the previous geog file.

#Let's tie these two together first
repeats_plus_singleSales <- rbind(rpts,singlez_q[,names(singlez_q)[names(singlez_q) %in% names(rpts)]])

#some columns in rpts we don't need either
# repeats_plus_singleSales <- rbind(rpts %>% dplyr::select_(names(rpts)[names(rpts) %in% names(singlez_q)]), 
#   singlez_q %>% dplyr::select_(names(singlez_q)[names(singlez_q) %in% names(rpts)]))
repeats_plus_singleSales <- rbind(rpts[,names(rpts) %in% names(singlez_q)], 
  singlez_q[,names(singlez_q) %in% names(rpts)])

#And that's all our sales
saveRDS(
  repeats_plus_singleSales, 
  "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/SingleSalesPlusRepeatSales_filtered_July16.rds")

#save as CSV to windfarms folder. Keep only relevant columns
write.csv(
  repeats_plus_singleSales %>% 
    data.frame %>% 
    dplyr::select(priceFinal,Title,date,oldneworboth,eastingsFinal,northingsFinal,isRepeatSale), 
  "C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/SingleSalesPlusRepeatSales_filtered_July16.csv",
  row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Actually, add in postcode via PiP for ease.
houses <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/SingleSalesPlusRepeatSales_filtered_July16.csv")

areacodes <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/UniqueAddressesOnly_allSales_areacodes.csv")

pcs <- read.csv("C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/postcode_centroids.csv")

#sanity checks... good good.
unique(houses$Title) %>% length
unique(areacodes$Title) %>% length
#good good!
unique(areacodes$postcode) %>% length
unique(pcs$postcode) %>% length

#Any missing postcodes? Newp.
table(0 + is.na(areacodes$postcode), useNA = 'always')

houses <- merge(houses,areacodes %>% dplyr::select(Title,postcode), by = 'Title')

#make clear it's a geog-deduced postcode
houses <- houses %>% rename(postcode_via_pip = postcode)

#merge in postcode ID as well
houses <- merge(houses,pcs %>% dplyr::select(id,postcode), by.x = 'postcode_via_pip', by.y = 'postcode')

houses <- houses %>% dplyr::select(Title:isRepeatSale,postcode_via_pip,postcode_id = id)

#save again
write.csv(houses,"C:/Users/SMI2/Dropbox/WindFarmsII/data/allSalesData/SingleSalesPlusRepeatSales_filtered_July16.csv", row.names = F)

windfarms <- read.csv("C:/Users/admin/Dropbox/WindFarmsII/data/allSalesData/wholeWindfarms/whole_windfarm_centroids.csv")

hist(log(windfarms$turbineCount))









