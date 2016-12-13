#graph model regressions
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1A: Postcode/windfarm centroids, Viz only in radii----
radii_justVizCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/Table1A.csv")

radii_justViz <- data.frame(windfarmRadii = radii_justVizCSV[c(5,7,9,11,13,15),'X'],
                            coeff = c(radii_justVizCSV[5,'X.1.'] %>% as.character(),
                                      radii_justVizCSV[7,'X.15.']%>% as.character(),
                                      radii_justVizCSV[9,'X.29.']%>% as.character(),
                                      radii_justVizCSV[11,'X.43.']%>% as.character(),
                                      radii_justVizCSV[13,'X.57.']%>% as.character(),
                                      radii_justVizCSV[15,'X.71.']%>% as.character()
                                      ))

radii_justViz$coeffNum <- gsub("\\*","",radii_justViz$coeff) %>% as.numeric

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1B: Postcode/windfarm centroids, Viz/non-viz in radii----
radii_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/Table1B.csv")


#Use previous radii label, then add flag for viz/non-viz
#First lot are non-viz, then viz
radii_VizAndNon <- data.frame(windfarmRadii = c(radii_justVizCSV[c(5,7,9,11,13,15),'X'] %>% as.character(),
                                                radii_justVizCSV[c(5,7,9,11,13,15),'X']%>% as.character()),
                              visible = c(rep(0,6),rep(1,6)),
                              coeff = c(
                                #non-viz first. 7,11,15,19,23,27 
                                radii_VizAndNonCSV[7,'X.1.'] %>% as.character(),
                                radii_VizAndNonCSV[11,'X.17.'] %>% as.character(),
                                radii_VizAndNonCSV[15,'X.33.'] %>% as.character(),
                                radii_VizAndNonCSV[19,'X.49.'] %>% as.character(),
                                radii_VizAndNonCSV[23,'X.65.'] %>% as.character(),
                                radii_VizAndNonCSV[27,'X.81.'] %>% as.character(),
                                #then viz. 5,9,13,17,21,25
                                radii_VizAndNonCSV[5,'X.1.'] %>% as.character(),
                                radii_VizAndNonCSV[9,'X.17.'] %>% as.character(),
                                radii_VizAndNonCSV[13,'X.33.'] %>% as.character(),
                                radii_VizAndNonCSV[17,'X.49.'] %>% as.character(),
                                radii_VizAndNonCSV[21,'X.65.'] %>% as.character(),
                                radii_VizAndNonCSV[25,'X.81.'] %>% as.character()
                              )
                              )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1C: Postcode/windfarm centroids, Viz/non-viz in bands----

#Already in a slightly more manageable format
# bands_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/Table1C.csv", as.is = T)
bands_VizAndNonCSV <- read.csv("data/regressionTables/Table1C_centroids.csv", as.is = T)
#With windfarm extensions

#Dropping some columns
names(bands_VizAndNonCSV)
bands_VizAndNonCSV <- bands_VizAndNonCSV %>% dplyr::select(-c(X.5.:X.6., X.9.,X.11.,X.13.:X.15.))

#Just values, no standard errors
bands_VizAndNon <- bands_VizAndNonCSV[seq(from = 5, to = 27, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNonCSV[3,c(2:ncol(bands_VizAndNonCSV))], " ",
                bands_VizAndNonCSV[32,c(2:ncol(bands_VizAndNonCSV))])

namez <- c("band",namez)

names(bands_VizAndNon) <- namez

#For now, drop sig
bands_VizAndNon <- apply(bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Mark viz and non
bands_VizAndNon$visible <- 0 + grepl("_vis",bands_VizAndNon$band)

#Now we've got that, drop differences in band names
#bands_VizAndNon$bands[grepl("_vis",bands_VizAndNon$bands)] <- gsub("_vis", "", bands_VizAndNon$bands)
bands_VizAndNon$band <- gsub("_vis", "", bands_VizAndNon$band)
bands_VizAndNon$band <- gsub("_nvis", "", bands_VizAndNon$band)

#Ready to gather that mofo
bands_VizAndNon_final <- gather(bands_VizAndNon,model,coeff,2:(ncol(bands_VizAndNon)-1))

bands_VizAndNon_final$coeff <- as.numeric(bands_VizAndNon_final$coeff) * 100

#bands should be factors in the right order
bands_VizAndNon_final$band <- factor(bands_VizAndNon_final$band[1:6], levels = bands_VizAndNon_final$band[1:6],
                                     ordered = T)

#~~~~~~~~~~~
#Repeat for CIs
bands_VizAndNonSE <- bands_VizAndNonCSV[seq(from = 6, to = 28, by = 2),]

bands_VizAndNonSE$X <- bands_VizAndNonCSV$X[seq(from = 5, to = 27, by = 2)]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNonCSV[3,c(2:ncol(bands_VizAndNonCSV))], " ",
                bands_VizAndNonCSV[32,c(2:ncol(bands_VizAndNonCSV))])

namez <- c("band",namez)

names(bands_VizAndNonSE) <- namez

#Mark viz and non
bands_VizAndNonSE$visible <- 0 + grepl("_vis",bands_VizAndNonSE$band)

#Now we've got that, drop differences in band names
#bands_VizAndNonSE$bands[grepl("_vis",bands_VizAndNonSE$bands)] <- gsub("_vis", "", bands_VizAndNonSE$bands)
bands_VizAndNonSE$band <- gsub("_vis", "", bands_VizAndNonSE$band)
bands_VizAndNonSE$band <- gsub("_nvis", "", bands_VizAndNonSE$band)

#remove brackets
bands_VizAndNonSE <- apply(bands_VizAndNonSE, 2, function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Ready to gather that mofo
bands_VizAndNonSE_final <- gather(bands_VizAndNonSE,model,SE,2:(ncol(bands_VizAndNon)-1))

bands_VizAndNonSE_final$SE <- as.numeric(bands_VizAndNonSE_final$SE) * 100

#bands should be factors in the right order
bands_VizAndNonSE_final$band <- factor(bands_VizAndNonSE_final$band[1:6], levels = bands_VizAndNonSE_final$band[1:6],
                                       ordered = T)

#~~~~~~~~~~~
#Combine, graph
bands_VizAndNon_final$SE <- bands_VizAndNonSE_final$SE
bands_VizAndNon_final$min <- bands_VizAndNon_final$coeff - (bands_VizAndNon_final$SE * 1.96)
bands_VizAndNon_final$max <- bands_VizAndNon_final$coeff + (bands_VizAndNon_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

bands_VizAndNon_final$visible <- factor(bands_VizAndNon_final$visible, labels = c('no','yes'))

#Alphabetical indices for names

labelz <- c(
  'FE: properties...',
  'FE: plus geography',
  'FE: plus rings',
  'FE: plus NUTS2',
  #'FE: plus population centres',
  #'NUTS3 means',
  'Single turbines',
  'More than one turbine',
  #'Coast < 1km',
  'Coast < 2km',
  #'Coast > 1km',
  'Coast > 2km'
  #'Urban',
  #'Rural',
  #'2000-2015'
)

letterz <- sapply(LETTERS[seq(1:length(labelz))], function(x) paste0('(',x,')'))

labelz <- sapply(seq(1:length(labelz)), function(x) paste0(letterz[x],' ',labelz[x]))

bands_VizAndNon_final$model <- factor(bands_VizAndNon_final$model,
                                       labels = labelz)

bands_VizAndNon_final$band <- factor(bands_VizAndNon_final$band, 
                                      labels = c(
                                        '0-2km',
                                        '2-3km',
                                        '3-4km',
                                        '4-5km',
                                        '5-8km',
                                        '8-14km'
                                      ))


minz = -15
maxz = 15

output <- ggplot(bands_VizAndNon_final, aes(x = band, y = coeff, colour = visible)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 'no'), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 'yes'), aes(group = model), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model, ncol = 2) +
  # facet_wrap(~model, scales = 'free_y', ncol = 2) +
  xlab("distance band") +
  ylab("percent") +
  # geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
  scale_y_continuous(limits = c(minz,maxz)) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave#("saves/C1_centroids.png", dpi=150, width = 10, height = 10)
ggsave("saves/C1_centroids.png", dpi=150, width = 8, height = 8)

#Repeat for first graph only
#Can't get it to select based on the label. Hmmph.
subz <- bands_VizAndNonSE_final %>% dplyr::filter(model %>% as.numeric == 1)
#Something *odd* going on here...
subz <- bands_VizAndNonSE_final[as.numeric(bands_VizAndNonSE_final$model) == 1,]

subz <- data.frame(bands_VizAndNon_final)
#Sod this, selecting by row
subz <- subz[1:12,]

output <- ggplot(subz, aes(x = band, y = coeff, colour = visible)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 'no'), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 'yes'), aes(group = model), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  # facet_wrap(~model, ncol = 2) +
  # facet_wrap(~model, scales = 'free_y', ncol = 2) +
  xlab("distance band") +
  ylab("percent") +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  # geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
  # scale_y_continuous(limits = c(minz,maxz)) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/C1_centroids_figure1.png", dpi=150, width = 7, height = 4)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1D: Postcode/windfarm centroids, Viz/non-viz in bands, then three groups for diff windfarm sizes----

bands_3groupsCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/Table1D.csv", as.is = T)

#Just values, no standard errors
bands_3groups <- bands_3groupsCSV[seq(from = 5, to = 75, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_3groupsCSV[3,c(2:ncol(bands_3groupsCSV))], " ",
                bands_3groupsCSV[124,c(2:ncol(bands_3groupsCSV))])

namez <- c("band",namez)

names(bands_3groups) <- namez

#For now, drop sig
bands_3groups <- apply(bands_3groups, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Add NAs
bands_3groups <- apply(bands_3groups, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
bands_3groups$visible <- 0 + grepl("_vis",bands_3groups$band)

#Mark which turbine size group
bands_3groups$sizeGroup <- '1-10'
bands_3groups$sizeGroup[grepl("_b2",bands_3groups$band)] <- '11-20'
bands_3groups$sizeGroup[grepl("_b3",bands_3groups$band)] <- '+20'

bands_3groups$sizeGroup <- factor(bands_3groups$sizeGroup[1:3], levels = bands_3groups$sizeGroup[1:3])

#Now we've got that, drop differences in band names
#bands_3groups$bands[grepl("_vis",bands_3groups$bands)] <- gsub("_vis", "", bands_3groups$bands)
bands_3groups$band <- gsub("_vis", "", bands_3groups$band)
bands_3groups$band <- gsub("_nvis", "", bands_3groups$band)
bands_3groups$band <- gsub("_b1", "", bands_3groups$band)
bands_3groups$band <- gsub("_b2", "", bands_3groups$band)
bands_3groups$band <- gsub("_b3", "", bands_3groups$band)

#Ready to gather that mofo
bands_3groups_final <- gather(bands_3groups,model,coeff,operation.date.H:Coast.1.H.G.R.N.D.1)

bands_3groups_final$coeff <- as.numeric(bands_3groups_final$coeff) * 100

#bands should be factors in the right order
bands_3groups_final$band <- factor(bands_3groups_final$band[1:18], levels = unique(bands_3groups_final$band),
                                    ordered = T)


#~~~~~~~~~~~~~~~
#CIs
bands_3groupsSE <- bands_3groupsCSV[seq(from = 6, to = 76, by = 2),]

bands_3groupsSE$X <- bands_3groupsCSV$X[seq(from = 5, to = 75, by = 2)]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_3groupsCSV[3,c(2:ncol(bands_3groupsCSV))], " ",
                bands_3groupsCSV[124,c(2:ncol(bands_3groupsCSV))])

namez <- c("band",namez)

names(bands_3groupsSE) <- namez

#For now, drop sig
bands_3groupsSE <- apply(bands_3groupsSE, c(1,2), function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Add NAs
bands_3groupsSE <- apply(bands_3groupsSE, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
bands_3groupsSE$visible <- 0 + grepl("_vis",bands_3groupsSE$band)

#Mark which turbine size group
bands_3groupsSE$sizeGroup <- '1-10'
bands_3groupsSE$sizeGroup[grepl("_b2",bands_3groupsSE$band)] <- '11-20'
bands_3groupsSE$sizeGroup[grepl("_b3",bands_3groupsSE$band)] <- '+20'

bands_3groupsSE$sizeGroup <- factor(bands_3groupsSE$sizeGroup[1:3], levels = bands_3groupsSE$sizeGroup[1:3])

#Now we've got that, drop differences in band names
#bands_3groupsSE$bands[grepl("_vis",bands_3groupsSE$bands)] <- gsub("_vis", "", bands_3groupsSE$bands)
bands_3groupsSE$band <- gsub("_vis", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_nvis", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_b1", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_b2", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_b3", "", bands_3groupsSE$band)

#Ready to gather that mofo
bands_3groupsSE_final <- gather(bands_3groupsSE,model,SE,operation.date.H:Coast.1.H.G.R.N.D.1)

bands_3groupsSE_final$SE <- as.numeric(bands_3groupsSE_final$SE) * 100

#bands should be factors in the right order
bands_3groupsSE_final$band <- factor(bands_3groupsSE_final$band[1:18], levels = unique(bands_3groupsSE_final$band),
                                   ordered = T)

#~~~~
#Combine
bands_3groups_final$SE <- bands_3groupsSE_final$SE
bands_3groups_final$min <- bands_3groups_final$coeff - (bands_3groups_final$SE * 1.96)
bands_3groups_final$max <- bands_3groups_final$coeff + (bands_3groups_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

#subset to group
# subz <- bands_3groups_final %>% filter(sizeGroup == '1-10')
# subz <- bands_3groups_final %>% filter(sizeGroup == '11-20')
# subz <- bands_3groups_final %>% filter(sizeGroup == '+20')
subz <- bands_3groups_final

output <- ggplot(subz, aes(x = band, y = coeff, colour = factor(visible))) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 0), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 1), aes(group = model, alpha = 0.6)) +
  # facet_wrap(~model, ncol = 3)
  # facet_wrap(~model, scales = 'free_y', ncol = 3) +
  facet_wrap(~model+sizeGroup, scales = 'free_y') +
  # facet_wrap(~model+sizeGroup) +
  #scale_y_continuous(limits = c(-10, 10)) +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1F: Postcode/windfarm centroids, distance bands only, BY NUTS3----

nuts3_distbandsCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/Table1F.csv", as.is = T)

#Just values, no standard errors
nuts3_distbands <- nuts3_distbandsCSV[seq(from = 5, to = 15, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(nuts3_distbandsCSV[3,c(2:ncol(nuts3_distbandsCSV))], " ",
                nuts3_distbandsCSV[20,c(2:ncol(nuts3_distbandsCSV))])

#namez <- c("band",namez)
namez <- c("band",nuts3_distbandsCSV[3,c(2:ncol(nuts3_distbandsCSV))])

names(nuts3_distbands) <- namez

#For now, drop sig
nuts3_distbands <- apply(nuts3_distbands, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#NAs so's graphing workz
#nuts3_distbands <- apply(nuts3_distbands, c(1,2), function(x) gsub("", NA, x)) %>% data.frame

NUTS3names <- c(
  'AngusDundee',
  'ClackmannanshireFife',
  'EastLothianMidlothian',
  'ScottishBorders',
  'Edinburgh',
  'Falkirk',
  'PerthKinrossStirling',
  'WestLothian',
  'EastDunbartonshire,WestDunbartonshireHelensburghLomond',
  'DumfriesGalloway',
  'EastNorthAyrshiremainland',
  'Glasgow',
  'InverclydeEastRenfrewshireRenfrewshire',
  'NorthLanarkshire',
  'SouthAyrshire',
  'SouthLanarkshire',
  'AberdeenAberdeenshire',
  'CaithnessSutherlandRossCromarty',
  'InvernessNairnMorayBadenochStrathspey',
  'LochaberSkyeLochalshArranCumbraeArgyllBute',
  'EileanSiar',
  'OrkneyIslands',
  'ShetlandIslands'
)

#For now, drop regions with no values or stupid values
nuts3_distbands <- nuts3_distbands %>% dplyr::select(-c(NUTS5,NUTS6,NUTS8,NUTS9,NUTS23))

NUTS3names <- NUTS3names[c(1:4,7,10:22)]

names(nuts3_distbands) <- c('band',NUTS3names)

nuts3_distbands_MELT <- gather(nuts3_distbands, nuts3zone, coeff, AngusDundee:OrkneyIslands)

nuts3_distbands_MELT$band <- factor(nuts3_distbands_MELT$band[1:6], levels = nuts3_distbands_MELT$band[1:6],
                                     ordered = T)

nuts3_distbands_MELT$coeff <- as.numeric(nuts3_distbands_MELT$coeff)


output <- ggplot(nuts3_distbands_MELT, aes(x = band, y = coeff, group = nuts3zone)) +
  geom_point(size = 1.5) +
  geom_line() + 
  #geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 0), aes(group = model), alpha = 0.6) +
  #geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 1), aes(group = model, alpha = 0.6)) +
  # facet_wrap(~nuts3zone, ncol = 3)
  facet_wrap(~nuts3zone, scales = 'free_y', ncol = 3) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1F_large_farms: Postcode/windfarm centroids, distance bands only, BY NUTS3, large windfarms only----

nuts3_distbandsLARGEFARMS_CSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/Table1F_largeFarms.csv", as.is = T)

#Just values, no standard errors
nuts3_distbandsLARGEFARMS_ <- nuts3_distbandsLARGEFARMS_CSV[seq(from = 5, to = 15, by = 2),]

#Get col names...
#Combine from name and FE
# namez <- paste0(nuts3_distbandsLARGEFARMS_CSV[3,c(2:ncol(nuts3_distbandsLARGEFARMS_CSV))], " ",
#                 nuts3_distbandsLARGEFARMS_CSV[20,c(2:ncol(nuts3_distbandsLARGEFARMS_CSV))])

#namez <- c("band",namez)
namez <- c("band",nuts3_distbandsLARGEFARMS_CSV[3,c(2:ncol(nuts3_distbandsLARGEFARMS_CSV))])

names(nuts3_distbandsLARGEFARMS_) <- namez

#For now, drop sig
nuts3_distbandsLARGEFARMS_ <- apply(nuts3_distbandsLARGEFARMS_, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#NAs so's graphing workz
#nuts3_distbandsLARGEFARMS_ <- apply(nuts3_distbandsLARGEFARMS_, c(1,2), function(x) gsub("", NA, x)) %>% data.frame

#THESE ARE THE RIGHT ONES!
NUTS3names <- c(
  'AngusDundee',
  'ClackmannanshireFife',
  'EastLothianMidlothian',
  'ScottishBorders',
  'Edinburgh',
  'Falkirk',
  'PerthKinrossStirling',
  'WestLothian',
  'EastDunbartonshire,WestDunbartonshireHelensburghLomond',
  'DumfriesGalloway',
  'EastNorthAyrshiremainland',
  'Glasgow',
  'InverclydeEastRenfrewshireRenfrewshire',
  'NorthLanarkshire',
  'SouthAyrshire',
  'SouthLanarkshire',
  'AberdeenAberdeenshire',
  'CaithnessSutherlandRossCromarty',
  'InvernessNairnMorayBadenochStrathspey',
  'LochaberSkyeLochalshArranCumbraeArgyllBute',
  'EileanSiar',
  'OrkneyIslands',
  'ShetlandIslands'
)

#For now, drop regions with no values or stupid values
nuts3_distbandsLARGEFARMS_ <- nuts3_distbandsLARGEFARMS_ %>% dplyr::select(-c(NUTS5,NUTS6,NUTS8,NUTS9,NUTS23))

NUTS3names <- NUTS3names[c(1:4,7,10:22)]

names(nuts3_distbandsLARGEFARMS_) <- c('band',NUTS3names)

nuts3_distbandsLARGEFARMS__MELT <- gather(nuts3_distbandsLARGEFARMS_, nuts3zone, coeff, AngusDundee:OrkneyIslands)
#nuts3_distbandsLARGEFARMS__MELT <- gather(nuts3_distbandsLARGEFARMS_, nuts3zone, coeff, NUTS1:NUTS22)

nuts3_distbandsLARGEFARMS__MELT$band <- factor(nuts3_distbandsLARGEFARMS__MELT$band[1:6], levels = nuts3_distbandsLARGEFARMS__MELT$band[1:6],
                                    ordered = T)

nuts3_distbandsLARGEFARMS__MELT$coeff <- as.numeric(nuts3_distbandsLARGEFARMS__MELT$coeff)


output <- ggplot(nuts3_distbandsLARGEFARMS__MELT, aes(x = band, y = coeff, group = nuts3zone)) +
  geom_point(size = 1.5) +
  geom_line() + 
  #geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 0), aes(group = model), alpha = 0.6) +
  #geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 1), aes(group = model, alpha = 0.6)) +
  # facet_wrap(~nuts3zone, ncol = 3)
  facet_wrap(~nuts3zone, scales = 'free_y', ncol = 3) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

#~~~~~~~~~~~~~~~~~~~~~
#Combine with previous----
combo <- nuts3_distbands_MELT
combo$source <- 'allFarms'

combo2 <- nuts3_distbandsLARGEFARMS__MELT
combo2$source <- 'largeFarms'

combo <- rbind(combo,combo2)

output <- ggplot(combo, aes(x = band, y = coeff, colour = source)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(combo,combo$source == 'allFarms'), aes(group = nuts3zone), alpha = 0.6) +
  geom_line(data = subset(combo,combo$source == 'largeFarms'), aes(group = nuts3zone), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~nuts3zone, scales = 'free_y', ncol = 3) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#F: Postcode/windfarm centroids, Viz/non-viz in bands: by NUTS3----

#Already in a slightly more manageable format
NUTS3_bands_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_all_tablesCSV/TableF.csv", as.is = T)

#Just values, no standard errors
NUTS3_bands_VizAndNon <- NUTS3_bands_VizAndNonCSV[seq(from = 8, to = 30, by = 2),]

#ALREADY GOT NAMES FROM FILE
#Get col names...
#Combine from name and FE
# namez <- paste0(NUTS3_bands_VizAndNonCSV[3,c(2:ncol(NUTS3_bands_VizAndNonCSV))], " ",
#                 NUTS3_bands_VizAndNonCSV[32,c(2:ncol(NUTS3_bands_VizAndNonCSV))])
# 
# namez <- c("band",namez)
# 
# names(NUTS3_bands_VizAndNon) <- namez

#For now, drop sig
NUTS3_bands_VizAndNon <- apply(NUTS3_bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Drop NUTS3s with no values / silly values
#NUTS3_bands_VizAndNon <- NUTS3_bands_VizAndNon %>% dplyr::select(-c(4,))

#This one's very holy. Try NAs.
NUTS3_bands_VizAndNon <- apply(NUTS3_bands_VizAndNon, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

names(NUTS3_bands_VizAndNon)[names(NUTS3_bands_VizAndNon)=='X'] <- 'band'

#Mark viz and non
NUTS3_bands_VizAndNon$visible <- 0 + grepl("_vis",NUTS3_bands_VizAndNon$band)

#Now we've got that, drop differences in band names
NUTS3_bands_VizAndNon$band <- gsub("_vis", "", NUTS3_bands_VizAndNon$band)
NUTS3_bands_VizAndNon$band <- gsub("_nvis", "", NUTS3_bands_VizAndNon$band)

#Ready to gather that mofo
NUTS3_bands_VizAndNon_final <- gather(NUTS3_bands_VizAndNon,model,coeff,Angus.and.Dundee:Shetland.Islands)

NUTS3_bands_VizAndNon_final$coeff <- as.numeric(NUTS3_bands_VizAndNon_final$coeff) * 100

#bands should be factors in the right order
NUTS3_bands_VizAndNon_final$band <- factor(NUTS3_bands_VizAndNon_final$band[1:6], levels = NUTS3_bands_VizAndNon_final$band[1:6],
                                     ordered = T)


#~~~~~~~~~~~~~~~~
#CIs
NUTS3_bands_VizAndNonSE <- NUTS3_bands_VizAndNonCSV[seq(from = 9, to = 31, by = 2),]

NUTS3_bands_VizAndNonSE$X <- NUTS3_bands_VizAndNonCSV$X[seq(from = 8, to = 30, by = 2)]

#drop brackets
NUTS3_bands_VizAndNonSE <- apply(NUTS3_bands_VizAndNonSE, c(1,2), function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Drop NUTS3s with no values / silly values
#NUTS3_bands_VizAndNonSE <- NUTS3_bands_VizAndNonSE %>% dplyr::select(-c(4,))

#This one's very holy. Try NAs.
NUTS3_bands_VizAndNonSE <- apply(NUTS3_bands_VizAndNonSE, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

names(NUTS3_bands_VizAndNonSE)[names(NUTS3_bands_VizAndNonSE)=='X'] <- 'band'

#Mark viz and non
NUTS3_bands_VizAndNonSE$visible <- 0 + grepl("_vis",NUTS3_bands_VizAndNonSE$band)

#Now we've got that, drop differences in band names
NUTS3_bands_VizAndNonSE$band <- gsub("_vis", "", NUTS3_bands_VizAndNonSE$band)
NUTS3_bands_VizAndNonSE$band <- gsub("_nvis", "", NUTS3_bands_VizAndNonSE$band)

#Ready to gather that mofo
NUTS3_bands_VizAndNonSE_final <- gather(NUTS3_bands_VizAndNonSE,model,SE,Angus.and.Dundee:Shetland.Islands)

NUTS3_bands_VizAndNonSE_final$SE <- as.numeric(NUTS3_bands_VizAndNonSE_final$SE) * 100

#bands should be factors in the right order
NUTS3_bands_VizAndNonSE_final$band <- factor(NUTS3_bands_VizAndNonSE_final$band[1:6], 
                                             levels = NUTS3_bands_VizAndNonSE_final$band[1:6],
                                           ordered = T)


#~~~~~~~~~~~~~~~~
#Combine
NUTS3_bands_VizAndNon_final$SE <- NUTS3_bands_VizAndNonSE_final$SE
NUTS3_bands_VizAndNon_final$min <- NUTS3_bands_VizAndNon_final$coeff - (NUTS3_bands_VizAndNon_final$SE * 1.96)
NUTS3_bands_VizAndNon_final$max <- NUTS3_bands_VizAndNon_final$coeff + (NUTS3_bands_VizAndNon_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

subz <- NUTS3_bands_VizAndNon_final
subz <- NUTS3_bands_VizAndNon_final[abs(NUTS3_bands_VizAndNon_final$SE) < 80 & !is.na(NUTS3_bands_VizAndNon_final$coeff),]


# output <- ggplot(NUTS3_bands_VizAndNon_final, aes(x = band, y = coeff, colour = factor(visible))) +
#   geom_point(size = 1.5) +
#   geom_line(data = subset(NUTS3_bands_VizAndNon_final,NUTS3_bands_VizAndNon_final$visible == 0), aes(group = model), alpha = 0.6) +
#   geom_line(data = subset(NUTS3_bands_VizAndNon_final,NUTS3_bands_VizAndNon_final$visible == 1), aes(group = model), alpha = 0.6) +
#   geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
#   # facet_wrap(~model, ncol = 3)
#   facet_wrap(~model, scales = 'free_y', ncol = 2) +
#   geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')
# 
# output
output <- ggplot(subz, aes(x = band, y = coeff, colour = factor(visible))) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 0), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 1), aes(group = model), alpha = 0.6) +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model, scales = 'free_y', ncol = 2) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/F1_centroids.png", dpi=150, width = 10, height = 12)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#postcode centroids, windfarms with extensions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Extensions 1C: Postcode/windfarm centroids, Viz/non-viz in bands----

bands_VizAndNon_extCSV <- read.csv("C:/Users/SMI2/Desktop/postcode_ext_tablesCSV/Table1C.csv", as.is = T)

#Just values, no standard errors
bands_VizAndNon_ext <- bands_VizAndNon_extCSV[seq(from = 5, to = 27, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNon_extCSV[3,c(2:ncol(bands_VizAndNon_extCSV))], " ",
                bands_VizAndNon_extCSV[36,c(2:ncol(bands_VizAndNon_extCSV))])

namez <- c("band",namez)

names(bands_VizAndNon_ext) <- namez

#For now, drop sig
bands_VizAndNon_ext <- apply(bands_VizAndNon_ext, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame
#Add NA
bands_VizAndNon_ext <- apply(bands_VizAndNon_ext, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
bands_VizAndNon_ext$visible <- 0 + grepl("_vis",bands_VizAndNon_ext$band)

#Now we've got that, drop differences in band names
#bands_VizAndNon_ext$bands[grepl("_vis",bands_VizAndNon_ext$bands)] <- gsub("_vis", "", bands_VizAndNon_ext$bands)
bands_VizAndNon_ext$band <- gsub("_vis", "", bands_VizAndNon_ext$band)
bands_VizAndNon_ext$band <- gsub("_nvis", "", bands_VizAndNon_ext$band)

#Ready to gather that mofo
bands_VizAndNon_ext_final <- gather(bands_VizAndNon_ext,model,coeff,2:16)

bands_VizAndNon_ext_final$coeff <- as.numeric(bands_VizAndNon_ext_final$coeff) * 100

#bands should be factors in the right order
bands_VizAndNon_ext_final$band <- factor(bands_VizAndNon_ext_final$band[1:6], levels = bands_VizAndNon_ext_final$band[1:6],
                                         ordered = T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Repeat for CIs
bands_VizAndNon_extSE <- bands_VizAndNon_extCSV[seq(from = 6, to = 28, by = 2),]

bands_VizAndNon_extSE$X <- bands_VizAndNon_extCSV$X[seq(from = 5, to = 27, by = 2)]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNon_extCSV[3,c(2:ncol(bands_VizAndNon_extCSV))], " ",
                bands_VizAndNon_extCSV[36,c(2:ncol(bands_VizAndNon_extCSV))])

namez <- c("band",namez)

names(bands_VizAndNon_extSE) <- namez

#Mark viz and non
bands_VizAndNon_extSE$visible <- 0 + grepl("_vis",bands_VizAndNon_extSE$band)

#Now we've got that, drop differences in band names
#bands_VizAndNon_extSE$bands[grepl("_vis",bands_VizAndNon_extSE$bands)] <- gsub("_vis", "", bands_VizAndNon_extSE$bands)
bands_VizAndNon_extSE$band <- gsub("_vis", "", bands_VizAndNon_extSE$band)
bands_VizAndNon_extSE$band <- gsub("_nvis", "", bands_VizAndNon_extSE$band)

#remove brackets
bands_VizAndNon_extSE <- apply(bands_VizAndNon_extSE, 2, function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Ready to gather that mofo
bands_VizAndNon_extSE_final <- gather(bands_VizAndNon_extSE,model,SE,2:16)

bands_VizAndNon_extSE_final$SE <- as.numeric(bands_VizAndNon_extSE_final$SE) * 100

#bands should be factors in the right order
bands_VizAndNon_extSE_final$band <- factor(bands_VizAndNon_extSE_final$band[1:6], levels = bands_VizAndNon_extSE_final$band[1:6],
                                           ordered = T)

#~~~~~~~~~~~~~~~~~~~~~~
#Combine
bands_VizAndNon_ext_final$SE <- bands_VizAndNon_extSE_final$SE
bands_VizAndNon_ext_final$min <- bands_VizAndNon_ext_final$coeff - (bands_VizAndNon_ext_final$SE * 1.96)
bands_VizAndNon_ext_final$max <- bands_VizAndNon_ext_final$coeff + (bands_VizAndNon_ext_final$SE * 1.96)

dodge <- position_dodge(width=0.2)


output <- ggplot(bands_VizAndNon_ext_final, aes(x = band, y = coeff, colour = factor(visible))) +
  geom_point(size = 1.5) +
  geom_line(data = subset(bands_VizAndNon_ext_final,bands_VizAndNon_ext_final$visible == 0), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(bands_VizAndNon_ext_final,bands_VizAndNon_ext_final$visible == 1), aes(group = model), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model, scales = 'free_y', ncol = 3) +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#INDIVIDUAL LEVEL:------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1C: Individual repeat-sales, Viz/non-viz in bands----

#Already in a slightly more manageable format
#bands_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/individual_level_tablesCSV/Table1C.csv", as.is = T)
#bands_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/individual_level_tablesCSV/Table1C_minusNaughtyNUTS3zones.csv", as.is = T)

#test version
# bands_VizAndNonCSV <- read.csv("C:/Data/WindFarmViewShed/Tests/StataTests/WindFarmsII/results_repeat_sales/csvs/TableC1_syntheticTest.csv", as.is = T)
bands_VizAndNonCSV <- read.csv("data/regressionTables/Table1C_terrain.csv", as.is = T)
#add row to adjust to correct row number for below
#bands_VizAndNonCSV <- rbind(rep(" ", times = ncol(bands_VizAndNonCSV)),bands_VizAndNonCSV)

#Change turbine less/more than names. They get lost.
#bands_VizAndNonCSV$X.7.[3] <- 'Turbine.lessThan100m'
#bands_VizAndNonCSV$X.8.[3] <- 'Turbine.moreThan100m'

#drop some cols
names(bands_VizAndNonCSV)
bands_VizAndNonCSV <- bands_VizAndNonCSV %>% dplyr::select(-c(X.5.:X.6., X.9., X.11.))

#Just values, no standard errors
bands_VizAndNon <- bands_VizAndNonCSV[seq(from = 5, to = 27, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNonCSV[3,c(2:ncol(bands_VizAndNonCSV))], " ",
                bands_VizAndNonCSV[32,c(2:ncol(bands_VizAndNonCSV))])

namez <- c("band",namez)

names(bands_VizAndNon) <- namez

#For now, drop sig
bands_VizAndNon <- apply(bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Mark viz and non
bands_VizAndNon$visible <- 0 + grepl("_vis",bands_VizAndNon$band)

#Now we've got that, drop differences in band names
#bands_VizAndNon$bands[grepl("_vis",bands_VizAndNon$bands)] <- gsub("_vis", "", bands_VizAndNon$bands)
bands_VizAndNon$band <- gsub("_vis", "", bands_VizAndNon$band)
bands_VizAndNon$band <- gsub("_nvis", "", bands_VizAndNon$band)

#Ready to gather that mofo
bands_VizAndNon_final <- gather(bands_VizAndNon,model,coeff,2:(ncol(bands_VizAndNon)-1) )

bands_VizAndNon_final$coeff <- as.numeric(bands_VizAndNon_final$coeff) * 100

#bands should be factors in the right order
bands_VizAndNon_final$band <- factor(bands_VizAndNon_final$band[1:6], levels = bands_VizAndNon_final$band[1:6],
                                     ordered = T)

#~~~~~~~~~~~
#Repeat for CIs
bands_VizAndNonSE <- bands_VizAndNonCSV[seq(from = 6, to = 28, by = 2),]

bands_VizAndNonSE$X <- bands_VizAndNonCSV$X[seq(from = 5, to = 27, by = 2)]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNonCSV[3,c(2:ncol(bands_VizAndNonCSV))], " ",
                bands_VizAndNonCSV[32,c(2:ncol(bands_VizAndNonCSV))])

namez <- c("band",namez)

names(bands_VizAndNonSE) <- namez

#Mark viz and non
bands_VizAndNonSE$visible <- 0 + grepl("_vis",bands_VizAndNonSE$band)

#Now we've got that, drop differences in band names
#bands_VizAndNonSE$bands[grepl("_vis",bands_VizAndNonSE$bands)] <- gsub("_vis", "", bands_VizAndNonSE$bands)
bands_VizAndNonSE$band <- gsub("_vis", "", bands_VizAndNonSE$band)
bands_VizAndNonSE$band <- gsub("_nvis", "", bands_VizAndNonSE$band)

#remove brackets
bands_VizAndNonSE <- apply(bands_VizAndNonSE, 2, function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Ready to gather that mofo
bands_VizAndNonSE_final <- gather(bands_VizAndNonSE,model,SE,2:(ncol(bands_VizAndNonSE)-1)  )

# bands_VizAndNonSE_final$SE <- abs(as.numeric(bands_VizAndNonSE_final$SE) * 100)
bands_VizAndNonSE_final$SE <- as.numeric(bands_VizAndNonSE_final$SE) * 100

#bands should be factors in the right order
bands_VizAndNonSE_final$band <- factor(bands_VizAndNonSE_final$band[1:6], levels = bands_VizAndNonSE_final$band[1:6],
                                       ordered = T)

#~~~~~~~~~~~
#Combine, graph
bands_VizAndNon_final$SE <- bands_VizAndNonSE_final$SE
bands_VizAndNon_final$min <- bands_VizAndNon_final$coeff - (bands_VizAndNon_final$SE * 1.96)
bands_VizAndNon_final$max <- bands_VizAndNon_final$coeff + (bands_VizAndNon_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

bands_VizAndNon_final$visible <- factor(bands_VizAndNon_final$visible, labels = c('no','yes'))


labelz = c(
  'FE: properties...',
  'FE: plus geography',
  'FE: plus rings',
  'FE: plus NUTS2',
  #'FE: plus population centres',
  #'NUTS3 means',
  'Turbines < 100m',
  'Turbines > 100m',
  #'Coast < 1km',
  'Coast < 2km',
  #'Coast > 1km',
  'Coast > 2km'
)

letterz <- sapply(LETTERS[seq(1:length(labelz))], function(x) paste0('(',x,')'))

labelz <- sapply(seq(1:length(labelz)), function(x) paste0(letterz[x],' ',labelz[x]))

bands_VizAndNon_final$model <- factor(bands_VizAndNon_final$model,
                                      labels = labelz)


#band labels
bands_VizAndNon_final$band <- factor(bands_VizAndNon_final$band, 
                                      labels = c(
                                        '0-2km',
                                        '2-3km',
                                        '3-4km',
                                        '4-5km',
                                        '5-8km',
                                        '8-14km'
                                      ))

#error bars don't draw if they're beyond range
#fix by reducing them to the range
minz = -15
maxz = 15

#not working: needs correcting in the dataset. Annoying.
# bands_VizAndNon_final$min1 <- ifelse(bands_VizAndNon_final$min < minz, minz, bands_VizAndNon_final$min) 
# bands_VizAndNon_final$max1 <- ifelse(bands_VizAndNon_final$max > maxz, maxz, bands_VizAndNon_final$max) 

#Oh: min and max got themselves backwards...

output <- ggplot(bands_VizAndNon_final, aes(x = band, y = coeff, colour = visible)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 'no'), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 'yes'), aes(group = model), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model,  ncol = 2) +
  # facet_wrap(~model, scales = 'free_y', ncol = 2) +
  ylab('percent') +
  xlab('distance band') + 
  # geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  scale_y_continuous(limits=c(minz,maxz)) +
  geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/C1_repeats.png", dpi=150, width = 8, height = 8)
#ggsave("saves/C1_repeats_minus3NaughtyNUTS3zones.png", dpi=150, width = 10, height = 12)


#Repeat for first graph only
#Can't get it to select based on the label. Hmmph.
subz <- data.frame(bands_VizAndNon_final)
#Sod this, selecting by row
subz <- subz[1:12,]

output <- ggplot(subz, aes(x = band, y = coeff, colour = visible)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 'no'), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 'yes'), aes(group = model), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  # facet_wrap(~model, ncol = 2) +
  # facet_wrap(~model, scales = 'free_y', ncol = 2) +
  xlab("distance band") +
  ylab("percent") +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  # geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
  # scale_y_continuous(limits = c(minz,maxz)) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/C1_terrain_figure1.png", dpi=150, width = 7, height = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1C_blg: Individual repeat-sales, Viz/non-viz in bands FOR BUILDING HEIGHT LINES OF SIGHT, RIGHT?----

#Should be precisely the same format...
bands_VizAndNonCSV <- read.csv("data/regressionTables/Table1C_buildings.csv", as.is = T)

#Change turbine less/more than names. They get lost.
bands_VizAndNonCSV$X.7.[3] <- 'Turbine.lessThan100m'
bands_VizAndNonCSV$X.8.[3] <- 'Turbine.moreThan100m'

#Remove some cols
names(bands_VizAndNonCSV)
bands_VizAndNonCSV <- bands_VizAndNonCSV %>% dplyr::select(-c(X.5.:X.6., X.9., X.11.))

#Just values, no standard errors
bands_VizAndNon <- bands_VizAndNonCSV[seq(from = 5, to = 27, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNonCSV[3,c(2:ncol(bands_VizAndNonCSV))], " ",
                bands_VizAndNonCSV[32,c(2:ncol(bands_VizAndNonCSV))])

namez <- c("band",namez)

names(bands_VizAndNon) <- namez

#For now, drop sig
bands_VizAndNon <- apply(bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Mark viz and non
bands_VizAndNon$visible <- 0 + grepl("_vis",bands_VizAndNon$band)

#Now we've got that, drop differences in band names
#bands_VizAndNon$bands[grepl("_vis",bands_VizAndNon$bands)] <- gsub("_vis", "", bands_VizAndNon$bands)
bands_VizAndNon$band <- gsub("_vis", "", bands_VizAndNon$band)
bands_VizAndNon$band <- gsub("_nvis", "", bands_VizAndNon$band)

#Ready to gather that mofo
bands_VizAndNon_final <- gather(bands_VizAndNon,model,coeff,2:(ncol(bands_VizAndNonSE)-1))

bands_VizAndNon_final$coeff <- as.numeric(bands_VizAndNon_final$coeff) * 100

#bands should be factors in the right order
bands_VizAndNon_final$band <- factor(bands_VizAndNon_final$band[1:6], levels = bands_VizAndNon_final$band[1:6],
                                     ordered = T)

#~~~~~~~~~~~
#Repeat for CIs
bands_VizAndNonSE <- bands_VizAndNonCSV[seq(from = 6, to = 28, by = 2),]

bands_VizAndNonSE$X <- bands_VizAndNonCSV$X[seq(from = 5, to = 27, by = 2)]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_VizAndNonCSV[3,c(2:ncol(bands_VizAndNonCSV))], " ",
                bands_VizAndNonCSV[32,c(2:ncol(bands_VizAndNonCSV))])

namez <- c("band",namez)

names(bands_VizAndNonSE) <- namez

#Mark viz and non
bands_VizAndNonSE$visible <- 0 + grepl("_vis",bands_VizAndNonSE$band)

#Now we've got that, drop differences in band names
#bands_VizAndNonSE$bands[grepl("_vis",bands_VizAndNonSE$bands)] <- gsub("_vis", "", bands_VizAndNonSE$bands)
bands_VizAndNonSE$band <- gsub("_vis", "", bands_VizAndNonSE$band)
bands_VizAndNonSE$band <- gsub("_nvis", "", bands_VizAndNonSE$band)

#remove brackets
bands_VizAndNonSE <- apply(bands_VizAndNonSE, 2, function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Ready to gather that mofo
bands_VizAndNonSE_final <- gather(bands_VizAndNonSE,model,SE,2:(ncol(bands_VizAndNonSE)-1))

bands_VizAndNonSE_final$SE <- abs(as.numeric(bands_VizAndNonSE_final$SE) * 100)

#bands should be factors in the right order
bands_VizAndNonSE_final$band <- factor(bands_VizAndNonSE_final$band[1:6], levels = bands_VizAndNonSE_final$band[1:6],
                                       ordered = T)

#~~~~~~~~~~~
#Combine, graph
bands_VizAndNon_final$SE <- bands_VizAndNonSE_final$SE
bands_VizAndNon_final$min <- bands_VizAndNon_final$coeff - (bands_VizAndNon_final$SE * 1.96)
bands_VizAndNon_final$max <- bands_VizAndNon_final$coeff + (bands_VizAndNon_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

bands_VizAndNon_final$visible <- factor(bands_VizAndNon_final$visible, labels = c('no','yes'))

#Set better model names
#Split off different fixed effects ones
unique(bands_VizAndNon_final$model)

labelz = c(
  'FE: properties...',
  'FE: plus geography',
  'FE: plus rings',
  'FE: plus NUTS2',
  #'FE: plus population centres',
  #'NUTS3 means',
  'Turbines < 100m',
  'Turbines > 100m',
  #'Coast < 1km',
  'Coast < 2km',
  #'Coast > 1km',
  'Coast > 2km'
)

letterz <- sapply(LETTERS[seq(1:length(labelz))], function(x) paste0('(',x,')'))

labelz <- sapply(seq(1:length(labelz)), function(x) paste0(letterz[x],' ',labelz[x]))

bands_VizAndNon_final$model <- factor(bands_VizAndNon_final$model,
                                      labels = labelz)

#band labels
bands_VizAndNon_final$band

bands_VizAndNon_final$band <- factor(bands_VizAndNon_final$band, 
                                      labels = c(
                                        '0-2km',
                                        '2-3km',
                                        '3-4km',
                                        '4-5km',
                                        '5-8km',
                                        '8-14km'
                                      ))

minz = -15
maxz = 15

output <- ggplot(bands_VizAndNon_final, aes(x = band, y = coeff, colour = visible)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 'no'), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(bands_VizAndNon_final,bands_VizAndNon_final$visible == 'yes'), aes(group = model), alpha = 0.6) +
  #facet_wrap(~model2, scales = 'free_y', ncol = 2) +
  facet_wrap(~model, ncol = 2) +
  ylab('percent') +
  xlab('distance band') + 
  geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
  scale_y_continuous(limits = c(minz,maxz)) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/C1_repeats_buildingHeight.png", dpi=150, width = 8, height = 8)


#SINGLE FIGURE
subz <- data.frame(bands_VizAndNon_final)
#Sod this, selecting by row
subz <- subz[1:12,]

output <- ggplot(subz, aes(x = band2, y = coeff, colour = visible)) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 'no'), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 'yes'), aes(group = model), alpha = 0.6) +
  # facet_wrap(~model, ncol = 3)
  # facet_wrap(~model, ncol = 2) +
  # facet_wrap(~model, scales = 'free_y', ncol = 2) +
  xlab("distance band") +
  ylab("percent") +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  # geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
  # scale_y_continuous(limits = c(minz,maxz)) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/C1_BLG_figure1.png", dpi=150, width = 7, height = 4)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1D: Individual-level, Viz/non-viz in bands, then three groups for diff windfarm sizes----

bands_3groupsCSV <- read.csv("C:/Users/SMI2/Desktop/individual_level_tablesCSV/Table1D.csv", as.is = T)

#Just values, no standard errors
bands_3groups <- bands_3groupsCSV[seq(from = 5, to = 75, by = 2),]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_3groupsCSV[3,c(2:ncol(bands_3groupsCSV))], " ",
                bands_3groupsCSV[80,c(2:ncol(bands_3groupsCSV))])

namez <- c("band",namez)

names(bands_3groups) <- namez

#For now, drop sig
bands_3groups <- apply(bands_3groups, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame
#Also drop dashes for this one... 
bands_3groups <- apply(bands_3groups, c(1,2), function(x) gsub("\\-", "", x)) %>% data.frame

#Add NAs
bands_3groups <- apply(bands_3groups, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
bands_3groups$visible <- 0 + grepl("_vis",bands_3groups$band)

#Mark which turbine size group
bands_3groups$sizeGroup <- '1-10'
bands_3groups$sizeGroup[grepl("_b2",bands_3groups$band)] <- '11-20'
bands_3groups$sizeGroup[grepl("_b3",bands_3groups$band)] <- '+20'

bands_3groups$sizeGroup <- factor(bands_3groups$sizeGroup[1:3], levels = bands_3groups$sizeGroup[1:3])

#Now we've got that, drop differences in band names
#bands_3groups$bands[grepl("_vis",bands_3groups$bands)] <- gsub("_vis", "", bands_3groups$bands)
bands_3groups$band <- gsub("_vis", "", bands_3groups$band)
bands_3groups$band <- gsub("_nvis", "", bands_3groups$band)
bands_3groups$band <- gsub("_b1", "", bands_3groups$band)
bands_3groups$band <- gsub("_b2", "", bands_3groups$band)
bands_3groups$band <- gsub("_b3", "", bands_3groups$band)

#Ready to gather that mofo
bands_3groups_final <- gather(bands_3groups,model,coeff,2:(ncol(bands_3groups)-2))

bands_3groups_final$coeff <- as.numeric(bands_3groups_final$coeff) * 100

#bands should be factors in the right order
bands_3groups_final$band <- factor(bands_3groups_final$band[1:18], levels = unique(bands_3groups_final$band),
                                   ordered = T)


#~~~~~~~~~~~~~~~
#CIs
bands_3groupsSE <- bands_3groupsCSV[seq(from = 5, to = 76, by = 2),]

bands_3groupsSE$X <- bands_3groupsCSV$X[seq(from = 5, to = 75, by = 2)]

#Get col names...
#Combine from name and FE
namez <- paste0(bands_3groupsCSV[3,c(2:ncol(bands_3groupsCSV))], " ",
                bands_3groupsCSV[80,c(2:ncol(bands_3groupsCSV))])

namez <- c("band",namez)

names(bands_3groupsSE) <- namez

#drop brackets
bands_3groupsSE <- apply(bands_3groupsSE, c(1,2), function(x) gsub("\\(|\\)", "", x)) %>% data.frame

#Add NAs
bands_3groupsSE <- apply(bands_3groupsSE, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
bands_3groupsSE$visible <- 0 + grepl("_vis",bands_3groupsSE$band)

#Mark which turbine size group
bands_3groupsSE$sizeGroup <- '1-10'
bands_3groupsSE$sizeGroup[grepl("_b2",bands_3groupsSE$band)] <- '11-20'
bands_3groupsSE$sizeGroup[grepl("_b3",bands_3groupsSE$band)] <- '+20'

bands_3groupsSE$sizeGroup <- factor(bands_3groupsSE$sizeGroup[1:3], levels = bands_3groupsSE$sizeGroup[1:3])

#Now we've got that, drop differences in band names
#bands_3groupsSE$bands[grepl("_vis",bands_3groupsSE$bands)] <- gsub("_vis", "", bands_3groupsSE$bands)
bands_3groupsSE$band <- gsub("_vis", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_nvis", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_b1", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_b2", "", bands_3groupsSE$band)
bands_3groupsSE$band <- gsub("_b3", "", bands_3groupsSE$band)

#Ready to gather that mofo
bands_3groupsSE_final <- gather(bands_3groupsSE,model,SE,2:(ncol(bands_3groupsSE)-2))

bands_3groupsSE_final$SE <- as.numeric(bands_3groupsSE_final$SE) * 100

#bands should be factors in the right order
bands_3groupsSE_final$band <- factor(bands_3groupsSE_final$band[1:18], levels = unique(bands_3groupsSE_final$band),
                                     ordered = T)

#~~~~
#Combine
bands_3groups_final$SE <- bands_3groupsSE_final$SE
bands_3groups_final$min <- bands_3groups_final$coeff - (bands_3groups_final$SE * 1.96)
bands_3groups_final$max <- bands_3groups_final$coeff + (bands_3groups_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

#subset to group
# subz <- bands_3groups_final %>% filter(sizeGroup == '1-10')
# subz <- bands_3groups_final %>% filter(sizeGroup == '11-20')
# subz <- bands_3groups_final %>% filter(sizeGroup == '+20')
#subz <- bands_3groups_final[abs(bands_3groups_final$coeff) < 30,]
subz <- bands_3groups_final

#WOOP! WOOP! CIs GONE WRONG HERE SOMEWHERE.

output <- ggplot(subz, aes(x = band, y = coeff, colour = factor(visible))) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 0), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 1), aes(group = model, alpha = 0.6)) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model+sizeGroup, scales = 'free_y', ncol = 6) +
  # facet_wrap(~model+sizeGroup, scales = 'free_y') +
  # facet_wrap(~model+sizeGroup) +
  #scale_y_continuous(limits = c(-10, 10)) +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1F: NUTS3 regions viz/non-viz------
# 
# NUTS3_bands_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/individual_level_tablesCSV/Table1F.csv", as.is = T)
# 
# #Just values, no standard errors
# NUTS3_bands_VizAndNon <- NUTS3_bands_VizAndNonCSV[seq(from = 5, to = 27, by = 2),]
# 
# names(NUTS3_bands_VizAndNon) <- c('band',NUTS3_bands_VizAndNonCSV[3,c(2:ncol(NUTS3_bands_VizAndNonCSV))])
# 
# #THESE ARE THE RIGHT ONES!
# NUTS3names <- c(
#   'AngusDundee',
#   'ClackmannanshireFife',
#   'EastLothianMidlothian',
#   'ScottishBorders',
#   'Edinburgh',
#   'Falkirk',
#   'PerthKinrossStirling',
#   'WestLothian',
#   'EastDunbartonshire,WestDunbartonshireHelensburghLomond',
#   'DumfriesGalloway',
#   'EastNorthAyrshiremainland',
#   'Glasgow',
#   'InverclydeEastRenfrewshireRenfrewshire',
#   'NorthLanarkshire',
#   'SouthAyrshire',
#   'SouthLanarkshire',
#   'AberdeenAberdeenshire',
#   'CaithnessSutherlandRossCromarty',
#   'InvernessNairnMorayBadenochStrathspey',
#   'LochaberSkyeLochalshArranCumbraeArgyllBute',
#   'EileanSiar',
#   'OrkneyIslands',
#   'ShetlandIslands'
# )
# 
# 
# names(NUTS3_bands_VizAndNon) <- c('band',NUTS3names)
# 
# #For now, drop sig
# NUTS3_bands_VizAndNon <- apply(NUTS3_bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame
# 
# #Drop NUTS3s with no values / silly values
# #NUTS3_bands_VizAndNon <- NUTS3_bands_VizAndNon %>% dplyr::select(-c(4,))
# 
# #This one's very holy. Try NAs.
# NUTS3_bands_VizAndNon <- apply(NUTS3_bands_VizAndNon, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame
# 
# #Mark viz and non
# NUTS3_bands_VizAndNon$visible <- 0 + grepl("_vis",NUTS3_bands_VizAndNon$band)
# 
# #Now we've got that, drop differences in band names
# NUTS3_bands_VizAndNon$band <- gsub("_vis", "", NUTS3_bands_VizAndNon$band)
# NUTS3_bands_VizAndNon$band <- gsub("_nvis", "", NUTS3_bands_VizAndNon$band)
# 
# #Ready to gather that mofo
# NUTS3_bands_VizAndNon_final <- gather(NUTS3_bands_VizAndNon,model,coeff,2:24)
# 
# NUTS3_bands_VizAndNon_final$coeff <- as.numeric(NUTS3_bands_VizAndNon_final$coeff) * 100
# 
# #bands should be factors in the right order
# NUTS3_bands_VizAndNon_final$band <- factor(NUTS3_bands_VizAndNon_final$band[1:6], levels = NUTS3_bands_VizAndNon_final$band[1:6],
#                                            ordered = T)
# 
# output <- ggplot(NUTS3_bands_VizAndNon_final, aes(x = band, y = coeff, colour = factor(visible))) +
#   geom_point(size = 1.5) +
#   geom_line(data = subset(NUTS3_bands_VizAndNon_final,NUTS3_bands_VizAndNon_final$visible == 0), aes(group = model), alpha = 0.6) +
#   geom_line(data = subset(NUTS3_bands_VizAndNon_final,NUTS3_bands_VizAndNon_final$visible == 1), aes(group = model), alpha = 0.6) +
#   # facet_wrap(~model, ncol = 3)
#   facet_wrap(~model, scales = 'free_y', ncol = 3) +
#   geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')
# 
# output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Repeat whole thing, add CIs and mark sig. 1F: NUTS3 regions viz/non-viz------

NUTS3_bands_VizAndNonCSV <- read.csv("C:/Users/SMI2/Desktop/individual_level_tablesCSV/Table1F.csv", as.is = T)





#Just values, no standard errors
NUTS3_bands_VizAndNon <- NUTS3_bands_VizAndNonCSV[seq(from = 5, to = 27, by = 2),]

names(NUTS3_bands_VizAndNon) <- c('band',NUTS3_bands_VizAndNonCSV[3,c(2:ncol(NUTS3_bands_VizAndNonCSV))])

#THESE ARE THE RIGHT ONES!
NUTS3names <- c(
  'AngusDundee',
  'ClackmannanshireFife',
  'EastLothianMidlothian',
  'ScottishBorders',
  'Edinburgh',
  'Falkirk',
  'PerthKinrossStirling',
  'WestLothian',
  'EastDunbartonshire,WestDunbartonshireHelensburghLomond',
  'DumfriesGalloway',
  'EastNorthAyrshiremainland',
  'Glasgow',
  'InverclydeEastRenfrewshireRenfrewshire',
  'NorthLanarkshire',
  'SouthAyrshire',
  'SouthLanarkshire',
  'AberdeenAberdeenshire',
  'CaithnessSutherlandRossCromarty',
  'InvernessNairnMorayBadenochStrathspey',
  'LochaberSkyeLochalshArranCumbraeArgyllBute',
  'EileanSiar',
  'OrkneyIslands',
  'ShetlandIslands'
)

names(NUTS3_bands_VizAndNon) <- c('band',NUTS3names)

#For now, drop sig
NUTS3_bands_VizAndNon <- apply(NUTS3_bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Drop NUTS3s with no values / silly values
#NUTS3_bands_VizAndNon <- NUTS3_bands_VizAndNon %>% dplyr::select(-c(4,))

#This one's very holy. Try NAs.
NUTS3_bands_VizAndNon <- apply(NUTS3_bands_VizAndNon, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
NUTS3_bands_VizAndNon$visible <- 0 + grepl("_vis",NUTS3_bands_VizAndNon$band)

#Now we've got that, drop differences in band names
NUTS3_bands_VizAndNon$band <- gsub("_vis", "", NUTS3_bands_VizAndNon$band)
NUTS3_bands_VizAndNon$band <- gsub("_nvis", "", NUTS3_bands_VizAndNon$band)


#~~~~~~~~
#Repeat for standard error vals
NUTS3_bands_VizAndNonSE <- NUTS3_bands_VizAndNonCSV[seq(from = 6, to = 28, by = 2),]

NUTS3_bands_VizAndNonSE$X <- NUTS3_bands_VizAndNonCSV$X[seq(from = 5, to = 27, by = 2)]

#names(NUTS3_bands_VizAndNonSE) <- c('band',NUTS3_bands_VizAndNonCSV[3,c(2:ncol(NUTS3_bands_VizAndNonCSV))])

names(NUTS3_bands_VizAndNonSE) <- c('band',NUTS3names)

#This one's very holy. Try NAs.
NUTS3_bands_VizAndNonSE <- apply(NUTS3_bands_VizAndNonSE, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
NUTS3_bands_VizAndNonSE$visible <- 0 + grepl("_vis",NUTS3_bands_VizAndNonSE$band)

#Now we've got that, drop differences in band names
NUTS3_bands_VizAndNonSE$band <- gsub("_vis", "", NUTS3_bands_VizAndNonSE$band)
NUTS3_bands_VizAndNonSE$band <- gsub("_nvis", "", NUTS3_bands_VizAndNonSE$band)

#replace brackets
NUTS3_bands_VizAndNonSE <- apply(NUTS3_bands_VizAndNonSE, 2, function(x) gsub("\\(|\\)", "", x)) %>% data.frame

NUTS3_bands_VizAndNonSE_final <- gather(NUTS3_bands_VizAndNonSE,model,SE,2:24)

NUTS3_bands_VizAndNonSE_final$SE <- as.numeric(NUTS3_bands_VizAndNonSE_final$SE) * 100

#bands should be factors in the right order
NUTS3_bands_VizAndNonSE_final$band <- factor(NUTS3_bands_VizAndNonSE_final$band[1:6], 
                                             levels = NUTS3_bands_VizAndNonSE_final$band[1:6],
                                           ordered = T)

#~~~~~~~~

#Ready to gather that mofo
NUTS3_bands_VizAndNon_final <- gather(NUTS3_bands_VizAndNon,model,coeff,2:24)

NUTS3_bands_VizAndNon_final$coeff <- as.numeric(NUTS3_bands_VizAndNon_final$coeff) * 100

#bands should be factors in the right order
NUTS3_bands_VizAndNon_final$band <- factor(NUTS3_bands_VizAndNon_final$band[1:6], levels = NUTS3_bands_VizAndNon_final$band[1:6],
                                           ordered = T)

#~~~~~~~~
#Add SE - structure should match
NUTS3_bands_VizAndNon_final$SE <- NUTS3_bands_VizAndNonSE_final$SE
NUTS3_bands_VizAndNon_final$min <- NUTS3_bands_VizAndNon_final$coeff - (NUTS3_bands_VizAndNon_final$SE * 1.96)
NUTS3_bands_VizAndNon_final$max <- NUTS3_bands_VizAndNon_final$coeff + (NUTS3_bands_VizAndNon_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

#get list of any areas that have NA. Not enough obs all round there probs
#listRemz <- NUTS3_bands_VizAndNon_final$model[is.na(NUTS3_bands_VizAndNon_final$coeff)] %>% unique %>% 
 # as.character()

#add some others that don't have enough obs to be reliable
#Get sample sizes
sampleSizez <- NUTS3_bands_VizAndNonCSV[48,2:ncol(NUTS3_bands_VizAndNonCSV)] %>%  t
sampleSizez <- gsub(",","",sampleSizez) %>% as.numeric()
NUTS3names_sampleSize <- NUTS3names[sampleSizez > 50000]

#listRemz <- c(listRemz,NUTS3names_sampleSize) %>% unique
listRemz <- NUTS3names_sampleSize

subz <- NUTS3_bands_VizAndNon_final[abs(NUTS3_bands_VizAndNon_final$SE) < 80 
                                    #& !is.na(NUTS3_bands_VizAndNon_final$coeff)
                                    & (NUTS3_bands_VizAndNon_final$model %in% listRemz),]


minz = -40
maxz = 40


output <- ggplot(subz, aes(x = band, y = coeff, colour = factor(visible))) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 0), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 1), aes(group = model), alpha = 0.6) +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model, ncol = 2) +
  # facet_wrap(~model, scales = 'free_y', ncol = 2) +
  scale_y_continuous(limits=c(minz,maxz)) +
  geom_errorbar(width = 0.1, aes(ymin=ifelse(min < minz,minz,min), ymax=ifelse(max > maxz, maxz, max)), position = dodge) +
    geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

ggsave("saves/F1_repeats.png", dpi=150, width = 10, height = 12)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1F for NUTS2 viz/non-viz------

NUTS2_bands_VizAndNonCSV <- read.csv("data/regressionTables/Table1F_NUTS2a.csv", as.is = T)

#Just values, no standard errors
NUTS2_bands_VizAndNon <- NUTS2_bands_VizAndNonCSV[seq(from = 5, to = 27, by = 2),]

names(NUTS2_bands_VizAndNon) <- c('band',NUTS2_bands_VizAndNonCSV[3,c(2:ncol(NUTS2_bands_VizAndNonCSV))])

#THESE ARE THE RIGHT ONES!
NUTS2names <- c(
  'Eastern',
  'South Western',
  'North Eastern',
  'Highlands & Islands'
)


names(NUTS2_bands_VizAndNon) <- c('band',NUTS2names)

#For now, drop sig
NUTS2_bands_VizAndNon <- apply(NUTS2_bands_VizAndNon, c(1,2), function(x) gsub("\\*", "", x)) %>% data.frame

#Drop NUTS2s with no values / silly values
#NUTS2_bands_VizAndNon <- NUTS2_bands_VizAndNon %>% dplyr::select(-c(4,))

#This one's very holy. Try NAs.
NUTS2_bands_VizAndNon <- apply(NUTS2_bands_VizAndNon, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
NUTS2_bands_VizAndNon$visible <- 0 + grepl("_vis",NUTS2_bands_VizAndNon$band)

#Now we've got that, drop differences in band names
NUTS2_bands_VizAndNon$band <- gsub("_vis", "", NUTS2_bands_VizAndNon$band)
NUTS2_bands_VizAndNon$band <- gsub("_nvis", "", NUTS2_bands_VizAndNon$band)


#~~~~~~~~
#Repeat for standard error vals
NUTS2_bands_VizAndNonSE <- NUTS2_bands_VizAndNonCSV[seq(from = 6, to = 28, by = 2),]

NUTS2_bands_VizAndNonSE$X <- NUTS2_bands_VizAndNonCSV$X[seq(from = 5, to = 27, by = 2)]

#names(NUTS2_bands_VizAndNonSE) <- c('band',NUTS2_bands_VizAndNonCSV[3,c(2:ncol(NUTS2_bands_VizAndNonCSV))])

names(NUTS2_bands_VizAndNonSE) <- c('band',NUTS2names)

#This one's very holy. Try NAs.
NUTS2_bands_VizAndNonSE <- apply(NUTS2_bands_VizAndNonSE, 2, function(x) gsub("^$|^ $", NA, x)) %>% data.frame

#Mark viz and non
NUTS2_bands_VizAndNonSE$visible <- 0 + grepl("_vis",NUTS2_bands_VizAndNonSE$band)

#Now we've got that, drop differences in band names
NUTS2_bands_VizAndNonSE$band <- gsub("_vis", "", NUTS2_bands_VizAndNonSE$band)
NUTS2_bands_VizAndNonSE$band <- gsub("_nvis", "", NUTS2_bands_VizAndNonSE$band)

#replace brackets
NUTS2_bands_VizAndNonSE <- apply(NUTS2_bands_VizAndNonSE, 2, function(x) gsub("\\(|\\)", "", x)) %>% data.frame

NUTS2_bands_VizAndNonSE_final <- gather(NUTS2_bands_VizAndNonSE,model,SE,2:(ncol(NUTS2_bands_VizAndNonSE)-1) )

NUTS2_bands_VizAndNonSE_final$SE <- as.numeric(NUTS2_bands_VizAndNonSE_final$SE) * 100

#bands should be factors in the right order
NUTS2_bands_VizAndNonSE_final$band <- factor(NUTS2_bands_VizAndNonSE_final$band[1:6], 
                                             levels = NUTS2_bands_VizAndNonSE_final$band[1:6],
                                             ordered = T)

#~~~~~~~~

#Ready to gather that mofo
NUTS2_bands_VizAndNon_final <- gather(NUTS2_bands_VizAndNon,model,coeff,2:(ncol(NUTS2_bands_VizAndNon)-1 ))

NUTS2_bands_VizAndNon_final$coeff <- as.numeric(NUTS2_bands_VizAndNon_final$coeff) * 100

#bands should be factors in the right order
NUTS2_bands_VizAndNon_final$band <- factor(NUTS2_bands_VizAndNon_final$band[1:6], levels = NUTS2_bands_VizAndNon_final$band[1:6],
                                           ordered = T)

#~~~~~~~~
#Add SE - structure should match
NUTS2_bands_VizAndNon_final$SE <- NUTS2_bands_VizAndNonSE_final$SE
NUTS2_bands_VizAndNon_final$min <- NUTS2_bands_VizAndNon_final$coeff - (NUTS2_bands_VizAndNon_final$SE * 1.96)
NUTS2_bands_VizAndNon_final$max <- NUTS2_bands_VizAndNon_final$coeff + (NUTS2_bands_VizAndNon_final$SE * 1.96)

dodge <- position_dodge(width=0.2)

#subz <- NUTS2_bands_VizAndNon_final[abs(NUTS2_bands_VizAndNon_final$SE) < 80 
#                                    & !is.na(NUTS2_bands_VizAndNon_final$coeff)

subz <- NUTS2_bands_VizAndNon_final
                                    
output <- ggplot(subz, aes(x = band, y = coeff, colour = factor(visible))) +
  geom_point(size = 1.5) +
  geom_line(data = subset(subz,subz$visible == 0), aes(group = model), alpha = 0.6) +
  geom_line(data = subset(subz,subz$visible == 1), aes(group = model), alpha = 0.6) +
  geom_errorbar(width = 0.1, aes(ymin=min, ymax=max), position = dodge) +
  # facet_wrap(~model, ncol = 3)
  facet_wrap(~model, scales = 'free_y', ncol = 2) +
  geom_hline(yintercept=0, alpha = 0.3, colour = 'blue')

output

#ggsave("saves/F1_repeats.png", dpi=150, width = 10, height = 12)











