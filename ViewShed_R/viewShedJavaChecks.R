library(ggplot2)
library(reshape2)
library(stringr)


#filenames <- Sys.glob("C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/*.csv")

filenames <- list.files(path = "C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/",
                        pattern = "*.csv", 
                        recursive = T,full.names=T)

#Just building heights
filenames <- list.files(path = "C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/withBuildingHeights/",
                        pattern = "*.csv", 
                        recursive = F,full.names=T)

count = 0



#for (f in filenames) {
#test on one
for (f in filenames) {

  ht <- read.csv(f)
  ht$index <- row.names(ht)
  #names(ht)[names(ht)=="heights"] <- "DEM"
  
  distance = ht$dist[1]
  
  #then drop the distance column
  ht <- ht[,2:ncol(ht)]
  
  hasBuildingHeights <- ifelse(length(names(ht))==5, T, F)
  
  #count = count + 1
  
  #cos plot isn't letting me easily control size
  # htm <- melt(ht, measure.vars=c('DEM','lineofsight'))
  htm <- melt(ht, id.vars=c('canISeeYou','index'))
  # htm <- melt(ht[,c(2:4)])
  #convert to metres
  htm$value <- htm$value * 5
  
  if(hasBuildingHeights) {
    
    cols <- ifelse(htm$canISeeYou == 1, c("#009933","#E69F00", "#56B4E9"), c("#009933","#E69F00", "#999999"))
  
    output <- ggplot(htm, aes(x = as.numeric(index), y=value, colour=variable)) +
      geom_line(size = 0.75) +
      # geom_line(stat='identity')
      scale_color_manual(values=cols) +
      xlab("bresenham line") +
      ylab("height (metres)")
    
    output
  
  } else {
    
    cols <- ifelse(htm$canISeeYou == 1, c("#E69F00", "#56B4E9"), c("#E69F00", "#999999"))
    
    output <- ggplot(htm, aes(x = as.numeric(index), y=value, colour=variable)) +
      geom_line(size = 0.75) +
      # geom_line(stat='identity')
      scale_color_manual(values=cols) +
      xlab("bresenham line") +
      ylab("height (metres)")
    
    output
    
  }
  
  filen = strsplit(basename(f),"\\.")[[1]][1]
  
  fullname = ifelse(hasBuildingHeights,
            paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/withBuildingHeights/images/',filen,'.jpg'),
            paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/noBuildingHeights/images/',filen,'.jpg'))
  
  #keep consistent scale
  thisheight = 3
  rng <- max(htm$value)-min(htm$value)
  aunit <- rng/thisheight#the height
  widthwillbe <- distance/aunit/10
  
  ggsave(fullname,
         output,
         dpi=200, width=widthwillbe,height=thisheight)
  

}



#Use actual distance in x lab and generally tidy for publication
for (f in filenames) {
  
  ht <- read.csv(f)
  ht$index <- row.names(ht)
  #names(ht)[names(ht)=="heights"] <- "DEM"
  
  distance = ht$dist[1]
  
  #then drop the distance column
  #ht <- ht[,2:ncol(ht)]
  
  #Add a new distance column with the distance spread along the Bresenham Line
  ht$dist <- distance * (as.numeric(ht$index)/nrow(ht))
  
  hasBuildingHeights <- ifelse(length(names(ht))==6, T, F)
  
  
  #count = count + 1
  
  #cos plot isn't letting me easily control size
  # htm <- melt(ht, measure.vars=c('DEM','lineofsight'))
  #change names before melt
  names(ht) <- c('dist','canISeeYou','DEM with buildings', 'DEM no buildings', 'line of sight', 'index')
  
  #hack to check
  #ht$`DEM with buildings` <- ht$`DEM with buildings`/5
  
  htm <- melt(ht, id.vars=c('canISeeYou','index', 'dist'))
  # htm <- melt(ht[,c(2:4)])
  #convert to metres
  htm$value <- htm$value * 5
  
  #set horizontal distance scale relative to height (for output below)
  scale = 8
  
  if(hasBuildingHeights) {
    
    cols <- ifelse(htm$canISeeYou == 1, c("#009933","#E69F00", "#56B4E9"), c("#009933","#E69F00", "#999999"))
    
    output <- ggplot(htm, aes(x = as.numeric(dist), y=value, colour=variable)) +
      geom_line(size = 0.75) +
      # geom_line(stat='identity')
      scale_color_manual(values=cols) +
      xlab(paste0("distance (metres, 1:",scale," scale relative to height)")) +
      ylab("height (metres)") +
      theme(legend.title=element_blank())
    
    output
    
  } else {
    
    cols <- ifelse(htm$canISeeYou == 1, c("#E69F00", "#56B4E9"), c("#E69F00", "#999999"))
    
    output <- ggplot(htm, aes(x = as.numeric(dist), y=value, colour=variable)) +
      geom_line(size = 0.75) +
      # geom_line(stat='identity')
      scale_color_manual(values=cols) +
      xlab(paste0("distance (metres, 1:",scale," scale relative to height)")) +
      ylab("height (metres)") +
      theme(legend.title=element_blank())
    
    output
    
  }
  
  filen = strsplit(basename(f),"\\.")[[1]][1]
  
  fullname = ifelse(hasBuildingHeights,
                    paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/withBuildingHeights/images/',filen,'.jpg'),
                    paste0('C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/noBuildingHeights/images/',filen,'.jpg'))
  
  #keep consistent scale
  thisheight = 2.5
  #diff in height in metres
  rng <- max(htm$value)-min(htm$value)
  aunit <- rng/thisheight#the height
  widthwillbe <- distance/aunit/scale
  
  # ggsave(fullname,
  #        output,
  #        dpi=200, width=widthwillbe,height=thisheight, limitsize = F)
  # 
  #messing for tree data
  ggsave(fullname,
         output,
         dpi=200, width=8,height=5, limitsize = F)
  
  
}












#Look at start of line at scale
# f = "C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/withBuildingHeights/lineOfSight_target1001_batch_0.csv"
# 
# ht <- read.csv(f)
# ht$index <- row.names(ht)
# #names(ht)[names(ht)=="heights"] <- "DEM"
# 
# cutdown = 400
# 
# distance = ht$dist[1] * (cutdown/nrow(ht))
# ht <- ht[1:cutdown,]
# 
# #then drop the distance column
# ht <- ht[,2:ncol(ht)]
# 
# 
# 
# hasBuildingHeights <- ifelse(length(names(ht))==5, T, F)
# 
# #count = count + 1
# 
# #cos plot isn't letting me easily control size
# # htm <- melt(ht, measure.vars=c('DEM','lineofsight'))
# htm <- melt(ht, id.vars=c('canISeeYou','index'))
# # htm <- melt(ht[,c(2:4)])
# #convert to metres
# htm$value <- htm$value * 5
# 
# 
# 
# 
# cols <- ifelse(htm$canISeeYou == 1, c("#009933","#E69F00", "#56B4E9"), c("#009933","#E69F00", "#999999"))
# 
# output <- ggplot(htm, aes(x = as.numeric(index), y=value, colour=variable)) +
#   geom_line(size = 0.75) +
#   # geom_line(stat='identity')
#   scale_color_manual(values=cols) +
#   xlab("bresenham line") +
#   ylab("height (metres)")
# 
# output
# 
# #write to scale
# thisheight = 2
# rng <- max(htm$value)-min(htm$value)
# aunit <- rng/thisheight#the height
# widthwillbe <- distance/aunit
# 
# ggsave("C:/Data/WindFarmViewShed/ViewShedJava/SimpleViewShed/data/lineofsight/withBuildingHeights/images/scale.jpg",
#        output,
#        dpi=200, width=widthwillbe,height=thisheight)
