#comparing older versions of turbine data to what we ended up with
library(dplyr)
library(tidyr)
library(pryr)
library(zoo)
library(ggplot2)
library(readstata13)

#load possible old turbine files...
oldTurbs <- read.dta13("C:/Users/SMI2/Dropbox/WindFarms/Data/turbinedata/turbine_data.dta")

#How does that compare on a map to what we currently have?
write.csv(oldTurbs, "data/oldTurbinesFromPrevProject.csv")

#Load new to compare dates
newTurbs <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/turbinesFinal_reducedColumns_tipHeightsComplete.csv")
newTurbs$statusDateFormatted <- as.Date(newTurbs$statusDateFormatted)

#Graph based on height - are dates different?
newTurbs$tipMoreThan100m <- 0
newTurbs$tipMoreThan100m[newTurbs$TipHeight >100] <- 1

ggplot(newTurbs, aes(x=statusDateFormatted, fill=factor(tipMoreThan100m))) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", position = "identity", colour="black", binwidth=365)


ggplot(newTurbs, aes(x=statusDateFormatted, y =TipHeight)) + 
  geom_point()

#~~~~~~~~~~~~~~~~~~~~~~~~
#Windfarms by size
windfarms <- newTurbs %>% group_by(nameMinusTurbine) %>% 
  summarise(count = n())

windfarms <- windfarms %>% arrange(-count)








