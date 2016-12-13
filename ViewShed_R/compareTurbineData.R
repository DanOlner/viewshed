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
