###############################################################################
############################ 3. CLIMATE DATA ##################################
###############################################################################

# Extracting CHELSA climate data for each city
# date = March 2021 
# Author = George Lloyd, University of Sheffield

# load necessary packages 
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(raster)

#-------------------------------------------------------------------------------------------------------------------
## load dataset with info on each city
city <- read.csv("Data/Species city and equation data/22.city.data.csv")

# select relevant columns and get coordinates for each city 
city<-select(city, City, Longitude, Latitude)
city <- data.frame(city$City, city$Lon, city$Lat, row.names=city$City)
city<-dplyr::select(city, -city.City)
colnames (city) = c("lon", "lat")


#--------------------------------------------------------------------------------------------------------------
##read chelsa data (current, 2050, 2070) from folder 

#first import all files in a single folder as a list 
rastlist <- list.files(path='Data/Chelsa climate data/current',pattern='CHELSA', full.names=FALSE)

# then convert to raster stack
allrasters <-stack(rastlist)

ras <- lapply(rastlist,raster) 

# get the data points for each city 
ext <- lapply(ras,extract, city) 
data<-as.data.frame(ext)
names(data)<-c(1,2,3,4,5)

# average across all models for each city
data$Avg_score = rowMeans(data[,c(1,2,3,4,5)])

# add onto city df
city.2$"current_max_temp_warm_month" = data$Avg_score
city.2$"current_precip_dry_month" = data$Avg_score
city.2$"2050_max_temp_warm_month" = data$Avg_score
city.2$"2050_precip_dry_month" = data$Avg_score
city.2$"2070_max_temp_warm_month" = data$Avg_score
city.2$"2070_precip_dry_month" = data$Avg_score

# save dataset
write.csv(data, "Chelsa.csv")

