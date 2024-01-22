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
city <- read.csv("23.cities.data.csv")

# select relevant columns and get coordinates for each city 
city<-dplyr::select(city, City, Lon, Lat)
city <- data.frame(city$City, city$Lon, city$Lat, row.names=city$City)
city<-dplyr::select(city, -city.City)
colnames (city) = c("lon", "lat")

#--------------------------------------------------------------------------------------------------------------------------------
## read current (1970-2000) worldclim bioclim data from temperature and precipitation rasters

temp<-raster("wc2.1_5m_bio_5.tif")
precip<-raster("wc2.1_5m_bio_14.tif")

# extract info for each city
city.1<-city
city.1$temp.hottest.month <- extract(temp, city)

city.2<-city

#--------------------------------------------------------------------------------------------------------------
##read chelsa data (baseline, 2050, 2070) from folder 

#first import all files in a single folder as a list 
rastlist <- list.files(pattern='CHELSA', all.files=TRUE, full.names=FALSE)

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

