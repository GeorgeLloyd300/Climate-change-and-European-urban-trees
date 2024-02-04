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
# this script shows how chelsa climate data for each city at current, 2050 and 2070 were extracted and 
# the average of 5 different models was calculated. Bio5 was used for temp, bio14 for prec - see methodology. 
# this was then added onto the "22.city.data.csv" so this script isnt necessary but just shows workings

# load dataset which has longitude and latitude data for each city
city <- read.csv("Data/Species city and equation data/22.city.data.csv")

# make a dataframe with only lon and lat for each city 
city <- city %>% select(City, Longitude, Latitude)
city <- data.frame(city$City, city$Lon, city$Lat, row.names=city$City)
city<-dplyr::select(city, -city.City)
colnames (city) = c("lon", "lat")


#--------------------------------------------------------------------------------------------------------------
##read chelsa data (either current, 2050, 2070 and 5(temp) or 14(prec)) from folder 

#first import all files in a single folder as a list 
rastlist <- list.files(path='Data/Chelsa climate data/2050/5',pattern='CHELSA', full.names=FALSE)

# then convert to raster stack
allrasters <-stack(rastlist)

ras <- lapply(rastlist, raster) 

# get the data points for each city 
ext <- lapply(ras, extract, city) 
data <- as.data.frame(ext)
names(data) <- c(1,2,3,4,5)

# average across all models for each city
data$Avg_score = rowMeans(data[,c(1,2,3,4,5)])

# add onto city df
city$"Temperature.2050" = data$Avg_score

# save dataset
write.csv(city, "22.city.data.csv")

