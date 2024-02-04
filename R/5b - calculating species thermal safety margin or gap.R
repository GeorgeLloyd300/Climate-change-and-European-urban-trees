###############################################################################
############################ 5b. CALC THERMAL MARGINS / GAPS ###################
###############################################################################

# Calculating each species thermal safety margin/gap
# date = March - June 2021 
# Author = George Lloyd, University of Sheffield

# Niches are calculated using two different scenarios and therefore this script needs to
# be repeated using the 2 different versions of 'new.niches' seen below
# 1 - a more conservative estimate using 5-95th percentiles 
# 2 - a less conservative estimate using 2-98th percentiles


# load necessary packages
library(tidyverse)
options(scipen = 999)

# read in relevant data :

# this data set contains all species niche data
new.niches <- read.csv("Outputs/Output step 4/species.niches.csv")

# data on each city
city.data  <- read.csv("Data/Species city and equation data/22.city.data.csv")

# master data set
Master_dataset <- read.csv("Outputs/Output step 2/master_tidy_step2.csv")

 
# -------------------------create master niches dataset -------------------------------------------

# this is a for loop that takes each species city combination at each time and calculates 
# whether or not the city's climate is within the species niche 
# the code should be altered slightly when using the 2 different versions of 'new.niches'
# for example, 'Max_temp_warm_month_q02' and 'Max_temp_warm_month_q98' below should be changed to 
# 'Max_temp_warm_month_q05' and 'Max_temp_warm_month_q95'

for(i in 1:nrow(city.data)) { 
  name <- city.data$City[i]
  
  # get climate in city
  climate<-(city.data%>% filter(City==name))
  a<-climate$temp.current 
  b<-climate$temp.2050
  c<-climate$temp.2070
  # get list of species in this city
  city<-filter(tidy.subset, city.name==name)
  city.sp<-unique(city$new.species)
  # subset niche data to get just these species niches
  # add new column showing if species is within its niche in future
  # true = citys temperature is within a species niche range (between p05 and p95 or p02 and p98)
  city.niches<-
    new.niches %>% select(searchTaxon, Max_temp_warm_month_q02 , Max_temp_warm_month_q98)%>% # this line should be changed if investigating different threshold or climatic variable (temperature or precipitation)
    subset(searchTaxon %in% city.sp) %>% mutate(city = name) %>%
    rename(species =searchTaxon, lower = Max_temp_warm_month_q02, upper= Max_temp_warm_month_q98)%>% # this line should be changed if investigating different threshold or climatic variable (temperature or precipitation)
    mutate(current=a)%>% mutate(n2050=b)%>% mutate(n2070=c)%>%
    mutate('within.current'= current <= upper)%>%
    mutate('within.2050'= n2050 <= upper)%>%
    mutate('within.2070'= n2070 <= upper)
  
  if(i==1) {
    master.niches <- city.niches
  } else {
    master.niches<-rbind(master.niches, city.niches)
  }
}

# -------------------- how far are species outside their niches -------------------------------------

# first make column of city and species so we can match
master.niches$city_species <- paste(master.niches$city, master.niches$species, sep="_")

# this below code works by asking : is the city within current niche? If not, then is it 
# above or below upper percentile (i.e. is the city temp hotter than the upper limit for a species thermal niche)
# Then it calculates how far above or within the niche that city is and repeats for each time point

## baseline ##
#if outside niche: how many degrees above 95th or 98th percentile?
outside<-master.niches %>% filter(within.current=="FALSE") %>% 
  mutate(safety.margin = current - upper)
# if within niche: how many degrees below 95th or 98th percentile?
within<-master.niches %>% filter(within.current =="TRUE") %>% 
  mutate(safety.margin = current - upper)

current<-rbind(within, outside)
current$city_species <- paste(current$city, current$species, sep="_")
current<-c(current$safety.margin[match(master.niches$city_species, current$city_species)])

## 2050 ##
# if above niche: how many degrees above 95th or 98th percentile?
outside<-master.niches %>% filter(within.2050=="FALSE") %>% 
  mutate(safety.margin = n2050 - upper)
# if within niche: how many degrees below 95th or 98th percentile?
within<-master.niches %>% filter(within.2050 =="TRUE") %>% 
  mutate(safety.margin = n2050 - upper)

n2050<-rbind(within, outside)
n2050$city_species <- paste(n2050$city, n2050$species, sep="_")
n2050<-c(n2050$safety.margin[match(master.niches$city_species, n2050$city_species)])


## 2070 ##
# if above niche: how many degrees above 95th or 98th percentile?
outside<-master.niches %>% filter(within.2070=="FALSE") %>% 
  mutate(safety.margin = n2070 - upper)
# if within niche: how many degrees below 95th or 98th percentile?
within<-master.niches %>% filter(within.2070 =="TRUE") %>% 
  mutate(safety.margin = n2070 - upper)

n2070<-rbind(within, outside)
n2070$city_species <- paste(n2070$city, n2070$species, sep="_")
n2070<-c(n2070$safety.margin[match(master.niches$city_species, n2070$city_species)])

# bind the different times together
master.niches<-cbind(master.niches, safety.margin.current =current,
                     safety.margin.2050 =n2050,
                     safety.margin.2070 =n2070)


# ----------------------- add final information on -------------------------------------
# calculate carbon per species per city (DIVIDE BY 1000 TO GET TONNES)
tidy.2<-tidy.subset%>% group_by(city.name,new.species) %>%
  summarise(carbon= sum(carbon)/1000)
tidy.2$city_species<-paste(tidy.2$city.name, tidy.2$new.species, sep ="_")

# match to niche dataset
carbon<- c(tidy.2$carbon[match (master.niches$city_species, tidy.2$city_species)])
master.niches<-cbind(master.niches, carbon = carbon)

master.niches<-master.niches%>%arrange(city_species)

# add koppen zones for each city
koppen<-c(city.data$Koppen.new[match(master.niches$city, city.data$City)])
master.niches<-cbind(master.niches, koppen=koppen)

## turn continuous 'safety margins' (degrees above, within) into categories for plotting

master.niches$safety.margin.current.cat<-cut(master.niches$safety.margin.current, breaks = c(-20, -0.5, 0, 0.51, 1.51, 2.51, 3.51, 5, 16),
                                             labels = c("-0.51 to -18°C", "0 to -0.5°C", "0 to 0.5°C", "0.51 to 1.5°C", "1.51 to 2.5°C", "2.51 to 3.5°C", "3.51 to 5°C", "5 to 15°C"))

master.niches$safety.margin.2050.cat<-cut(master.niches$safety.margin.2050, breaks = c(-20, -0.5, 0, 0.51, 1.51, 2.51, 3.51, 5, 16),
                                          labels = c("-0.51 to -18°C", "0 to -0.5°C", "0 to 0.5°C", "0.51 to 1.5°C", "1.51 to 2.5°C", "2.51 to 3.5°C", "3.51 to 5°C", "5 to 15°C"))

master.niches$safety.margin.2070.cat<-cut(master.niches$safety.margin.2070, breaks = c(-20, -0.5, 0, 0.51, 1.51, 2.51, 3.51, 5, 16),
                                          labels = c("-0.51 to -18°C", "0 to -0.5°C", "0 to 0.5°C", "0.51 to 1.5°C", "1.51 to 2.5°C", "2.51 to 3.5°C", "3.51 to 5°C", "5 to 15°C"))

# calculate total carbon for each koppen zone
total.carbon<-city.data%>% group_by(Koppen.new)%>% summarise(total.carbon =sum(Total.carbon.tonnes))

# match to master niches dataset 
total.carbon<- c(total.carbon$total.carbon[match (master.niches$koppen, total.carbon$Koppen.new)])

# add this matched column to master niches and calculate percentage of total
master.niches<-master.niches%>% mutate(koppen.carbon = total.carbon)%>%
  mutate(percentage.carbon = carbon/koppen.carbon*100)


# save
write.csv(master.niches, "Outputs/Output step5/master.niches.temp.2_98.csv")



 #############################################################################################################################

# creating summary of niche data using master niches created above
master.niches<-read.csv("master.niches.temp.2_98.csv")
master.niches<-read.csv("master.niches.temp.5_95.csv")

# this is another for loop that calculates the amount of carbon in each category 
# of safety margin / gap

# make vector of city names 
cities<-unique(master.niches$city)

# make vector of names of safety margin categories (temperature)
names<-data.frame(names = c("-0.51 to -18°C", "0 to -0.5°C", "0 to 0.5°C", "0.51 to 1.5°C", "1.51 to 2.5°C", "2.51 to 3.5°C", "3.51 to 5°C", "5 to 15°C"))

#-------------------- current --------------------------------------------------------------------------------
for(i in 1:nrow(names)) { name <- names$names[i]

b<-master.niches %>% group_by(city) %>%                         # calculate amount carbon in each category in each city
  filter(safety.margin.current.cat== name) %>%
  summarise(carbon = sum(carbon, na.rm = T)) 
b<- c(b$carbon[match (cities, b$city)])                         # match to city names
b<-as.data.frame(cbind(city= cities, carbon=b,                            
                       safety.margin = name, time= "current"))  # create new dataset
if(i==1) {summary.1<-b} 
else {summary.1<-rbind(summary.1,b)}}
summary.1[is.na(summary.1)] <- 0

#---------------------  2050  --------------------------------------------------------------------------------
for(i in 1:nrow(names)) { name <- names$names[i]

b<-master.niches %>% group_by(city) %>%                                   
  filter(safety.margin.2050.cat== name) %>%
  summarise(carbon = sum(carbon,na.rm = T)) 
b<- c(b$carbon[match (cities, b$city)])                                   
b<-as.data.frame(cbind(city= cities, carbon=b,                            
                       safety.margin = name, time = "2050"))  
if(i==1) {summary.2<-b} 
else {summary.2<-rbind(summary.2,b)}}
summary.2[is.na(summary.2)] <- 0

#---------------------  2070  --------------------------------------------------------------------------------
for(i in 1:nrow(names)) { name <- names$names[i]

b<-master.niches %>% group_by(city) %>%                                  
  filter(safety.margin.2070.cat== name) %>%
  summarise(carbon = sum(carbon, na.rm = T)) 
b<- c(b$carbon[match (cities, b$city)])                                   
b<-as.data.frame(cbind(city= cities, carbon=b,                            
                       safety.margin = name, time="2070"))  
if(i==1) {summary.3<-b} 
else {summary.3<-rbind(summary.3,b)}}
summary.3[is.na(summary.3)] <- 0

#----------------------------------------------------------------------------------------------------

# bind above datasets of different times
summary<-rbind(summary.1, summary.2, summary.3)

# change name of current to baseline
summary$time<-gsub("current", "Baseline", summary$time)

# add total species and carbon for each city so we can work out percentages 
data<-master.niches %>% group_by(city) %>%
  summarise(total.sp = n(), total.carbon = sum(carbon, na.rm=T)) 

# match to summary df
sp<-c(data$total.sp[match (summary$city, data$city)])
carbon<-c(data$total.carbon[match (summary$city, data$city)])

# join these matched vectors onto summary df
summary<-cbind(summary, total.carbon =carbon, total.sp=sp)

# create percentages 
summary$carbon<-as.numeric(summary$carbon)
summary<-summary %>% mutate(percentage.carbon = carbon/total.carbon*100)

# add koppen zones to summary
koppen<-c(city.data$Koppen.new[match(summary$city,city.data$City)])
summary<-cbind(summary, koppen)

# save datasets seperately 
write.csv(summary, "Outputs/Output step5/summary.master.temp.5_95.csv")
write.csv(summary, "Outputs/Output step5/summary.master.temp.2_98.csv")




