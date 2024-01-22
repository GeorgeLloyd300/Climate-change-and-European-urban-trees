# Calculating each species thermal and hydraulic safety margin/gap
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

## This should be called what it is - EG niche.95
# this data set contains 5-95th percentiles
new.niches <- read.csv("GLOBAL_NICHES_EUROPE_PLANTS.csv")


## This should be called what it is - EG niche.98
# this data set contains 2-98th percentiles
new.niches <- read.csv("EURO_TREES_MAIN_GLOBAL_NICHES_ALL_2.CSV")

# data on each city
city.data  <- read.csv("22.city.data.csv")

# master data set
tidy.subset < -read.csv("tidy.subset.csv")





# ------------------------- 1). create master niches data set -------------------------------------------

# this is a for loop that takes each species city combination at each time and calculates 
# whether or not the city's climate is within the species niche 
# the code should be altered slightly when using the 2 different versions of 'new.niches'
# for example, 'Precip_dry_month_q02' and 'Precip_dry_month_q98' below should be changed to 
# 'Precip_dry_month_q05' and 'Precip_dry_month_q95'


for(i in 1:nrow(city.data)) { 
  name <- city.data$City[i]
  
  # get climate in city
  climate <- (city.data%>% filter(City == name))
  a <- climate$prec.current 
  b <- climate$prec.2050
  c <- climate$prec.2070
  
  # get list of species in this city
  city    <- filter(tidy.subset, city.name == name)
  city.sp <- unique(city$new.species)
  # subset niche data to get just these species niches
  # add new column showing if species is within its niche in future
  # true = cities precipitation is within species niche range (between p05 and p95 or p02 and p98)
  
  city.niches <-
    
    new.niches %>% select(searchTaxon, 
                          Precip_dry_month_q02 , 
                          Precip_dry_month_q98) %>% # this line should be changed when using different threshold (Precip_dry_month_q02 to Precip_dry_month_q05 and Precip_dry_month_q95 to Precip_dry_month_q98) 
    subset(searchTaxon %in% city.sp) %>% 
    mutate(city = name) %>%
    rename(species = searchTaxon, 
           lower   = Precip_dry_month_q02, 
           upper   = Precip_dry_month_q98) %>% # this line should be changed when using different threshold (Precip_dry_month_q02 to Precip_dry_month_q05 and Precip_dry_month_q95 to Precip_dry_month_q98) 
    mutate(current = a) %>% 
    mutate(n2050   = b) %>% mutate(n2070 = c)  %>%
    mutate('within.current'= current >= lower) %>%
    mutate('within.2050'= n2050 >= lower) %>%
    mutate('within.2070'= n2070 >= lower)
  
  if(i == 1) {
    
    master.niches <- city.niches
    
  } else {
    
    master.niches<-rbind(master.niches, city.niches)
    
  }
  
}





# -------------------- 2). how far are species outside their niches? -------------------------------------

# first make column of city and species so we can match
master.niches$city_species <- paste(master.niches$city, master.niches$species, sep="_")

# this below code works by asking : is the city within current niche? If not, then is it 
# above or below lower percentile (i.e. is city precipitation below lower limit of niche (too dry for species))
# Then it calculates how far above or below niche that city is and repeats for each time point

## baseline ##
## if outside niche: how many mm below lower limit of niche (q05 or q02)?
outside <- master.niches %>% filter(within.current == "FALSE") %>% 
  mutate(safety.margin = lower - current)

## if within niche: how many mm above lower limit of niche (q05 or q02)?
within <- master.niches %>% filter(within.current == "TRUE") %>% 
  mutate(safety.margin = lower - current)

current              <- rbind(within, outside)
current$city_species <- paste(current$city, current$species, sep = "_")
current              <- c(current$safety.margin[match(master.niches$city_species, current$city_species)])


## 2050 ##
## if outside niche: how many mm below lower limit of niche (q05 or q02)?
outside<-master.niches %>% filter(within.2050 == "FALSE") %>% 
  mutate(safety.margin = lower - n2050 )


## if within niche: how many mm above lower limit of niche (q05 or q02)?
within<-master.niches %>% filter(within.2050 == "TRUE") %>% 
  mutate(safety.margin = lower - n2050)

n2050              <- rbind(within, outside)
n2050$city_species <- paste(n2050$city, n2050$species, sep="_")
n2050              <- c(n2050$safety.margin[match(master.niches$city_species, n2050$city_species)])


## 2070 ##
## if outside niche: how many mm below lower limit of niche (q05 or q02)?
outside <- master.niches %>% filter(within.2070 == "FALSE") %>% 
  mutate(safety.margin = lower - n2070)


## if within niche: how many mm above lower limit of niche (q05 or q02)?
within<-master.niches %>% filter(within.2070 == "TRUE") %>% 
  mutate(safety.margin = lower- n2070)

n2070              <- rbind(within, outside)
n2070$city_species <- paste(n2070$city, n2070$species, sep="_")
n2070<-c(n2070$safety.margin[match(master.niches$city_species, n2070$city_species)])

## bind the different times together
master.niches<-cbind(master.niches, safety.margin.current =current,
                     safety.margin.2050 = n2050,
                     safety.margin.2070 = n2070)





# ----------------------- 3). add final information on -------------------------------------


## calculate carbon per species per city (DIVIDE BY 1000 TO GET TONNES)
tidy.2 <- tidy.subset %>% group_by(city.name, new.species) %>%
  summarise(carbon = sum(carbon)/1000)


tidy.2$city_species <- paste(tidy.2$city.name, tidy.2$new.species, sep = "_")


## match to niche data set
carbon        <- c(tidy.2$carbon[match (master.niches$city_species, tidy.2$city_species)])
master.niches <- cbind(master.niches, carbon = carbon)

master.niches <- master.niches %>% arrange(city_species)


## add koppen zones for each city
koppen <- c(city.data$Koppen.new[match(master.niches$city, city.data$City)])
master.niches<-cbind(master.niches, koppen = koppen)


## turn continuous 'safety margins' (mm below, within) into categories for plotting
master.niches$safety.margin.current.cat <- cut(master.niches$safety.margin.current, 
                                               
                                               breaks = c(-70, -10.1, 0, 10.1, 20.1, 30.1, 40),
                                               labels = c("-10.1 to -66mm", 
                                                          "0 to -10mm", 
                                                          "0 to 10mm", 
                                                          "10.1 to 20mm", 
                                                          "20.1 to 30mm", 
                                                          "30.1 to 40mm"))

master.niches$safety.margin.2050.cat   <- cut(master.niches$safety.margin.2050, 
                                              
                                              breaks = c(-70, -10.1, 0, 10.1, 20.1, 30.1, 40),
                                              labels = c("-10.1 to -66mm", 
                                                         "0 to -10mm", 
                                                         "0 to 10mm", 
                                                         "10.1 to 20mm", 
                                                         "20.1 to 30mm", 
                                                         "30.1 to 40mm"))

master.niches$safety.margin.2070.cat<-cut(master.niches$safety.margin.2070, 
                                          
                                          breaks = c(-70, -10.1, 0, 10.1, 20.1, 30.1, 40),
                                          labels = c("-10.1 to -66mm", 
                                                     "0 to -10mm", 
                                                     "0 to 10mm", 
                                                     "10.1 to 20mm", 
                                                     "20.1 to 30mm", 
                                                     "30.1 to 40mm"))

## Calculate total carbon for each koppen zone
total.carbon < -city.data %>% group_by(Koppen.new) %>% 
  
  summarise(total.carbon = sum(Total.carbon.tonnes))


## match to master niches dataset 
total.carbon <- c(total.carbon$total.carbon[match (master.niches$koppen, total.carbon$Koppen.new)])


## add this matched column to master niches and calculate percentage of total
master.niches <- master.niches %>% mutate(koppen.carbon = total.carbon) %>%
  mutate(percentage.carbon = carbon/koppen.carbon * 100)


# save
write.csv(master.niches, "master.niches.prec.2_98.csv")



######################################## 4). Summarize Niche data #############################################

## creating summary of niche data using master niches created above
master.niches <- read.csv("master.niches.prec.2_98.csv")

# this is another for loop that calculates the amount of carbon in each category 
# of safety margin 

# make vector of city names 
cities <- unique(master.niches$city)

# make vector of names of safety margin categories (precipitation)
names <- data.frame(names = c("-10.1 to -66mm", 
                              "0 to -10mm", 
                              "0 to 10mm", 
                              "10.1 to 20mm", 
                              "20.1 to 30mm", 
                              "30.1 to 40mm"))




#-------------------- current --------------------------------------------------------------------------------
for(i in 1:nrow(names)) { name <- names$names[i]

b <- master.niches %>% group_by(city) %>%                         # calculate amount carbon in each category in each city
  filter(safety.margin.current.cat== name) %>%
  summarise(carbon = sum(carbon, na.rm=T)) 

b <- c(b$carbon[match (cities, b$city)])                          # match to city names
b <- as.data.frame(cbind(city= cities, carbon=b,                            
                         safety.margin = name, time= "current"))  # create new dataset

if(i==1) {summary.1<-b} 
else {summary.1<-rbind(summary.1,b)}}
summary.1[is.na(summary.1)] <- 0


#---------------------  2050  --------------------------------------------------------------------------------
for(i in 1:nrow(names)) { name <- names$names[i]

b <- master.niches %>% group_by(city)   %>%                                   
  filter(safety.margin.2050.cat== name) %>%
  summarise(carbon = sum(carbon, na.rm=T)) 

b <- c(b$carbon[match (cities, b$city)])                                   
b <- as.data.frame(cbind(city= cities, carbon=b,                            
                         safety.margin = name, time = "2050"))  
if(i==1) {summary.2<-b} 
else {summary.2<-rbind(summary.2,b)}}
summary.2[is.na(summary.2)] <- 0



#---------------------  2070  --------------------------------------------------------------------------------
for(i in 1:nrow(names)) { name <- names$names[i]

b<-master.niches %>% group_by(city) %>%                                  
  filter(safety.margin.2070.cat== name) %>%
  summarise(carbon = sum(carbon, na.rm=T)) 
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

# save datasets
write.csv(summary, "summary.master.prec.5_95.csv")
write.csv(summary, "summary.master.prec.2_98.csv") 





