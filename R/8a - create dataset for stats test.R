# 8a - create dataset for stats test.R

# load necessary packages
library(tidyverse)
library(rstatix)
options(scipen=999)

# this code needs to be repeated 6 times (current, 2050 and 2070 and then repeat for 5-95 and 2-98)

## read in relevant data 

koppen<-read.csv("22.city.data.csv")
koppen<-koppen%>% select("Koppen.new")

# data using 5-95 scenario
master.niches.prec<-read.csv("master.niches.prec.5_95.csv")
master.niches.temp<-read.csv("master.niches.temp.5_95.csv")

#data using 2-98 scenario
master.niches.prec<-read.csv("master.niches.prec.2_98.csv")
master.niches.temp<-read.csv("master.niches.temp.2_98.csv")



#ignore
CITY_CARBON <- cbind(CITY_CARBON,
                     events=rep(c(100),times=44))

#---- current ----
#FIRST NEED TO DO SOME PREP AND CALCULATIONS WITH ABOVE DATASETS

# add temperature safety margins to precipitation niche df so we have just one dataset to work with instead of 2
match<-c(master.niches.temp$safety.margin.current[match(master.niches.temp$city_species, master.niches.prec$city_species)])
master.niches.prec<-master.niches.prec %>% mutate(temp.margin.current = match)


#----- -calculating % sp and carbon in each 'zone'---------------------------------------------------------------------------------------------------#

# first: how much carbon per city
city<-master.niches.prec%>% group_by(city)%>%
  summarise(carbon= sum(carbon, na.rm = T), koppen= koppen, sp=unique(species))%>%
  group_by(city)%>%summarise(carbon= carbon, koppen = koppen, sp=(n()))%>%
  group_by(city)%>%summarise(carbon= unique(carbon), koppen = unique(koppen), sp=unique(sp))

# calculating % carbon in each 'category' (e.g. outside both niches, within both etc

## how much carbon ONLY AT RISK from temp
temp.risk<-master.niches.prec%>%group_by(city )%>% subset(temp.margin.current > 0)%>%subset(safety.margin.current < 0)%>%
  summarize(carbon =sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.1<-c(temp.risk$carbon[match(city$city, temp.risk$city)])
carbon.1[is.na(carbon.1)] <- 0
sp.1<-c(temp.risk$sp[match(city$city, temp.risk$city)])

## how much carbon ONLY AT RISK FROM PREC
prec.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.current > 0) %>% subset(temp.margin.current < 0) %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.2<-c(prec.risk$carbon[match(city$city, prec.risk$city)])
carbon.2[is.na(carbon.2)] <- 0
sp.2<-c(prec.risk$sp[match(city$city, prec.risk$city)])

## how much carbon RISK FROM BOTH
both.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.current > 0) %>% subset(temp.margin.current > 0)  %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))

# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.3<-c(both.risk$carbon[match(city$city, both.risk$city)])
carbon.3[is.na(carbon.3)] <- 0
sp.3<-c(both.risk$sp[match(city$city, both.risk$city)])

#---------------------COMBINE THESE AND DO SOME MORE CALCULATIONS 

# combine above calculations and add some more info from city dataset
combined<-data.frame(koppen = city$koppen, city= city$city, 
                     total.carbon = city$carbon , total.sp= city$sp,
                     sp.risk.temp =sp.1, carbon.risk.temp =carbon.1, 
                     sp.risk.prec =sp.2, carbon.risk.prec =carbon.2,
                     sp.risk.both =sp.3, carbon.risk.both =carbon.3)

# calculate percentages using this combined dataset
combined<-combined%>% mutate(
  carbon.risk.temp.perc =(carbon.risk.temp/total.carbon)*100,
  carbon.risk.prec.perc =(carbon.risk.prec/total.carbon)*100,
  carbon.risk.both.perc =(carbon.risk.both/total.carbon)*100,
  threat.all = carbon.risk.temp.perc+carbon.risk.prec.perc+carbon.risk.both.perc)

# calculate mean 
combined.means<-combined%>% group_by(city)%>%
  summarise(mean.carbon.risk.temp = mean(carbon.risk.temp.perc,na.rm=T),
            mean.carbon.risk.prec = mean(carbon.risk.prec.perc, na.rm=T),
            mean.carbon.risk.both = mean(carbon.risk.both.perc,na.rm=T))

combined.means.current<-combined.means %>% mutate(threat.all = 
          mean.carbon.risk.temp+mean.carbon.risk.prec+mean.carbon.risk.both,
          koppen = koppen$Koppen.new, time.point=rep(c("current"),times=22))







#---- 2050 ----
#----------- FIRST NEED TO DO SOME PREP AND CALCULATIONS WITH ABOVE DATASETS

# add temperature safety margins to precipitation niche df so we have just one dataset to work with instead of 2
match<-c(master.niches.temp$safety.margin.2050[match(master.niches.temp$city_species, master.niches.prec$city_species)])
master.niches.prec<-master.niches.prec %>% mutate(temp.margin.2050 = match)


#----- -calculating % sp and carbon in each 'zone'---------------------------------------------------------------------------------------------------#

# first: how much carbon per city
city<-master.niches.prec%>% group_by(city)%>%
  summarise(carbon= sum(carbon, na.rm = T), koppen= koppen, sp=unique(species))%>%
  group_by(city)%>%summarise(carbon= carbon, koppen = koppen, sp=(n()))%>%
  group_by(city)%>%summarise(carbon= unique(carbon), koppen = unique(koppen), sp=unique(sp))

# calculating % carbon in each 'category' (e.g. outside both niches, within both etc

## how much carbon ONLY AT RISK from temp
temp.risk<-master.niches.prec%>%group_by(city )%>% subset(temp.margin.2050 > 0)%>%subset(safety.margin.2050 < 0)%>%
  summarize(carbon =sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.1<-c(temp.risk$carbon[match(city$city, temp.risk$city)])
carbon.1[is.na(carbon.1)] <- 0
sp.1<-c(temp.risk$sp[match(city$city, temp.risk$city)])

## how much carbon ONLY AT RISK FROM PREC
prec.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2050 > 0) %>% subset(temp.margin.2050 < 0) %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.2<-c(prec.risk$carbon[match(city$city, prec.risk$city)])
carbon.2[is.na(carbon.2)] <- 0
sp.2<-c(prec.risk$sp[match(city$city, prec.risk$city)])

## how much carbon RISK FROM BOTH
both.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2050 > 0) %>% subset(temp.margin.2050 > 0)  %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.3<-c(both.risk$carbon[match(city$city, both.risk$city)])
carbon.3[is.na(carbon.3)] <- 0
sp.3<-c(both.risk$sp[match(city$city, both.risk$city)])

#-------------------- COMBINE THESE AND DO SOME MORE CALCULATIONS 

# combine above calculations and add some more info from city dataset
combined<-data.frame(koppen = city$koppen, city= city$city, 
                     total.carbon = city$carbon , total.sp= city$sp,
                     sp.risk.temp =sp.1, carbon.risk.temp =carbon.1, 
                     sp.risk.prec =sp.2, carbon.risk.prec =carbon.2,
                     sp.risk.both =sp.3, carbon.risk.both =carbon.3)

# calculate percentages using this combined dataset
combined<-combined%>% mutate(
  carbon.risk.temp.perc =(carbon.risk.temp/total.carbon)*100,
  carbon.risk.prec.perc =(carbon.risk.prec/total.carbon)*100,
  carbon.risk.both.perc =(carbon.risk.both/total.carbon)*100,
  threat.all = carbon.risk.temp.perc+carbon.risk.prec.perc+carbon.risk.both.perc)

# calculate mean 
combined.means<-combined%>% group_by(city)%>%
  summarise(mean.carbon.risk.temp = mean(carbon.risk.temp.perc,na.rm=T),
            mean.carbon.risk.prec = mean(carbon.risk.prec.perc, na.rm=T),
            mean.carbon.risk.both = mean(carbon.risk.both.perc,na.rm=T)) 

combined.means.2050<-combined.means %>% mutate(threat.all =
        mean.carbon.risk.temp+mean.carbon.risk.prec+mean.carbon.risk.both, 
        koppen = koppen$Koppen.new, time.point=rep(c("2050"),times=22))



#---- 2070 ----
#----------- FIRST NEED TO DO SOME PREP AND CALCULATIONS WITH ABOVE DATASETS

# add temperature safety margins to precipitation niche df so we have just one dataset to work with instead of 2
match<-c(master.niches.temp$safety.margin.2070[match(master.niches.temp$city_species, master.niches.prec$city_species)])
master.niches.prec<-master.niches.prec %>% mutate(temp.margin.2070 = match)


#----- -calculating % sp and carbon in each 'zone'---------------------------------------------------------------------------------------------------#

# first: how much carbon per city
city<-master.niches.prec%>% group_by(city)%>%
  summarise(carbon= sum(carbon, na.rm = T), koppen= koppen, sp=unique(species))%>%
  group_by(city)%>%summarise(carbon= carbon, koppen = koppen, sp=(n()))%>%
  group_by(city)%>%summarise(carbon= unique(carbon), koppen = unique(koppen), sp=unique(sp))

# calculating % carbon in each 'category' (e.g. outside both niches, within both etc

## how much carbon ONLY AT RISK from temp
temp.risk<-master.niches.prec%>%group_by(city )%>% subset(temp.margin.2070 > 0)%>%subset(safety.margin.2070 < 0)%>%
  summarize(carbon =sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.1<-c(temp.risk$carbon[match(city$city, temp.risk$city)])
carbon.1[is.na(carbon.1)] <- 0
sp.1<-c(temp.risk$sp[match(city$city, temp.risk$city)])

## how much carbon ONLY AT RISK FROM PREC
prec.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2070 > 0) %>% subset(temp.margin.2070 < 0) %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.2<-c(prec.risk$carbon[match(city$city, prec.risk$city)])
carbon.2[is.na(carbon.2)] <- 0
sp.2<-c(prec.risk$sp[match(city$city, prec.risk$city)])

## how much carbon RISK FROM BOTH
both.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2070 > 0) %>% subset(temp.margin.2070 > 0)  %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.3<-c(both.risk$carbon[match(city$city, both.risk$city)])
carbon.3[is.na(carbon.3)] <- 0
sp.3<-c(both.risk$sp[match(city$city, both.risk$city)])

#-------------------- COMBINE THESE AND DO SOME MORE CALCULATIONS 

# combine above calculations and add some more info from city dataset
combined<-data.frame(koppen = city$koppen, city= city$city, 
                     total.carbon = city$carbon , total.sp= city$sp,
                     sp.risk.temp =sp.1, carbon.risk.temp =carbon.1, 
                     sp.risk.prec =sp.2, carbon.risk.prec =carbon.2,
                     sp.risk.both =sp.3, carbon.risk.both =carbon.3)

# calculate percentages using this combined dataset
combined<-combined%>% mutate(
  carbon.risk.temp.perc =(carbon.risk.temp/total.carbon)*100,
  carbon.risk.prec.perc =(carbon.risk.prec/total.carbon)*100,
  carbon.risk.both.perc =(carbon.risk.both/total.carbon)*100,
  threat.all = carbon.risk.temp.perc+carbon.risk.prec.perc+carbon.risk.both.perc)

# calculate mean 
combined.means<-combined%>% group_by(city)%>%
  summarise(mean.carbon.risk.temp = mean(carbon.risk.temp.perc,na.rm=T),
            mean.carbon.risk.prec = mean(carbon.risk.prec.perc, na.rm=T),
            mean.carbon.risk.both = mean(carbon.risk.both.perc,na.rm=T))

combined.means.2070<-combined.means %>% mutate(threat.all = 
            mean.carbon.risk.temp+mean.carbon.risk.prec+mean.carbon.risk.both, 
            koppen = koppen$Koppen.new, time.point=rep(c("2070"),times=22))


#---- now combine these datasets and make them 'tidy' ----

# combine
combined.all<-rbind(combined.means.current, combined.means.2050, combined.means.2070)

# remove unwanted columns 
combined.all<-combined.all%>% select(-mean.carbon.risk.both, -mean.carbon.risk.prec,
            -mean.carbon.risk.temp) %>% rename_at('threat.all', ~'carbon')

#order df by city
combined.all<- combined.all[order(combined.all$city), ]

# change column order
combined.all<- combined.all[ , c("carbon", "time.point", "city", "koppen")]  

write.csv(combined.all,"threatened carbon dataset_2_98.csv")
