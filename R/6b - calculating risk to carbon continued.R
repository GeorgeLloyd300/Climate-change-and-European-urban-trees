###############################################################################
############## 6b. CALCULATING RISK TO CARBON CONTINUED #######################
###############################################################################

# Various calculations relating to riks to current carbon, continued
# date = 07/07/2023 
# Author = George Lloyd, University of Sheffield

# load necessary packages
library(tidyverse)
options(scipen = 999)

# this script calculates percentage and SE of carbon in different safety margins / gaps
# in different climate zones and different timepoints

# read in datasets with differing percentiles
summary.temp<-read.csv("Outputs/Output step 5/summary.master.temp.5_95.csv")
summary.prec<-read.csv("Outputs/Output step 5/summary.master.prec.5_95.csv")

# read in datasets with differing percentiles
summary.temp<-read.csv("Outputs/Output step 5/summary.master.temp.2_98.csv")
summary.prec<-read.csv("Outputs/Output step 5/summary.master.prec.2_98.csv")


############# HDYRAULIC NICHES - PRECIPITATION #########################################################################

#replace na's with 0's so SE calculated correctly
summary.prec[is.na(summary.prec)] <- 0

#filter to get one climate zone
df<-summary.prec%>% filter(koppen == "Warm Mediterranean  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
a<-df%>% group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T), 
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon,na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Warm Mediterranean(n=3)")

#filter to get one climate zone
df<-summary.prec%>% filter(koppen == "Temperate oceanic  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
b<-df%>% group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Temperate oceanic(n=14)")

#filter to get one climate zone
df<-summary.prec%>% filter(koppen == "Humid subtropical  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
c<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Humid subtropical (n=4)")

#filter to get one climate zone
df<-summary.prec%>% filter(koppen == "Humid continental  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
d<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = TRUE),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Humid continental (n=1)")

# join datasets
df.all.prec<-rbind(a,b,c,d)

write.csv(df.all.prec, "df.all.prec.2.csv")




############# THERMAL NICHES - TEMPERATURE #########################################################################

#replace na's with 0's so SE calculated correctly
summary.temp[is.na(summary.temp)] <- 0

#filter to get one climate zone
df<-summary.temp%>% filter(koppen == "Warm Mediterranean  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
a<-df%>% group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T), 
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon,na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Warm Mediterranean(n=3)")

#filter to get one climate zone
df<-summary.temp%>% filter(koppen == "Temperate oceanic  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
b<-df%>% group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Temperate oceanic(n=14)")

#filter to get one climate zone
df<-summary.temp%>% filter(koppen == "Humid subtropical  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
c<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Humid subtropical (n=4)")

#filter to get one climate zone
df<-summary.temp%>% filter(koppen == "Humid continental  ")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
d<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = TRUE),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Humid continental (n=1)")

# join datasets
df.all.temp<-rbind(a,b,c,d)

write.csv(df.all.temp, "df.all.temp.2.csv")



###############################################################################################################
# calculate current, 2050, 2070 carbon at risk 

# at threshold 0-100 (min - max)
df.all.prec<-read.csv("df.all.prec.0_100.csv")
df.all.temp<-read.csv("df.all.temp.0_100.csv")

temp.risk<-df.all.temp%>% 
  subset(safety.margin!= "-0.51 to -18°C")%>%
  subset(safety.margin!= "0 to -0.5°C")%>%
  subset(safety.margin!= "0 to 0.5°C")%>%group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))

prec.risk<-df.all.prec%>% 
  subset(safety.margin!= "-10.1 to -66mm")%>%
  subset(safety.margin!= "0 to -10mm")%>%
  subset(safety.margin!= "0 to 10mm")%>%group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))

# at threshold 5-95th 
df.all.prec<-read.csv("df.all.prec.csv")
df.all.temp<-read.csv("df.all.temp.csv")

temp.risk.2<-df.all.temp%>% 
  subset(safety.margin!= "-0.51 to -18°C")%>%
  subset(safety.margin!= "0 to -0.5°C")%>%
  group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))

prec.risk.2<-df.all.prec%>% 
  subset(safety.margin!= "-10.1 to -66mm")%>%
  subset(safety.margin!= "0 to -10mm")%>%
  group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))

#combine
all<-rbind(prec.risk, prec.risk.2, temp.risk, temp.risk.2)

#save
write.csv(all, "Outputs/Output step 6/sensitivity_analysis.csv")




