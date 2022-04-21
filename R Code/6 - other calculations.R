# Calculations using master dataset
# date = March - June 2021 
# Author = George Lloyd, University of Sheffield

# load necessary packages
library(tidyverse)
options(scipen = 999)

## read in data 

# dataset with information about each city
city<-read.csv("22.city.data.csv")
# master dataset 
tidy.subset<-read.csv("tidy.subset.csv")
# datasets containing each species niche using 2-98th percentiles 
master.niches<-read.csv("master.niches.prec.2_98.csv")
master.niches<-read.csv("master.niches.temp.2_98.csv")
# datasets containing each species niche using 5-95th percentiles 
master.niches<-read.csv("master.niches.prec.csv")
master.niches<-read.csv("master.niches.temp.csv")


#-----------------------------------------------------------------------------------------------
# Calculate mean carbon storage per tree per climate zone 
means<-tidy.subset%>%group_by(koppen)%>%
  summarise(mean = mean(carbon), se = sd(carbon)/sqrt(length(carbon)), 
            total.carbon = sum(carbon)*0.001)

# Calculate Shannon Diversity Index per climate zone 
means<-city%>%group_by(Koppen.new)%>%
  summarise(mean = mean(Species.richness..Shannon.), 
            se = sd(Species.richness..Shannon.)/sqrt(length(Species.richness..Shannon.)))

#----------------------------------------------------------------------------------------------------
# Calculate mean (± one standard error) change in max temp hottest month (baseline to 2070)
new<-city%>% 
  mutate(temp.percent= (temp.current-temp.2070)/temp.current*100, 
         prec.percent= (prec.current-prec.2070)/prec.current*100,
         temp.change = temp.2070-temp.current,
         prec.change = prec.2070-prec.current) %>%
  group_by(Koppen.new)%>% 
  summarise(mean(temp.change), mean(prec.change), 
            mean(temp.percent), mean(prec.percent),
            sd.temp=sd(temp.change)/sqrt(length(temp.change)),
            sd.prec=sd(prec.change)/sqrt(length(prec.change)),
            sample.size =length(City))
# save data
write.csv(new, "climate.change.per.koppen.csv")

#-----------------------------------------------------------------------------------------------------
# Calculating the risk of future temp and prec changes to current stored carbon
# (% and SE of carbon at risk at each time)

# at threshold 2-98
df.all.prec<-read.csv("df.all.prec.2_98.csv")
df.all.temp<-read.csv("df.all.temp.2_98.csv")
# at threshold 5-95 
df.all.prec<-read.csv("df.all.prec.csv")
df.all.temp<-read.csv("df.all.temp.csv")

# risk of temperature
temp.risk<-df.all.temp%>% 
  subset(safety.margin!= "-0.51 to -18°C")%>%
  subset(safety.margin!= "0 to -0.5°C")%>%
  group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))

# risk of precipitation
prec.risk<-df.all.prec%>% 
  subset(safety.margin!= "-10.1 to -66mm")%>%
  subset(safety.margin!= "0 to -10mm")%>%
  group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))


#----------------------------------------------------------------------------------------------------------
# T-test - difference between % at risk carbon between baseline and 2070 

# read in summary temp or prec dataset 
summary<-read.csv("summary.master.prec.csv")      # 5-95th percentile prec
summary<-read.csv("summary.master.temp.csv")      # 5-95th percentile temp
summary<-read.csv("summary.master.prec.2_98.csv") # 2-98th percentile prec
summary<-read.csv("summary.master.temp.2_98.csv") # 2-98th percentile temp

# calculate % carbon at risk at each time in each city and koppen climate zone

# temperature
df<-summary%>% subset(!safety.margin== "-0.51 to -18°C") %>% 
  subset(!safety.margin=="0 to -0.5°C")%>%
  group_by(city, time, koppen)%>% 
  summarise(sum= sum(percentage.carbon, na.rm=T),
  se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)))

# precipitation
df<-summary%>% subset(!safety.margin== "-10.1 to -66mm") %>% 
  subset(!safety.margin=="0 to -10mm")%>%
  group_by(city, time, koppen)%>% 
  summarise(sum= sum(percentage.carbon, na.rm=T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)))


# choose climate zone
climate<-as.data.frame(filter(df, koppen == "Cold semi-arid"))

# run paired t-test on that zone
climate%>%
  pairwise_t_test(
    sum ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

#-------------------------------------------------------------------------------------------------------------
# Calculate the highest carbon storing species in each category of figure 5 (for use in table 1)

# add temp safety margins to prec niche df 
match<-c(master.niches.temp$safety.margin.2070[match(master.niches.temp$city_species, master.niches.prec$city_species)])
master.niches.prec<-master.niches.prec%>% mutate(temp.margin.2070 = match)

# then select climate zone and change symbols (> is gap < is margin)
df<-master.niches.prec%>% filter(koppen == "Cold semi-arid")%>%
  subset(temp.margin.2070 < 0)%>%subset(safety.margin.2070 < 0)%>%
  group_by(species)%>% summarise(sum = sum(percentage.carbon))

# then just look at percentage.carbon column to see how much carbon in the highest storing species 
# and note this down for use in table 1 



