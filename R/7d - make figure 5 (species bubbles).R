###############################################################################
############################ 7D. MAKE FIG 5  ##################################
###############################################################################

# Create a figure showing the average safety margin/gap of every species in 
# each koppen climate zone at 2070
# date = June 2021 
# Author = George Lloyd, University of Sheffield

# load necessary packages
library(tidyverse)
library(directlabels)
library(ggrepel)
library(grid)
library(RColorBrewer)
library(reshape2)
library(ggpubr)
library(patchwork)
library(gridExtra)
library(cowplot)
library(viridis)
options(scipen=999)

# read in relevant data 

# data using 5-95 scenario
master.niches.prec<-read.csv("master.niches.prec.5_95.csv")
master.niches.temp<-read.csv("master.niches.temp.5_95.csv")

#data using 2-98 scenario
master.niches.prec<-read.csv("master.niches.prec.2_98.csv")
master.niches.temp<-read.csv("master.niches.temp.2_98.csv")

#----------- FIRST NEED TO DO SOME PREP AND CALCULATIONS WITH ABOVE DATASETS -------------------------------

# add temperature safety margins to precipitation niche df so we have just one dataset to work with instead of 2
match<-c(master.niches.temp$safety.margin.2070[match(master.niches.temp$city_species, master.niches.prec$city_species)])
master.niches.prec<-master.niches.prec %>% mutate(temp.margin.2070 = match)

#--------------------- calculating % sp and carbon in each 'zone'---------------------------------------------------------------------------------------------------#

# first: how many sp and how much carbon per city
city<-master.niches.prec%>% group_by(city)%>%
  summarise(carbon= sum(carbon, na.rm = T), koppen= koppen, sp=unique(species))%>%
  group_by(city)%>%summarise(carbon= carbon, koppen = koppen, sp=(n()))%>%
  group_by(city)%>%summarise(carbon= unique(carbon), koppen = unique(koppen), sp=unique(sp))

# calculating % of species or carbon in each 'category' (e.g. outside both niches, within both etc

## how many sp (change to carbon if necessary) are ONLY AT RISK from temp
temp.risk<-master.niches.prec%>%group_by(city )%>% subset(temp.margin.2070 > 0)%>%subset(safety.margin.2070 < 0)%>%
  summarize(carbon =sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.1<-c(temp.risk$carbon[match(city$city, temp.risk$city)])
carbon.1[is.na(carbon.1)] <- 0
sp.1<-c(temp.risk$sp[match(city$city, temp.risk$city)])

## repeat for how much carbon/sp ARE ONLY AT RISK FROM PREC
prec.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2070 > 0) %>% subset(temp.margin.2070 < 0) %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.2<-c(prec.risk$carbon[match(city$city, prec.risk$city)])
carbon.2[is.na(carbon.2)] <- 0
sp.2<-c(prec.risk$sp[match(city$city, prec.risk$city)])

## how much carbon/sp AT RISK FROM BOTH
both.risk<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2070 > 0) %>% subset(temp.margin.2070 > 0)  %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.3<-c(both.risk$carbon[match(city$city, both.risk$city)])
carbon.3[is.na(carbon.3)] <- 0
sp.3<-c(both.risk$sp[match(city$city, both.risk$city)])

## how much carbon/sp inside thermal and hydraulic niches
both.safe<-master.niches.prec%>%group_by(city)%>%subset(safety.margin.2070 < 0) %>% subset(temp.margin.2070 < 0)  %>%
  summarize(carbon=sum(carbon, na.rm = T), sp=(n()))
# match so all 21 cities there (i.e. even cities with no risk are still included)
carbon.4<-c(both.safe$carbon[match(city$city, both.safe$city)])
carbon.4[is.na(carbon.4)] <- 0
sp.4<-c(both.safe$sp[match(city$city, both.safe$city)])   

#-------------------- COMBINE THESE AND DO SOME MORE CALCULATIONS ----------------------------------

# combine above calculations and add some more info from city dataset
combined<-data.frame(koppen = city$koppen, city= city$city, 
                     total.carbon = city$carbon , total.sp= city$sp,
                     sp.risk.temp =sp.1, carbon.risk.temp =carbon.1, 
                     sp.risk.prec =sp.2, carbon.risk.prec =carbon.2,
                     sp.risk.both =sp.3, carbon.risk.both =carbon.3,
                     sp.safe.both =sp.4, carbon.safe.both =carbon.4)

# calculate percentages using this combined dataset
combined<-combined%>% mutate(
  sp.risk.temp.perc =(sp.risk.temp/total.sp)*100,
  carbon.risk.temp.perc =(carbon.risk.temp/total.carbon)*100,
  sp.risk.prec.perc =(sp.risk.prec/total.sp)*100,
  carbon.risk.prec.perc =(carbon.risk.prec/total.carbon)*100,
  sp.risk.both.perc =(sp.risk.both/total.sp)*100,
  carbon.risk.both.perc =(carbon.risk.both/total.carbon)*100,
  sp.safe.both.perc =(sp.safe.both/total.sp)*100, 
  carbon.safe.both.perc =(carbon.safe.both/total.carbon)*100
)

# calculate mean and sd 
combined.means<-combined%>% group_by(koppen)%>%
  summarise(mean.sp.risk.temp = mean(sp.risk.temp.perc, na.rm=T),
            sd.sp.risk.temp = sd(sp.risk.temp.perc, na.rm=T),
            median.sp.risk.temp = median(sp.risk.temp.perc, na.rm=T),
            mean.carbon.risk.temp = mean(carbon.risk.temp.perc,na.rm=T),
            sd.carbon.risk.temp = sd(carbon.risk.temp.perc,na.rm=T),
            median.carbon.risk.temp = median(carbon.risk.temp.perc,na.rm=T),
            mean.sp.risk.prec = mean(sp.risk.prec.perc, na.rm=T),
            sd.sp.risk.prec = sd(sp.risk.prec.perc, na.rm=T),
            median.sp.risk.prec = median(sp.risk.prec.perc, na.rm=T),
            mean.carbon.risk.prec = mean(carbon.risk.prec.perc, na.rm=T),
            sd.carbon.risk.prec = sd(carbon.risk.prec.perc, na.rm=T),
            median.carbon.risk.prec = median(carbon.risk.prec.perc, na.rm=T),
            mean.sp.risk.both = mean(sp.risk.both,na.rm=T),
            sd.sp.risk.both = sd(sp.risk.both,na.rm=T),
            median.sp.risk.both = median(sp.risk.both,na.rm=T),
            mean.carbon.risk.both = mean(carbon.risk.both.perc,na.rm=T),
            sd.carbon.risk.both = sd(carbon.risk.both.perc,na.rm=T),
            median.carbon.risk.both = median(carbon.risk.both.perc,na.rm=T),
            mean.sp.safe.both = mean(sp.safe.both.perc, na.rm=T),
            sd.sp.safe.both = sd(sp.safe.both.perc, na.rm=T),
            median.sp.safe.both = median(sp.safe.both.perc, na.rm=T),
            mean.carbon.safe.both = mean(carbon.safe.both.perc, na.rm=T),
            sd.carbon.safe.both = sd(carbon.safe.both.perc, na.rm=T),
            median.carbon.safe.both = median(carbon.safe.both.perc, na.rm=T))



# find percentages of totals per zone
combined.sums<-combined%>% group_by(koppen)%>%
  summarise(total.sp =sum(total.carbon),
            total.carbon=sum(total.carbon),
            sum.sp.risk.temp = sum(sp.risk.temp, na.rm=T),
            sum.carbon.risk.temp = sum(carbon.risk.temp,na.rm=T),
            sum.sp.risk.prec = sum(sp.risk.prec, na.rm=T),
            sum.carbon.risk.prec = sum(carbon.risk.prec, na.rm=T),
            sum.sp.risk.both = mean(sp.risk.both,na.rm=T),
            sum.carbon.risk.both = sum(carbon.risk.both,na.rm=T),
            sum.sp.safe.both = sum(sp.safe.both, na.rm=T),
            sum.carbon.safe.both = sum(carbon.safe.both, na.rm=T))

#--------- NOW WE HAVE DATA ON RISK PER CITY BUT NEED TO SEE RISK FOR EACH SPECIES ------------------------------------------------------------------------------------------------

# which species safest/threatened from TEMPERATURE per koppen zone 
Cfb.temp<-master.niches.temp%>%group_by(species)%>%subset(koppen=="Temperate oceanic") %>% 
  summarise(carbon = sum(carbon, na.rm = T),safety.margin.2070=mean(safety.margin.2070, na.rm = T))
Csa.temp<-master.niches.temp%>%group_by(species)%>%subset(koppen=="Warm Mediterranean")%>% 
  summarise(carbon = sum(carbon, na.rm = T),safety.margin.2070=mean(safety.margin.2070, na.rm = T))
Dfb.temp<-master.niches.temp%>%group_by(species)%>%subset(koppen=="Humid continental") %>% 
  summarise(carbon = sum(carbon),safety.margin.2070=mean(safety.margin.2070))
Cfa.temp<-master.niches.temp%>%group_by(species)%>%subset(koppen=="Humid subtropical") %>% 
  summarise(carbon = sum(carbon),safety.margin.2070=mean(safety.margin.2070))
Dsk.temp<-master.niches.temp%>%group_by(species)%>%subset(koppen=="Cold semi-arid") %>% 
  summarise(carbon = sum(carbon),safety.margin.2070=mean(safety.margin.2070))

# which species safest/threatened per koppen zone for precipitation
# note: here we are creating mean safety margin of all cities in each zone
# so even if a species has a mean inside both niches, in one city it may be outside one niche and therefore in the combined means dataset there is carbon in zones which dont show up in this dataset

Cfb.prec<-master.niches.prec%>%group_by(species)%>%subset(koppen=="Temperate oceanic") %>% 
  summarise(carbon = sum(percentage.carbon),safety.margin.2070=mean(safety.margin.2070)) %>% cbind(temp.margin=Cfb.temp$safety.margin.2070)
Csa.prec<-master.niches.prec%>%group_by(species)%>%subset(koppen=="Warm Mediterranean")%>% 
  summarise(carbon = sum(percentage.carbon),safety.margin.2070=mean(safety.margin.2070)) %>% cbind(temp.margin=Csa.temp$safety.margin.2070)
Dfb.prec<-master.niches.prec%>%group_by(species)%>%subset(koppen=="Humid continental") %>% 
  summarise(carbon = sum(percentage.carbon),safety.margin.2070=mean(safety.margin.2070)) %>% cbind(temp.margin=Dfb.temp$safety.margin.2070)
Cfa.prec<-master.niches.prec%>%group_by(species)%>%subset(koppen=="Humid subtropical") %>% 
  summarise(carbon = sum(percentage.carbon),safety.margin.2070=mean(safety.margin.2070)) %>% cbind(temp.margin=Cfa.temp$safety.margin.2070)
Dsk.prec<-master.niches.prec%>%group_by(species)%>%subset(koppen=="Cold semi-arid") %>% 
  summarise(carbon = sum(percentage.carbon),safety.margin.2070=mean(safety.margin.2070)) %>% cbind(temp.margin=Dsk.temp$safety.margin.2070)

          
#--------------- MAKE SCATTERPLOTS SHOWING EACH SP THREAT PER KOPPEN ZONE --------------------------------------------------------------------------------------------------------------------------------------------------

## Temperate oceanic climate
Cfb.prec$title <- "Temperate oceanic climate"

# 5-95 plot
a<-ggplot(Cfb.prec, aes(x=temp.margin, y=safety.margin.2070))+
  geom_point(alpha =0.6, aes(size = carbon))+ 
  scale_size_continuous(breaks = c(0.000002, 0.1, 1,5,10,15), range = c(0.75, 20) , name = "Percentage of
total carbon",
                        labels = c("0.001", "0.1", "1", "5","10","15"))+
  facet_grid(.~title)+
  labs(y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)")+ scale_x_continuous(limits = c(-11, 9), expand = c(0, 0), breaks = c(-10, -5, 0, 5, 10), labels = c("10 margin", "5 margin", "0", "5 gap", "10 gap")) +
  scale_y_continuous(limits = c(-46,20), expand = c(0, 0), breaks = c( -40, -20, 0, 20), labels = c("40 margin", "20 margin", "0", "20 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-11), xmax=c(0), ymin=c(-46) , ymax=c(0), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(-11), xmax=c(0), ymin=c(0) , ymax=c(20), alpha=0.5, fill="orange")+  # make coloured rectangles
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(-46) , ymax=c(0), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(0) , ymax=c(20), alpha=0.6, fill="red")+
  theme(legend.text = element_text(size = 13),legend.title = element_text(size=13),
        legend.margin=margin(0, 0, 0, -4), axis.text.y = element_text(size = 14), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14), strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 5, y = -19, size=5, label  = "43.17%±39.80")+
  annotate("text", x = -5, y = 10, size=5, label  = "0%±0")+
  annotate("text", x = 5, y = 10, size=5, label   = "0.05%±0.16")+
  annotate("text", x = -5, y = -19, size=5, label = "56.76%±39.74 ") + theme(legend.position = "none") 
a

# 2-98 plot
aa<-ggplot(Cfb.prec, aes(x=temp.margin, y=safety.margin.2070))+
  geom_point(alpha =0.6, aes(size = carbon))+ 
  scale_size_continuous(breaks = c(0.1, 1,5,10), range = c(0.75, 20) , name = "Percentage of
total carbon",labels = c("0.1", "1", "5","10"))+
  facet_grid(.~title)+
  labs(y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)")+ scale_x_continuous(limits = c(-11, 9), expand = c(0, 0), breaks = c(-10, -5, 0, 5), labels = c("10 margin","5 margin", "0", "5 gap")) +
  scale_y_continuous(limits = c(-46,20), expand = c(0, 0), breaks = c(-40, -20, 0, 20), labels = c("40 margin", "20 margin", "0", "20 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-11), xmax=c(0), ymin=c(-46) , ymax=c(0), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(-11), xmax=c(0), ymin=c(0) , ymax=c(20), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(-46) , ymax=c(0), alpha=0.5, fill="orange")+ 
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(0) , ymax=c(20), alpha=0.6, fill="red")+
  theme(legend.text = element_text(size = 13),legend.title = element_text(size=13),
        axis.text.y = element_text(size = 14), legend.margin=margin(0, 0, 0, -4), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14), strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 4, y = -7, size=5, label  = "29.55%±36.26")+ 
  annotate("text", x = -6, y = 8, size=5, label  = "0%±0")+
  annotate("text", x = 4, y = 8, size=5, label  = "0.05%±0.16")+
  annotate("text", x = -6, y = -7, size=5, label  = "70.33±36.21")
aa


## Warm Mediterranean climate
Csa.prec$title <- "Warm Mediterranean climate"

# 5-95 plot
b<-ggplot(Csa.prec, aes(x=temp.margin, y=safety.margin.2070))+
  geom_point(alpha =0.7, aes(size = carbon))+ 
  scale_size_continuous(breaks = c(0.0002, 1, 5, 10, 30,50), range = c(0.75, 20) , name = "Percentage of 
total carbon",
                        labels = c("0.001","1", "5", "10", "30", "50"))+
  facet_grid(. ~ title)+
  labs( y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-9, 9), expand = c(-0, -0), breaks = c(-10, -5, 0, 5, 10), labels = c("10 margin","5 margin", "0", "5 gap", "10 gap")) +
  scale_y_continuous(limits = c(-16,15), expand = c(0, 0), breaks = c(-15, 0, 15), labels = c("15 margin","0", "15 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-9), xmax=c(0), ymin=c(0) , ymax=c(-16), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(-16) , ymax=c(0), alpha=0.5, fill="orange")+  # make coloured rectangles
  annotate("rect", xmin=c(-9), xmax=c(0), ymin=c(0) , ymax=c(15), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(0) , ymax=c(15), alpha=0.6, fill="red")+
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 13), 
        legend.margin=margin(0, 0, 0, -4),axis.text.y = element_text(size = 14), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 6.5, y = -6, size=5, label  = "71.99%±NA")+
  annotate("text", x = -6, y = 13, size=5, label  = "0%±NA")+
  annotate("text", x = 6.5, y = 13, size=5, label = "0.01%±NA")+
  annotate("text", x = -5, y = -6, size=5, label   = "28.01%±NA")+ theme(legend.position = "none") 
b


# 2-98 plot
bb<-ggplot(Csa.prec, aes(x=temp.margin, y=safety.margin.2070))+
  geom_point(alpha =0.7, aes(size = carbon))+ 
  scale_size_continuous(breaks = c(0.1, 1, 5, 10, 15), range = c(0.75, 20) , name = "Percentage of 
total carbon", labels = c("0.1","1", "5", "10", "15"))+
  facet_grid(. ~ title)+
  labs( y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-9, 9), expand = c(-0, -0), breaks = c(-10,-5, 0, 5, 10), labels = c("10 margin", "5 margin", "0", "5 gap", "10 gap")) +
  scale_y_continuous(limits = c(-16, 15), expand = c(0, 0), breaks = c(-15, 0, 15), labels = c("15 margin","0", "15 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+ 
  annotate("rect", xmin=c(-9), xmax=c(0), ymin=c(-16) , ymax=c(0), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(-9), xmax=c(0), ymin=c(0) , ymax=c(15), alpha=0.5, fill="orange")+ 
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(-16) , ymax=c(0), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(9), ymin=c(0) , ymax=c(15), alpha=0.6, fill="red")+
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 13), 
        legend.margin=margin(0, 0, 0, -4),axis.text.y = element_text(size = 14), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = -5, y = 5, size=5, label  = "0%±NA")+ 
  annotate("text", x = 5, y = -5, size=5, label  = "69.71%±NA")+ 
  annotate("text", x = 5, y = 5, size=5, label  = "0.01%±NA")+ 
  annotate("text", x = -5, y = -5, size=5, label  = "30.29%±NA")

bb

## Humid continental climate
Dfb.prec$title <- "Humid continental climate"

# 5-95 plot
c<-ggplot(Dfb.prec, aes(x=temp.margin, y=safety.margin.2070, size =carbon))+
  geom_point(alpha =0.7)+ scale_size(range = c(0.75, 20))+
  theme(plot.title = element_text(hjust = 0.5, size =12, face= "bold"))+
  facet_grid(. ~ title)+
  labs( y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbo
storage (tonnes)")+ scale_x_continuous(limits = c(-10, 11), expand = c(-0, -0), breaks = c(-5, 0, 5),labels = c( "5 margin","0", "5 gap")) +
  scale_y_continuous(limits = c(-37, 15), expand = c(0, 0), breaks = c(-30,-15, 0, 15),labels = c("30 margin", "15 margin", "0", "15 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-10), xmax=c(0), ymin=c(-37) , ymax=c(0), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(0), xmax=c(11), ymin=c(-37) , ymax=c(0), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(-10), xmax=c(0), ymin=c(0) , ymax=c(15), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(11), ymin=c(0) , ymax=c(15), alpha=0.6, fill="red")+ # make coloured rectangles
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 13), 
        legend.margin=margin(0, 0, 0, -4),axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14), strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 7, y = -12, size=5, label  = "54.89%±40.47")+
  annotate("text", x = -4.5, y = 5, size=5, label  = "0%±0")+
  annotate("text", x = 7, y = 5, size=5, label   = "0.17%±0.33")+
  annotate("text", x = -4, y = -12, size=5, label = "44.94%±40.42")+
  theme(legend.key.size = unit(0.5,"point"))+ theme(legend.position = "none")   
c

# 2-98 plot
cc<-ggplot(Dfb.prec, aes(x=temp.margin, y=safety.margin.2070, size =carbon))+
  geom_point(alpha =0.7)+ scale_size_continuous(breaks = c(0.1, 1, 5, 10, 15), range = c(0.75, 20) , name = "Percentage of 
total carbon", labels = c("0.1","1", "5", "10", "15"))+
  theme(plot.title = element_text(hjust = 0.5, size =12, face= "bold"))+
  facet_grid(. ~ title)+
  labs( y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-10, 11), expand = c(-0, -0), breaks = c( -5, 0, 5),labels = c("5 margin",  "0",  "5 margin")) +
  scale_y_continuous(limits = c(-37, 15), expand = c(0, 0), breaks = c(-30,-15, 0, 15),labels = c("30 margin","15 margin", "0", "15 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-10), xmax=c(0), ymin=c(-37) , ymax=c(0), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(0), xmax=c(11), ymin=c(-37) , ymax=c(0), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(-10), xmax=c(0), ymin=c(0) , ymax=c(15), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(11), ymin=c(0) , ymax=c(15), alpha=0.6, fill="red")+
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 13), 
        legend.margin=margin(0, 0, 0, -4),axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14), strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 5, y = -17, size=5, label  = "44.01%±30.47")+
  annotate("text", x =-5, y = 5, size=5, label  = "0%±0")+
  annotate("text", x =5, y = 5, size=5, label  = "0.17%±0.33")+
  annotate("text", x =-5, y = -17, size=5, label  = "55.83%±30.47")+
  theme(legend.key.size = unit(0.5,"point"))  
cc

# humid subtropical 
Cfa.prec$title <- "Humid subtropical climate"  

# 5-95 plot
d<-ggplot(Cfa.prec, aes(x=temp.margin, y=safety.margin.2070, size =carbon))+
  geom_point(alpha =0.7)+ scale_size(range = c(0.75, 20))+
  facet_grid(.~title)+
  labs(y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-8, 15), expand = c(-0, -0), breaks = c(-5,0,5,10),labels = c("5 margin", "0", "5 gap", "10 gap")) +
  scale_y_continuous(limits = c(-39,31), expand = c(0, 0), breaks = c(-30,0,30),labels = c("30 margin","0", "30 gap"))+ theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-8), xmax=c(0), ymin=c(0) , ymax=c(-39), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(0), xmax=c(15), ymin=c(-39) , ymax=c(0), alpha=0.5, fill="orange")+  # make coloured rectangles
  annotate("rect", xmin=c(-8), xmax=c(0), ymin=c(0) , ymax=c(31), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(15), ymin=c(0) , ymax=c(31), alpha=0.6, fill="red")+
  theme(legend.text = element_text(size = 13),legend.title = element_text(size = 13),
        plot.margin = unit( c(1,0,0.8,0), "in"), legend.margin=margin(0, 0, 0, -4),
        strip.text.x = element_text(size=18), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.y = element_text(size=14), axis.text.x = element_text(size = 14))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 11, y = -13, size=5, label  = "92.15%±8.94")+
  annotate("text", x = -4, y = 18, size=5, label  = "0%±0")+
  annotate("text", x = 11, y = 18, size=5, label   = "0.46%±0.61")+
  annotate("text", x = -4, y = -13, size=5, label = "7.39%±8.34") +
  theme(legend.key.size = unit(0.5,"point"))+ theme(legend.position = "none")    
d

# 2-98 plot
dd<-ggplot(Cfa.prec, aes(x=temp.margin, y=safety.margin.2070, size =carbon))+
  geom_point(alpha =0.7)+
  scale_size_continuous(breaks = c(0.1, 1, 5, 10, 15), range = c(0.75, 20) , name = "Percentage of 
total carbon", labels = c("0.1","1", "5", "10", "15"))+
  facet_grid(.~title)+
  labs(y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-8, 15), expand = c(-0, -0), breaks = c(-5,0,5,10),labels = c("5 margin", "0","5 gap", "10 gap")) +
  scale_y_continuous(limits = c(-39, 31), expand = c(0, 0), breaks = c(-30,0,30),labels = c("30 margin",  "0", "30 gap"))+ theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-8), xmax=c(0), ymin=c(-39) , ymax=c(0), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(-8), xmax=c(0), ymin=c(0) , ymax=c(31), alpha=0.5, fill="orange")+  # make coloured rectangles
  annotate("rect", xmin=c(0), xmax=c(15), ymin=c(-39) , ymax=c(0), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(0), xmax=c(15), ymin=c(0) , ymax=c(31), alpha=0.6, fill="red")+  
  theme(legend.text = element_text(size = 13),legend.title = element_text(size = 13),
        plot.margin = unit( c(1,0,0.8,0), "in"), legend.margin=margin(0, 0, 0, -4),
        strip.text.x = element_text(size=18), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.y = element_text(size=14), axis.text.x = element_text(size = 14))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 8, y = -10, size=5, label  = "82.52%±20.59")+
  annotate("text", x = -4, y = 5, size=5, label  = "0%±0")+
  annotate("text", x = 8, y = 5, size=5, label  = "0.45%±0.61")+
  annotate("text", x = -3.5, y = -10, size=5, label  = "17.02%±20.00")+
  theme(legend.key.size = unit(0.5,"point"))  
dd

# cold semi-arid
Dsk.prec$title <- "Cold semi-arid climate"

# 5-95 plot
e<-ggplot(Dsk.prec, aes(x=temp.margin, y=safety.margin.2070, size =carbon))+
  geom_point(alpha =0.7)+ scale_size(range = c(0.75, 20))+
  theme(plot.title = element_text(hjust = 0.5, size =12, face= "bold"))+
  facet_grid(.~title)+
  labs( y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-4, 16), expand = c(-0, -0), breaks = c(-5,0,5,10,15),labels = c("5 margin", "0","5 gap", "10 gap", "15 gap")) +
  scale_y_continuous(limits = c(-12, 21), expand = c(0, 0), breaks = c(-10,0,10, 20),labels = c("10 margin", "0", "10 gap", "20 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-4), xmax=c(0), ymin=c(0) , ymax=c(-12), alpha=0.5, fill="#66CC00")+
  annotate("rect", xmin=c(0), xmax=c(16), ymin=c(-12) , ymax=c(0), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(-4), xmax=c(0), ymin=c(0) , ymax=c(21), alpha=0.5, fill="orange")+
  annotate("rect", xmin=c(0), xmax=c(16), ymin=c(0) , ymax=c(21), alpha=0.6, fill="red")+ # make coloured rectangles
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 13), 
        legend.margin=margin(0, 0, 0, -4),axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14), strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 13, y = -9.7, size=5, label  = "97.58%±2.93")+
  annotate("text", x = -2, y = 19, size=5, label  = "0%±0")+
  annotate("text", x = 13, y = 19, size=5, label   = "2.40%±2.94")+
  annotate("text", x = -1.5, y = -9.7, size=5, label = "0.02%±0.02") +
  theme(legend.key.size = unit(0.5,"point"))+ theme(legend.position = "none")    
e

# 2-98 plot
ee<-ggplot(Dsk.prec, aes(x=temp.margin, y=safety.margin.2070, size =carbon))+
  geom_point(alpha =0.7) + 
  scale_size_continuous(breaks = c(0.1, 1, 5, 10, 15), range = c(0.75, 20) , name = "Percentage of 
total carbon", labels = c("0.1","1", "5", "10", "15"))+
  theme(plot.title = element_text(hjust = 0.5, size =12, face= "bold"))+
  facet_grid(.~title)+
  labs( y="Hydraulic safety 2070 (mm)", x= "Thermal safety 2070 (°C)", colour="Thermal safety 
margin 2070 (°C)", size = "Current carbon
storage (tonnes)")+ scale_x_continuous(limits = c(-4, 16), expand = c(-0, -0), breaks = c(-5, 0, 5, 10, 15),labels = c("5 margin", "0","5 gap", "10 gap", "15 gap")) +
  scale_y_continuous(limits = c(-12, 21), expand = c(0, 0), breaks = c(-10,0,10, 20),labels = c("10 margin", "0", "10 gap", "20 gap"))+theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size =18, face= "bold"))+
  theme(text = element_text(size = 15))+
  annotate("rect", xmin=c(-4), xmax=c(0), ymin=c(-12) , ymax=c(0), alpha=0.5, fill="#66CC00")+  
  annotate("rect", xmin=c(-4), xmax=c(0), ymin=c(0) , ymax=c(21), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(0), xmax=c(16), ymin=c(-12) , ymax=c(0), alpha=0.5, fill="orange")+  
  annotate("rect", xmin=c(0), xmax=c(16), ymin=c(0) , ymax=c(21), alpha=0.6, fill="red")+  
  theme(legend.text = element_text(size = 13), legend.title = element_text(size = 13), 
        legend.margin=margin(0, 0, 0, -4),axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14), strip.text.x = element_text(size=18))+
  theme(plot.margin=grid::unit(c(2,2,2,2), "mm"))+
  annotate("text", x = 12.5, y = -10, size=5, label  = "99.21%±0.92")+
  annotate("text", x = -2, y = 5, size=5, label  = "0%±0")+
  annotate("text", x = 12.5, y = 5, size=5, label  = "0.75%±0.90")+
  annotate("text", x = -0.8, y = -10, size=5, label  = "0.02%±0.01")+
  theme(legend.key.size = unit(0.5,"point"))
ee

# combine these 5 figures and print
label1 <- grobTree(textGrob("Thermal safety margin/gap 2070 (°C)", x=0.32,  y=0, hjust=0, vjust= 1.3,
                          gp=gpar(fontsize=18)))

label2 <- grobTree(textGrob("Hydraulic safety margin/gap 2070 (mm)", x=0,  y=0.5, vjust= -1, rot=90,
                          gp=gpar(fontsize=18)))

label3 <- grobTree(textGrob("5th - 95th percentile", x=0.27,  y=1, vjust= -0.3,
                           gp=gpar(fontsize=18)))

label4 <- grobTree(textGrob("2nd - 98th percentile", x=0.74,  y=1, vjust= -0.3,
                            gp=gpar(fontsize=18)))

label5 <- grobTree(textGrob("Niche breadth", x=0.5,  y=1, vjust= -1.08,
                            gp=gpar(fontsize=18)))
# using ggarrange

all<-ggarrange(a, aa, b, bb, c, cc, d, dd, e, ee, ncol = 2, nrow = 5, widths = c(1, 1.2))+
  annotation_custom(label1)+
  annotation_custom(label2)+
  annotation_custom(label3)+
  annotation_custom(label4)+
  annotation_custom(label5)+
  theme(plot.margin = unit(c(1,0,1,1), "cm")) 


# export
png(filename="plot.png", 
    type="cairo-png",
    units="px", 
    width=5700, 
    height=6500, 
    pointsize=12, 
    res=450)
print(all)
dev.off()



