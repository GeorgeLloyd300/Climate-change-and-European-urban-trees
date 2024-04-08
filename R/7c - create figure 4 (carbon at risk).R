###############################################################################
############################ 7c. CREATE FIG 4  ##################################
###############################################################################

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

summary<-read.csv("summary.master.temp.2_98.csv")
summary<-read.csv("summary.master.temp.5_95.csv")

summary<-read.csv("summary.master.prec.2_98.csv")
summary<-read.csv("summary.master.prec.5_95.csv")


# for good quality export
png(filename="plot.png", 
    type="cairo-png",
    units="px", 
    width=5000, 
    height=6500, 
    pointsize=12, 
    res=450)
print(all)
dev.off()

# making a plot of just climate zones rather than cities 

#-------------------------------------------------------------------------------------------------------
# make df.all dataframe of percentages per climate zone instead of per city

# read in different summary dataset (prec or temp, 5-95 or 2-98) then run below code 

#replace na's with 0's so SE calculated correctly
summary[is.na(summary)] <- 0

#filter to get one climate zone
df<-summary%>% filter(koppen == "Warm Mediterranean")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
a<-df%>% group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T), 
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
              mean = mean(percentage.carbon,na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
   mutate(koppen = "Warm Mediterranean(n=1)")

#filter to get one climate zone
df<-summary%>% filter(koppen == "Temperate oceanic")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon, na.rm = T))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
b<-df%>% group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Temperate oceanic(n=12)")

#filter to get one climate zone
df<-summary%>% filter(koppen == "Humid subtropical")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
c<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = T),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Humid subtropical (n=3)")

#filter to get one climate zone
df<-summary%>% filter(koppen == "Humid continental")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
d<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = TRUE),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Humid continental (n=4)")

#filter to get one climate zone
df<-summary%>% filter(koppen == "Cold semi-arid")
# calculate total carbon in that climate
total.climate<-df%>% group_by(city)%>% summarise(total = mean(total.carbon))%>%
  summarise(total=sum(total))
# calculate mean and sd percentage carbon in each safety margin category in each climate:
e<-df%>%group_by(time, safety.margin)%>%
  summarise(sum=sum(carbon, na.rm = TRUE),
            se = sd(percentage.carbon, na.rm=T)/sqrt(length(percentage.carbon)),
            mean = mean(percentage.carbon, na.rm =T)) %>%
  mutate(percentage = sum/total.climate$total*100) %>%
  mutate(koppen = "Cold semi-arid (n=2)")


# join datasets
df.all<-rbind(a,b,c,d,e)

#--------------------------------------------------------------------------------------------------------------
###### make temp plot ######

# temp categories
categories.temp <-c( "5 to 15°C","3.51 to 5°C", "2.51 to 3.5°C", "1.51 to 2.5°C",
                "0.51 to 1.5°C","0 to 0.5°C", "0 to -0.5°C","-0.51 to -18°C")


names.temp <-c( "-5 to -15°C","-3.51 to -5°C", "-2.51 to -3.5°C", "-1.51 to -2.5°C",
                "-0.51 to -1.5°C","0 to -0.5°C", "0 to 0.5°C","0.51 to 18°C")

names.temp <-c( "5 to 15°C gap","3.51 to 5°C gap", "2.51 to 3.5°C gap", "1.51 to 2.5°C gap",
                "0.51 to 1.5°C gap","0 to 0.5°C gap", "0 to 0.5°C margin","0.51 to 18°C margin")

# define colours
mycolors.temp <- c("#990000", "#CC0000", "#FF0000", "#FF3333","#FF6633", "#FF9966", 
                   "#FFFF33", "#33CC00")

# colourblind safe colours
mycolors.temp <- c("black", "gray15", "gray30", "gray45","gray60", "gray75",
                   "#F0E442", "#009E73")                 

# define levels of times 
df.all$time<-factor(df.all$time,levels = c("Baseline","2050","2070"))


# make plot
# use percentage +- se for dodged bars or percentagesum +- se for stacked bars

temp<-df.all%>%
ggplot(aes(x = koppen, y = percentage, fill = factor(safety.margin, levels = categories.temp, labels = names.temp)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = percentage-se, ymax = percentage+se), width = 0.7, position = position_dodge(0.9))+
  facet_grid(.~time) +
        labs(title = "", x ="", y = "Percentage of total stored carbon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size =12), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 14),
        panel.background = element_rect(fill = "white", colour ="white"),
        axis.ticks.x = element_blank())+
  scale_fill_discrete(type= mycolors.temp, name = "
       Species
       thermal
       safety")+
  scale_y_continuous(breaks = seq(0, 100, 20))+
  geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = 100), size =0.5)+ # vertical lines
  geom_segment(aes(x = 1.5, y = 0, xend = 1.5, yend = 100))+
  geom_segment(aes(x = 2.5, y = 0, xend = 2.5, yend = 100))+
  geom_segment(aes(x = 3.5, y = 0, xend = 3.5, yend = 100))+
  geom_segment(aes(x = 4.5, y = 0, xend = 4.5, yend = 100))+
  geom_segment(aes(x = 5.5, y = 0, xend = 5.5, yend = 100))+
  geom_segment(aes(x = 0.5, y = 0, xend = 5.5, yend = 0))+ # horizontal lines
  geom_segment(aes(x = 0.5, y = 100, xend = 5.5, yend = 100))

#-----------------------------------------------------------------------------------------------------------------------------------------------
###### make prec plot ######

# prec categories
categories.prec<-c("30.1 to 40mm","20.1 to 30mm","10.1 to 20mm", "0 to 10mm", "0 to -10mm","-10.1 to -66mm")

# prec names
names.prec<-c("30.1 to 40mm gap","20.1 to 30mm gap","10.1 to 20mm gap", "0 to 10mm gap", "0 to 10mm margin","10.1 to 66mm margin")
  
# define colours
mycolors.prec <- c("#990000", "#FF0000","#FF3333", "#FF9966", "#FFFF33", "#33CC00")  

#colourblind
mycolors.prec<- c("black", "gray15", "gray45", "gray75","#F0E442", "#009E73")

#gray
mycolors.prec<- c("black", "gray29", "gray45", "gray52","gray69", "gray91")

# define levels of times 
df.all$time<-factor(df.all$time,levels = c("Baseline","2050","2070"))

# make plot
prec<-df.all%>%
  ggplot(aes(x = koppen, y = percentage, fill = factor(safety.margin, levels = categories.prec, labels = names.prec)))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = percentage-se, ymax = percentage+se), width = 0.7, position = position_dodge(0.9))+
  facet_grid(.~time) +
  labs(title = "", x ="", y = "Percentage of total stored carbon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size =12), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 14),
        panel.background = element_rect(fill = "white", colour ="white"),
        axis.ticks.x = element_blank())+
  scale_fill_discrete(type= mycolors.prec, name = "
       Species
       hydraulic
       safety")+
  scale_y_continuous(breaks = seq(0, 100, 20))+
  geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = 100), size =0.5)+ # vertical lines
  geom_segment(aes(x = 1.5, y = 0, xend = 1.5, yend = 100))+
  geom_segment(aes(x = 2.5, y = 0, xend = 2.5, yend = 100))+
  geom_segment(aes(x = 3.5, y = 0, xend = 3.5, yend = 100))+
  geom_segment(aes(x = 4.5, y = 0, xend = 4.5, yend = 100))+
  geom_segment(aes(x = 5.5, y = 0, xend = 5.5, yend = 100))+
  geom_segment(aes(x = 0.5, y = 0, xend = 5.5, yend = 0))+ # horizontal lines
  geom_segment(aes(x = 0.5, y = 100, xend = 5.5, yend = 100))



# combine plots
all<-ggarrange(temp, prec, ncol = 1, nrow = 2, labels ="auto", font.label = list(size = 18))

#print
png(filename="plot.png", 
    type="cairo-png",
    units="px", 
    width=5400, 
    height=5500, 
    pointsize=12, 
    res=450)
print(all)
dev.off()


# calculate current, 2050, 2070 carbon at risk 

temp.risk<-df.all%>% 
  subset(safety.margin!= "-0.51 to -18°C")%>%
  subset(safety.margin!= "0 to -0.5°C")%>%
  subset(safety.margin!= "0 to 0.5°C")%>%group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))

prec.risk<-df.all%>% 
  subset(safety.margin!= "-10.1 to -66mm")%>%
  subset(safety.margin!= "0 to -10mm")%>%
  subset(safety.margin!= "0 to 10mm")%>%group_by(koppen, time)%>%
  summarise(sum = sum(percentage), se= sum(se))






