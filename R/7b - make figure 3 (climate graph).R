###############################################################################
############################ 7B. MAKE FIG 3 KOPPEN MAP #######################
###############################################################################

# Create a figure showing each climate zones temperature and precipitation at 
# baseline, 2050 and 2070
# date = June 2023 
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

# load relevant data
city<-read.csv("Data/Species city and equation data/22.city.data.csv")
climate<-read.csv("city.tidy.climate.2.csv") # data on each cities climate at different times


# make labels 
category.labs<-c("Precipitation of driest month(mm)", "Temperature of hottest month(°C)")
names(category.labs) <- c("prec", "temp")

#---------------------------- HOW THIS WORKS -------------------------------------------------------------------------------------------------------
# first we subset 'climate' dataset to get just one climate zone
# then subset this to get either precipitation (prec) or temperate (temp)
# then plot the temp or prec at each time 
# then use some code to change labels and theme etc

#------------------------- MAKE TEMPERATE OCEANIC FIGURE ------------------------------------------------------------------------------

# make precipitation graph
a<-climate%>%subset(koppen.new =="Temperate oceanic") %>% 
  subset(category=="prec") %>%
  ggplot(aes(x= time, y =value, group= City, colour="blue", shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City)+
  geom_line(size =0.75, show.legend = FALSE, colour ="blue")+
  geom_point(size = 2.5, colour ="black")+
  labs(title ="Temperate oceanic climate", x= "", y ="", shape ="    Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
   ylim(25,67)+
  theme(strip.text.y = element_blank(),strip.text.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size =18), plot.margin = unit( c(0,0.1,0.1,0), "in"),
        legend.margin=margin(-10, 0, 0, -6), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),axis.text.y = element_text(size = 14), 
        legend.title = element_text(size = 17), legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 13))

# make temperature graph
b<-climate%>%subset(koppen.new =="Temperate oceanic") %>% 
  subset(category=="temp") %>%
  ggplot(aes(x= time, y =value, group= City, colour=category, shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City, switch = "x")+
  geom_line(size =0.75, show.legend = FALSE)+
  geom_point(size = 2.5, colour ="black")+
  labs(title ="Temperate oceanic climate", x= "", y ="", shape ="    Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(17,35)+
  theme(strip.text.y = element_blank(),plot.title = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme( plot.margin = unit( c(0,0.1,0.1,0) , "in" ),legend.margin=margin(-10, 0, 0, -6) )+
  theme(strip.text.x = element_text(size = 14, angle =90), 
        strip.background = element_rect(color="white", fill="white", size=3),
        axis.text.y = element_text(size = 14), 
        legend.title = element_text(size = 17), legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 13))

# combine both into one figure 
cfb<-ggdraw() +
  draw_plot(a, x = 0.02, y = .5, width = .97, height = .5) +
  draw_plot(b, x = 0.02, y = 0, width = .97, height = .54)

#------------------------- MAKE HUMID CONTINENTAL FIGURE ------------------------------------------------------------------------------

# make precipitation graph
a<-climate%>%subset(koppen.new =="Humid continental") %>% 
  subset(category=="prec") %>%
  ggplot(aes(x= time, y =value, group= City, colour="blue", shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City)+
  geom_line(size =0.75, show.legend = FALSE, colour ="blue")+
  geom_point(size = 2.5, colour ="black")+
  labs(title ="Humid continental climate", x= "", y ="", shape ="    Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(0,40)+
  theme(strip.text.y = element_blank(),strip.text.x = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme( plot.margin = unit( c(0,0.1,0.1,0), "in"), legend.margin=margin(-10, 0, 0, -6), 
         axis.text.y = element_text(size = 14),plot.title = element_text(hjust = 0.5, size =18), 
         legend.title = element_text(size = 17), legend.text = element_text(size = 14),
         axis.title.y = element_text(size = 13))

# make temperature graph
b<-climate%>%subset(koppen.new =="Humid continental") %>% 
  subset(category=="temp") %>%
  ggplot(aes(x= time, y =value, group= City, colour=category, shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City, switch = "x")+
  geom_line(size =0.75, show.legend = FALSE)+
  geom_point(size = 2.5, colour ="black")+
  labs(title ="Humid continental climate", x= "", y ="", shape ="    Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
   ylim(19,38)+
  theme(strip.text.y = element_blank(), plot.title = element_blank(),axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  theme( plot.margin = unit( c(0,0.1,0.1,0) , "in" ),legend.margin=margin(-10, 0, 0, -6))+
  theme(strip.text.x = element_text(size = 14, angle =0), 
        strip.background = element_rect(color="white", fill="white", size=3),
        axis.text.y = element_text(size = 14), 
        legend.title = element_text(size = 17), legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 13))

# combine both into one figure 
dfb<-ggdraw() +
  draw_plot(a, x = 0.02, y = .5, width = .97, height = .5) +
  draw_plot(b, x = 0.02, y = 0, width = .97, height = .54)


#------------------------- MAKE HUMID SUBTROPICAL FIGURE ------------------------------------------------------------------------------

# make precipitation graph
a<-climate%>%subset(koppen.new =="Humid subtropical") %>% 
  subset(category=="prec") %>%
  ggplot(aes(x= time, y =value, group= City, colour="blue", shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City)+
  geom_line(size =0.75, show.legend = FALSE, colour ="blue")+
  geom_point(size = 2.5, colour ="black",show.legend = FALSE)+
  labs(title ="Humid subtropical climate", x= "", y ="", shape ="Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
   ylim(0,40)+
  theme(strip.text.y = element_blank(),strip.text.x = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size =18), axis.text.y = element_text(size = 14))+ 
  theme(plot.margin = unit( c(0,0.1,0.1,0), "in"),legend.margin=margin(-10, 0, 0, -6),
         axis.title.y = element_text(size = 13))

# make temperature graph
b<-climate%>%subset(koppen.new =="Humid subtropical") %>% 
  subset(category=="temp") %>%
  ggplot(aes(x= time, y =value, group= City, colour=category, shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City, switch = "x")+
  geom_line(size =0.75, show.legend = FALSE)+
  geom_point(size = 2.5, colour ="black", show.legend = FALSE)+
  labs(title ="Humid subtropical climate", x= "", y ="", shape ="Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(19,38)+
  theme(strip.text.y = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),plot.title = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(plot.margin = unit( c(0,0.1,0.1,0), "in"), legend.margin=margin(-10, 0, 0, -6),
        strip.text.x = element_text(size = 14, angle =0), 
        strip.background = element_rect(color="white", fill="white", size=3), 
        axis.text.y = element_text(size = 14),axis.title.y = element_text(size = 13))

# combine both into one figure 
cfa<-ggdraw() +
  draw_plot(a, x = 0.02, y = .5, width = .97, height = .5) +
  draw_plot(b, x = 0.02, y = 0, width = .97, height = .54)

#------------------------- MAKE WARM MEDITERANEAN FIGURE ------------------------------------------------------------------------------

# make precipitation graph
a<-climate%>%subset(koppen.new =="Warm Mediterranean") %>% 
  subset(category=="prec") %>%
  ggplot(aes(x= time, y =value, group= City, colour="blue", shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City)+
  geom_line(size =0.75, show.legend = FALSE, colour ="blue")+
  geom_point(size = 2.5, colour ="black",show.legend = FALSE)+
  labs(title ="              Warm Mediterranean climate", x= "", y ="      Precipitation of driest month (mm)", shape ="Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(0,40)+
  theme(strip.text.y = element_blank(),strip.text.x = element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size=18),plot.margin = unit( c(0,0.1,0.1,0), "in"),
        legend.margin=margin(-10, 0, 0, -6), axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 13))

# make temperature graph
b<-climate%>%subset(koppen.new =="Warm Mediterranean") %>% 
  subset(category=="temp") %>%
  ggplot(aes(x= time, y =value, group= City, colour=category, shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City, switch = "x")+
  geom_line(size =0.75, show.legend = FALSE)+
  geom_point(size = 2.5, colour ="black",show.legend = FALSE)+
  labs(title ="Warm Mediterranean climate", x= "", y ="Temperature of hottest month (°C)", shape ="Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(19,38)+
  theme(strip.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit( c(0,0.1,0.1,0), "in"), legend.margin=margin(-10, 0, 0, -6))+
  theme(strip.text.x = element_text(size = 14, angle =90), 
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 13),
        strip.background = element_rect(color="white", fill="white", size=3))

# combine both into one figure 
csa<-ggdraw() +
  draw_plot(a, x = 0.02, y = .5, width = .97, height = .5) +
  draw_plot(b, x = 0.02, y = 0, width = .97, height = .54)


#------------------------- MAKE COLD SEMI ARID FIGURE ------------------------------------------------------------------------------

# make precipitation graph
a<-climate%>%subset(koppen.new =="Cold semi-arid") %>% 
  subset(category=="prec") %>%
  ggplot(aes(x= time, y =value, group= City, colour="blue", shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City)+
  geom_line(size =0.75, show.legend = FALSE, colour ="blue")+
  geom_point(size = 2.5, colour ="black",show.legend = FALSE)+
  labs(title ="Cold semi-arid climate", x= "", y ="Precipitation of driest month (mm)", shape ="Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(0,40)+
  theme(strip.text.y = element_blank(),strip.text.x = element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size=18),plot.margin = unit( c(0,0.1,0.1,0), "in"),
        legend.margin=margin(-10, 0, 0, -6), axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 13))

# make temperature graph
b<-climate%>%subset(koppen.new =="Cold semi-arid") %>% 
  subset(category=="temp") %>%
  ggplot(aes(x= time, y =value, group= City, colour=category, shape = factor(time, levels=c("Baseline", "2050", "2070"))))+
  facet_grid(.~City, switch = "x")+
  geom_line(size =0.75, show.legend = FALSE)+
  geom_point(size = 2.5, colour ="black",show.legend = FALSE)+
  labs(title ="Cold semi-arid climate", x= "", y ="Temperature of hottest month (°C)", shape ="Time") +
  scale_x_discrete(limits=c("Baseline","2050","2070"),expand=expansion(mult=c(0.1, 0.1)))+
  ylim(19,38)+
  theme(strip.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.title = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin = unit( c(0,0.1,0.1,0), "in"), legend.margin=margin(-10, 0, 0, -6))+
  theme(strip.text.x = element_text(size = 14, angle =0), 
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 13),
        strip.background = element_rect(color="white", fill="white", size=3))

# combine both into one figure 
bsk<-ggdraw() +
  draw_plot(a, x = 0.02, y = .5, width = .97, height = .5) +
  draw_plot(b, x = 0.02, y = 0, width = .97, height = .54)


#--------------------------- COMBINE ALL FIGURES USING GGDRAW --------------------------------------------------
all<-ggdraw() +
  draw_plot(bsk, x = 0.01, y = 0.5, width = 0.2, height = 0.5) +
  draw_plot(cfa, x = 0.22, y = 0.5, width = 0.3, height = 0.5) +
  draw_plot(dfb, x = 0.5, y = 0.5, width =0.5 , height = 0.5) +
  draw_plot(cfb, x = 0.1, y = 0, width = 0.9, height = 0.51) +
  draw_plot(csa, x = 0.02, y = 0, width = 0.1, height = 0.51) 

#---------------------------- EXPORT FINAL FIGURE -----------------------------------------------------
png(filename="plot.png", 
    type="cairo-png",
    units="px", 
    width=6000, 
    height=6800, 
    pointsize=12, 
    res=450)
print(all)
dev.off()
