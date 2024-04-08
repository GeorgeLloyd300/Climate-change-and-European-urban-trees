###############################################################################
############################ 7A. MAKE FIG 1 KOPPEN MAP ########################
###############################################################################

# Create a map of Europe with koppen zones overlaid with all study cities 
# date = June 2021 
# Author = George Lloyd, University of Sheffield

# load necessary packages 
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(concaveman)
library(mapview)
library(viridis)
library(raster)
library(tiff)


#---------------------------- IMPORT AND EDIT CITY DATA --------------------------------------------

# get data on lon lat of each city
city<-read.csv("Data/Species city and equation data/22.city.data.csv")

# seperate df into climate zones - so we can label points different shapes
a<-subset(city, Koppen.new =="Temperate oceanic")
b<-subset(city, Koppen.new =="Humid continental")
c<-subset(city, Koppen.new =="Humid subtropical")
d<-subset(city, Koppen.new =="Warm Mediterranean")
e<-subset(city, Koppen.new =="Cold semi-arid")

# remove some cities and make seperate df of just Bristol to label differently as it overlaps with London label
bristol<-subset(a, City == "Bristol")
a<-subset(a, !City =="Bristol")
dublin<-subset(a, City == "Greater Dublin")
a<-subset(a, !City =="Greater Dublin")

# extract names of cities to a vector for creating labels later
names.a<-a$City
names.b<-b$City
names.c<-c$City
names.d<-d$City
names.e<-e$City
names.bristol<-bristol$City
names.dublin<-dublin$City

# change row names to citys
rownames(a)<-a$City
rownames(b)<-b$City
rownames(c)<-c$City
rownames(d)<-d$City
rownames(e)<-e$City
rownames(bristol)<-bristol$City
rownames(dublin)<-dublin$City

# make dataframes into formal class spatialpoints
coordinates(a) <- c("Lon","Lat")
coordinates(b) <- c("Lon","Lat")
coordinates(c) <- c("Lon","Lat")
coordinates(d) <- c("Lon","Lat")
coordinates(e) <- c("Lon","Lat")
coordinates(bristol) <- c("Lon","Lat")
coordinates(dublin) <- c("Lon","Lat")

#--------------------------- CREATE BACKGROUND MAP ---------------------------------------------------------------

# create eu background map using rnaturalearth package
europe <- ne_countries(continent = "Europe", scale = "medium")

# get koppen world map from .tif file
koppen<-raster("Beck_KG_V1_present_0p0083.tif")

# create extent for EU focus zone (left, right, bottom, top)
eu <- extent(c(-10, 29, 37, 62))

# crop raster and shp file to this extent
r <- crop(koppen, eu)
shp <- crop(europe, eu)

#---------------------- PLOT MAPS USING BASE PLOT FUNCTION ------------------------------------------------------------

plot(r)
plot(shp, lwd = 0.5, add =T)
plot(a, col = "white", bg ="grey10",pch =21, cex=1.7, add =T) # Temperate
text(a, labels = names.a, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 3, offset = 0.65)
plot(b, col = "white", bg ="grey10", pch =22, cex=1.5, add =T) # Humid continental
text(b, labels = names.b, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 3, offset = 0.65)
plot(c, col = "white", bg ="grey10", pch =23, cex=1.5, add =T) # Humid subtropical
text(c, labels = names.c, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 3, offset = 0.65)
plot(d, col = "white", bg ="grey10", pch =24, cex=1.5, add =T) # Warm Mediterranean
text(d, labels = names.d, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 3, offset = 0.65)
plot(e, col = "white", bg ="grey10", pch =25, cex=1.5, add =T) # Cold semi-arid
text(e, labels = names.e, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 3, offset = 0.65)
plot(bristol, col = "white", bg ="grey10",pch =21, cex=1.7, add =T) # Bristol 
text(bristol, labels = names.bristol, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 1, offset = 0.4)
plot(dublin, col = "white", bg ="grey10", pch =21, cex=1.7, add =T) # Dublin
text(dublin, labels = names.dublin, halo = TRUE, hw = 0.08, hc = "white", col = "black",pos = 1, offset = 0.25)

#--------------------------------- EXPORT MAP -----------------------------------------------------------------
png('output/my_plot.png')
par(mar = c(4.1, 4.4, 4.1, 1.9), xaxs="i", yaxs="i")
plot(flowers$weight, flowers$shootarea, 
     xlab = "weight (g)",
     ylab = expression(paste("shoot area (cm"^"2",")")),
     xlim = c(0, 30), ylim = c(0, 200), bty = "l",
     las = 1, cex.axis = 0.8, tcl = -0.2,
     pch = 16, col = "dodgerblue1", cex = 0.9)
text(x = 28, y = 190, label = "A", cex = 2)
dev.off()


#-------------------- DETERMINE WHICH KOPPEN ZONES CITIES ARE IN ----------------------------------------------------------------------
# some are in between zones so are difficult to identify - this methodology classifies cities into zones

# make df (polygon) of a cities extent/boundaries (BUDAPEST)
df <- data.frame (Lon  = c(19.098, 19.184, 19.319, 19.332, 19.143,19.081, 18.958, 18.984, 18.925),
                  Lat  = c(47.614, 47.544, 47.516, 47.461, 47.351, 47.405, 47.375, 47.463, 47.573))

# need to close this polygon (first and last points must be identical)
df <- rbind(df, df[1,])

# make it a formal class spatialpoints
coordinates(df) <- c("Lon","Lat")

# convert to spatial lines
df <- as(df,"SpatialLines")

# crop koppen zone map to city boundaries 
# (numbers equate to left, right, bottom, top latitude)
eu <- extent(c(18.87, 19.37, 47.35, 47.65 ))
r <- crop(koppen, eu)

# plot this map which you can zoom in on and decide by eye which zone it is in
plot(r)
plot(df, add=T)

## if its not immediately obvious by eye need a different method

#first make city extent into a polygon 
df_sf <- st_as_sf(df) 
df_poly <- st_polygonize(df_sf)

# then crop koppen raster to this polygon and check it looks right
r2 = mask(r,df_poly)
plot(r2)

# then convert raster to points 
df<-as.data.frame(rasterToPoints(r2))

# see how many individual km2 there are of each climate type within the 
# cities boundaries. 
table(df$Beck_KG_V1_present_0p0083)

# Numbers in table code for different climate type as seen on koppen website 


