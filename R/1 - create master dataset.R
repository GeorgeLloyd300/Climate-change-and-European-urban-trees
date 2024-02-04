###############################################################################
############################ 1. CREATE MASTER DATASET #########################
###############################################################################



# Compiling data from all 22 cities into one dataset
# date = Jan - Feb 2021 
# Author = George Lloyd, University of Sheffield

# load necessary packages 
library(tidyverse)
library(Hmisc)
library(tinytex)
library(Taxonstand)



#------------------------------------------------------------------------------------------------------------------------------------------------------------

# this first section reads in raw data from each city, tidies it up and saves it
# this is an example how it was done for one city (Budapest) but as you need to manually repeat it for each city it takes ages...
# therefore a folder of already tidied inventory data is provided at "./Data/Tidy inventory data" which allows the user to skip to line 262


# import the raw inventory data for a city
city <- read.csv("./Data/Raw inventory data/Budapest.csv")


# select relevant columns and add city.name
city <- city %>% 
  select(Tree.species, Trunk.diameter..cm...1m.height, Tree.height..m.) %>%
  mutate(city.name = "Budapest", climate = "Cfa")


# change column names 
colnames (city) = c("species" , "diameter" , "height" , "city.name" , "climate")


# re-order columns
city <-  city[ , c("city.name", "species", "diameter", "height", "climate")]   


# remove rows with no data (nothing in the species column)
city <- subset(city, species!=" ")


# remove rows containing specific strings e.g. unknown, unspecified etc
master.new <-  master.new[!grepl("unknown", master.new$species),]


# change characters in whole column
city$height= gsub("<","0-", city$height)


# remove strings within special characters e.g. () and "
city$diameter = gsub("\\*|\\(|\\)","", city$diameter)

# remove strings within ()
city <- (gsub("\\*\\([^\\)]+\\)","",as.character(city$diameter))) 


# replace NA's with values
city[c("height")][is.na(city[c("height")])]  <-  2.5


# create latin name format 
city <- city  %>% mutate(species = tolower(species))  %>%
  mutate(species = capitalize(species))


# combine two columns e.g. genus and species into one if this is applicable to raw data
city$species  <-  paste(city$Genere, city$Specie, sep=" ")






#------------------------------------------------------------------------------------------------------------------------------------------------------------
# converting ranges of height and diameter into just one number (the midpoint)

# change characters (e.g. <) to ensure there is a '-' for the split below
city$height= gsub("<","0-", city$height)


# split column up based on '-'
city <- city %>%
  separate(height, c("lower", "upper"), "-")


# change to numeric
city$lower  <-  as.numeric(city$lower)
city$upper  <-  as.numeric(city$upper)


# create mean of this split column
city <- city %>% 
  mutate(height=(upper+lower)/2)


# remove upper and lower columns
city <-  dplyr ::select(city, -lower, - upper)


#  IF ONLY CIRCUMFERENCE DATA - CALCULATE DIAMETER AS THIS IS WHAT WE NEED 
city <- city %>% 
  mutate(diameter=(diameter)/pi)


city$diameter  <-  as.numeric(city$diameter)





#------------------------------------------------------------------------------------------------------------------------------------------------------------
# creating taxonomically correct species list

# run the taxonomic check against The Plant List (TPL) via Taxonstand to produce the
# taxonomically corrected species list 


# create a unique list of species from master dataset above
species <-  as.data.frame(unique(master$species))
colnames (species) = c( "original_name")


##### This function might take a while as TPL has to access a server
##### In case you get an error (e.g., Service Unavailable) you can batch this process.

species_TLPed.5500.tidy  <-  Taxonstand::TPL(species.formatted$species, infra = TRUE, corr = TRUE, repeats = 100)  # this takes 5' on my computer


# make df with relevant col n rows
taxonstand.sp <-  species_TLPed.5500.tidy  %>%
  select(Taxon, New.Genus, New.Species)


# make new_name by combining genus n species
taxonstand.sp$new_name  <-  paste(taxonstand.sp$New.Genus, taxonstand.sp$New.Species, sep=" ")


CHANGED.true <- CHANGED.true  %>%
  select(Taxon, new_name)
colnames (CHANGED.true) = c( "original_name", "new_name")


taxonstand.sp <- select(taxonstand.sp, Taxon, new_name)


save(species_TLPed.5500, file = "species_TLped.5500.Rdata", compress = F)


colnames(taxonstand.sp)=c("original_name", "new_name")





#--------------------------------------------------------------------------------------------
# use match function to replace old species names with new ones by matching old and new


# e.g. new_species = lookup_newspecies_column[match(Orignal_species, Lookup_old_species_column)]
AMENDED_species  <-  as.data.frame(CHANGED.true$new_name[match(species$original_name, CHANGED.true$original_name)])


# get no. of unique species out of 4001 that were corrected. no. = 1443!?
UNIQUE.AMENDED <- as.data.frame ((unique(AMENDED_species$AMENDED_species)))


### at this point i have managed to correct 4000 out of 5500
### this taxo correct list of 4000 contained 1443 unique sp

### i need to correct the last 1500 some other way 
### i will do this by manually formatting the 1500 then retrying taxon check


#change characters in whole column
species.formatted  <-  as.data.frame(gsub('sp.', '',  species$original_name))
colnames(species.formatted) <- c("species")

species.formatted  <-  as.data.frame(gsub('Spp', '', species.formatted$species))
colnames(species.formatted) <- c("species")

species.formatted  <-  as.data.frame(gsub('spp.', '', species.formatted$species))
colnames(species.formatted) <- c("species")

species.formatted  <-  as.data.frame(gsub('sspp.', '', species.formatted$species))
colnames(species.formatted) <- c("species")

species.formatted  <-  as.data.frame(gsub('n. sp.', '', species.formatted$species))
colnames(species.formatted) <- c("species")

# remove all strings within ()
species.formatted <- as.data.frame(gsub("\\s*\\([^\\)]+\\)","",(species.formatted$species))) 
colnames(species.formatted) <- c("species")

# remove all strings within '' 
species.formatted <- as.data.frame(gsub("\\s*\\'[^\\)]+\\'","",(species.formatted$species)))
colnames(species.formatted) <- c("species")





#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Making new taxonomically correct species column in each city

# read in list of taxonomically correct sp made above
correct.sp <- read.csv("master.taxo.tidying.summary.csv")


# match corrected species column with original city column - if doesnt match then continue
correct.budapest <- as.data.frame((correct.sp$new_manual[match (city$species, 
                                                  correct.sp$original.name)]))
colnames(correct.budapest)=c("species")


# create a df with every species in the city
budapest.unique.sp <- as.data.frame(unique(city$species)) 


# run taxonstand on these 
budapest.taxonstand <-  Taxonstand::TPL(budapest.unique.sp$`unique(city$species)`, 
                                        infra = TRUE, corr = TRUE, repeats = 100)  # this takes 5' on my compute


# merge newgenus and newspecies cols
budapest.taxonstand$new.name  <-  paste(budapest.taxonstand$New.Genus, budapest.taxonstand$New.Species, sep=" ")


# select relevant columns from taxonstand output
budapest.taxonstand <- select(budapest.taxonstand, Taxon, Plant.Name.Index, new.name)


# manually edit the last few that havnt been matched in excel 
write.csv(budapest.taxonstand, "budapest.taxonstand.csv")
budapest.taxonstand.1 <- read.csv("budapest.taxonstand.csv")


# match this new corrected species column with original species column in city 
budapest.corrected <- as.data.frame(budapest.taxonstand.1$new.name[match (city$species, 
                                                  budapest.taxonstand.1$Taxon)])

colnames(budapest.corrected)=c("new.species")


# add corrected names back onto original df as new.species column
city <- mutate(city, new.species = budapest.corrected$new.species)


# SAVE TIDY DATA FOR THIS CITY AND MOVE ONTO NEXT CITY
write.csv(city, ("Outputs/Tidy Inventory data/Budapest.tidy.csv"))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creating master dataset 1 - simply tidied city data added together 
# When all inventories are tidied (you can find tidied inventory data at https://drive.google.com/drive/folders/15gT8xcDCiI0B11qZ9hvwK2KmtturCLYG?usp=drive_link
# skipping the long manual process above)...
# bind them together to create master dataset 1


# at this point over 60% of trees in Oslo were determined to be only genus level binomials so it was removed from this study - genus level equations are far less accurate

df <- list.files(path='Outputs/Tidy inventory data') %>% 
  lapply(read_csv) %>% 
  bind_rows

assen    <- read.csv("Outputs/Tidy inventory data/Assen.tidy.csv")
belfast  <- read.csv("Outputs/Tidy inventory data/Belfast.tidy.csv")
bologna  <- read.csv("Outputs/Tidy inventory data/Bologna.tidy.csv")
bolzano <- read.csv("Outputs/Tidy inventory data/Bolzano.tidy.csv")
bordeaux <- read.csv("Outputs/Tidy inventory data/Bordeaux.tidy.csv")
budapest <- read.csv("Outputs/Tidy inventory data/Budapest.tidy.csv")
bristol  <- read.csv("Outputs/Tidy inventory data/Bristol.tidy.csv")
caceres  <- read.csv("Outputs/Tidy inventory data/Caceres.tidy.csv")
camden  <- read.csv("Outputs/Tidy inventory data/Camden.tidy.csv")
fingal.county  <- read.csv("Outputs/Tidy inventory data/Fingal county.tidy.csv")
geneva <- read.csv("Outputs/Tidy inventory data/Geneva.tidy.csv")
girona <- read.csv("Outputs/Tidy inventory data/Girona.tidy.csv")
hamburg <- read.csv("Outputs/Tidy inventory data/Hamburg.tidy.csv")
madrid <- read.csv("Outputs/Tidy inventory data/Madrid.tidy.csv")
helsinki <- read.csv("Outputs/Tidy inventory data/Helsinki.tidy.csv")
montpellier <- read.csv("Outputs/Tidy inventory data/Montpellier.tidy.csv")
namur <- read.csv("Outputs/Tidy inventory data/Namur.tidy.csv")
paris <- read.csv("Outputs/Tidy inventory data/Paris.tidy.csv")
turin <- read.csv("Outputs/Tidy inventory data/Turin.tidy.csv")
vienna <- read.csv("Outputs/Tidy inventory data/Vienna.tidy.csv")
warsaw <- read.csv("Outputs/Tidy inventory data/Warsaw.tidy.csv")
zagreb<- read.csv("Outputs/Tidy inventory data/Zagreb.tidy.csv")

# bind datasets
master_1 <-  rbind(assen, belfast, bologna, bolzano, bordeaux, budapest, bristol, caceres, 
               camden, fingal.county, geneva, girona, hamburg, helsinki, madrid, 
               helsinki, montpellier, namur, paris, turin, vienna, warsaw, zagreb)



#-------------------------------------------------------------------------------------
# Creating master dataset 2 - further tidying of inventory data

# remove rows with blanks or NA's in species or diameter columns 
# (these are both essential fields which need data)
master_2  <-  master_1  %>%
  subset(diameter!="NA")  %>%
  subset(diameter!="0")  %>%
  subset(diameter!="No Code Allocated")  %>%
  subset(new.species!="NA")
  
master_2[master_2$diameter == " "]  <-  NA 


# remove NA's as above method didnt work for some reason  
master_2$new.species= gsub("NA","no", master_2$new.species)


master_2 <- master_2[!grepl("no", master_2$new.species),]


# change 0's to na's in height column 
master_2[master_2 == 0]  <-  NA


# change platanus acerifolia to platanus hispanica (2 different names for london plane)
master_2$new.species= gsub("Platanus acerifolia","Platanus hispanica", master_2$new.species)


# remove words from diameter column
master_2$diameter= gsub("Centrimetres","", master_2$diameter)


# remove rows with - diameter or height
master_2$diameter= gsub("-","", master_2$diameter)
master_2$height= gsub("-","", master_2$height)


#### change bordeaux diameter (its circumference not diameter)


# subset bordeaux
bordeaux <- subset(master_2, city.name == "Bordeaux") 


# remove from master 
master_2  <-  master_2  %>%             
  subset(city.name!="Bordeaux")


# calc diam
bordeaux <- bordeaux %>% 
  mutate(diameter=(diameter)/pi)


bordeaux$diameter  <-  as.numeric(bordeaux$diameter)


# add back in 
master_2 <- rbind(master_2, bordeaux)


#caceres looks like diameter is in metres so need to x100 
# subset caceres
cac
eres <- subset(master_2, city.name == "Caceres") 

caceres$diameter  <-  as.numeric(caceres$diameter)
caceres$height    <-  as.numeric(caceres$height)


caceres <- mutate(caceres, diameter = (diameter)*100)


# remove caceres from master 
master_2  <-  master_2  %>%             
  subset(city.name!="Caceres")


# add back in corrected version
master_2 <- rbind(master_2, caceres)


master_2$diameter  <-  as.numeric(master_2$diameter)
master_2$height  <-  as.numeric(master_2$height)


#--------------------------------------------------------------------------------------
# make master dataset 3 
# Removing species occuring less than 250 times so we can reduce the size of the master dataset

# first find total no. of each species 
no.trees <- master_2 %>% group_by(new.species) %>% summarize(freq =(n()))


# then filter ones occuring <250
less_250 <- filter(no.trees, freq <= 250)


# create vector of species name to be removed
remove_these <- as.vector(less_250$new.species)


# remove all rows that contain these names from master dataset
master_3 <- anti_join(master_2, less_250, by = "new.species")


#-------------------------------------------------------------------------------------------------
# making master 4 - final changes

# remove some palm species that have no equation or dwd (I.E. they arnt TRUE trees)
master_4 <- master_3 %>%
  subset(!new.species == "Phoenix canariensis")

master_4 <- master_4 %>%
  subset(!new.species == "Trachycarpus fortunei")

master_4 <- master_4 %>%
  subset(!new.species == "Washingtonia filifera")

master_4 <- master_4 %>%
  subset(!new.species == "Yucca" )

master_4 <- master_4 %>%
  subset (!new.species == "Trachycarpus fortunei ")

# remove trailing white space from strings to combat names appearing twice

master_4$new.species <- (trimws(master_4$new.species, which = c("right")))
master_4$new.species= gsub("Gingkgo","Ginkgo", master_4$new.species)


# removing trees with diameter- height ratio way off
# just very low or high measurements 

# (this will remove cities with na in percentage ie. missing height values )
# therefore, remove those cities then add back on after:
master_4.no.height <- subset(master_4,city.name == "Bristol"| 
                               city.name == "Hamburg"| 
                               city.name == "Oslo"|city.name == "Girona")


# make percentage col then remove ones that are outliers
master_4 <- master_4 %>%
  
  mutate(percentage   = diameter/height*100) %>%
  subset(!percentage >= 2000) %>%
  subset(!percentage <= 50)  %>%
  select(-percentage)

# add back on cities without height
master_dataset <- rbind(master_4, master_4.no.height)


# save master dataset as the final dataset and output of step 1 of this github repo
save(master_dataset, file = "Outputs/dataset_1.csv", compress = F)


###############################################################################
################################# TBC #########################################
###############################################################################

