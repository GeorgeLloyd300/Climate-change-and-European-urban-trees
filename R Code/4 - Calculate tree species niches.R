###############################################################################
############################ 4. CALC NICHES ###################################
###############################################################################


# \ 
# 
# The text and code below summarises a workflow in R that can be used to relatively rapidly assess 
# the environmental range of a species. See :
# https://www.sciencedirect.com/science/article/pii/S0048969719323289#f0030 
#   
#   \


## Function to load or install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="https://cran.csiro.au/")
  sapply(pkg, require, character.only = TRUE)
}


## Install Main package, if needed
# devtools::install_github("HMB3/habitatIntersect")


## Load packages 
library(habitatIntersect)
ipak(sdmgen_packages)





# STEP 1 :: Download species occurrence data ========================================


# The backbone of the R workflow is a list of (taxonomically Ridgey-Didge!) Taxa names 
# that we supply. The analysis is designed to process data for one species at a time, 
# allowing species results to be updated as required. 


## Create lists of all taxa that we will model at all levels
analysis_taxa = read_csv('./data/Taxonomy/George_species_AO_12.04.2021.csv') %>%
  .$Species %>% as.character() %>% unique() %>% str_trim() %>% .[!is.na(.)]  %>% sort()

analysis_taxa



# The species list is supplied to a series of functions to calculate environmental ranges 
# and habitat suitability. The initial functions download all species records from the the Global 
# Biodiversity Information Facility (GBIF, https://www.gbif.org/). The species data are downloaded 
# as individual .Rdata files to the specified folders, which must exist first, without returning anything.


# Now download GBIF and ALA occurrence data for each species. The downloading functions 
# are separated, because the ALA and GBIF columns are slightly different, but both 
# data sources are needed to properly quantify species ranges. The package functions 
# expect these folders (a typical R project structure), create them if they don't exist


## Download data using the rgbif pacakge - this only gets species with < 200k records.
## Species with > 200k records need to be done manually on GBIF site
download_GBIF_all_species(species_list  = analysis_taxa,
                          download_path = "./data/GBIF/",
                          download_limit = 200000)



# STEP 2 :: Combine species occurrence data

#We will use the chelsa bioclim data (https://chelsa-climate.org/bioclim/)


## Use worldclim 2 data
## https://drive.google.com/open?id=1mQHVmYxSMw_cw1iGvfU9M7Pq6Kl6nz-C
chelsa_bioclim <- list.files('./data/Chelsa/', full.names = TRUE)
chelsa_climate <- raster::stack(chelsa_bioclim)


chelsa_annual_temp   <- chelsa_climate[[1]]
chelsa_annual_precip <- chelsa_climate[[12]]



# Next we filter the records to those taken after 1950, and those inside the raster boundaries 
# (i.e. species records in the ocean according to the raster boundaries will be excluded).


## Combine ALA data, and filter to records on land taken > 1950
data('gbif_keep')


## Combine Family ALA data
GBIF.LAND.SPP <- combine_gbif_records(species_list      = analysis_taxa,
                                      records_path      = "./data/GBIF/",
                                      records_extension = "_GBIF_records.RData",
                                      record_type       = "GBIF",
                                      keep_cols         = gbif_keep,
                                      world_raster      = chelsa_annual_temp)





# STEP 3 :: extract environmental values ========================================


# The next step requires a template raster of 1km * 1km cells, which is used to filter 
# records to 1 per one 1km cell. This raster needs to have the same extent (global) 
# resolution (1km) and projection (WGS84) of the data used to analyse the species 
# distributions. It should have a value of 1 for land, and NA for the ocean. 
# This takes ages in R.....


## This raster has no data for the ocean, 1 for land, 1km*1Km resolution in WGS84
template_raster_1km_WGS84 = raster("./data/template_1km_WGS84.tif")


# The next function in the workflow combines occurrence files from ALA and GBIF into one table, 
# and extracts environmental values. It assumes that both files come from the combine_ala_records 
# and combine_gbif_records functions. Note that the order of the raster names in 'world_raster' 
# must match the order of names in the character vector 'env_variables'. In this case, it's simply 
# the biolclim variables (i.e. bio1-bio19)


## Combine GBIF and ALA data, and extract environmental values
## Note that the climate values are too small
COMBO.RASTER.SPP = combine_records_extract(ala_df          = GBIF.LAND.SPP,
                                           site_df         = 'NONE',
                                           thin_records    = TRUE,
                                           template_raster = template_raster_1km_WGS84,
                                           world_raster    = chelsa_climate,
                                           prj             = CRS("+init=epsg:4326"),
                                           species_list    = analysis_taxa,
                                           
                                           ## These two will need to change.
                                           ## Specify them in the code
                                           biocl_vars      = bioclim_variables,
                                           env_vars        = env_variables,
                                           
                                           ## This might need to change too
                                           raster_divide   = TRUE,
                                           raster_denom    = 10,
                                           save_data       = FALSE,
                                           save_run        = "EUROPE_PLANTS",
                                           data_path       = "./output/results/")





# STEP 4 :: Automated cleanin' of outlier records ========================================

# The workfow uses four shapefiles as part of analysis and mapping: Australia, the World, 
# the global Koppen Zones. The Koppen data are from CliMond, centred on 1975: 
#   https://www.climond.org/Core/Authenticated/KoppenGeiger.aspx


# The next stage of the workflow use a series of cleaning functions to automate the removal 
# of records for each species which are outliers. Doing this manually is extremely tedious, 
# and although errors will be made, automation is preferable across large suites of taxa. 
# The first cleaning function takes a data frame of all species records, and flag records 
# as institutional or spatial outliers. This function uses the CoordinateCleaner package: 
#   https://cran.r-project.org/web/packages/CoordinateCleaner/index.html. It takes the 
# records data.frame is that returned by the combine_records_extract function above.


COORD.CLEAN = coord_clean_records(records    = COMBO.RASTER.SPP,
                                  capitals   = 10000,  
                                  centroids  = 5000,   
                                  save_data  = FALSE,
                                  save_run   = "TARGET_INSECT_SPECIES",
                                  data_path  = "./output/results/")


# The next cleaning function takes a data frame of all species records, flags records as 
# spatial outliers (T/F for each record in the df), and saves images of the checks for each. 
# Manual cleaning of spatial outliers is very tedious, but automated cleaning makes mistakes, 
# so checking is handy. This function uses the CoordinateCleaner package 
# https://cran.r-project.org/web/packages/CoordinateCleaner/index.html. 
# It assumes that the input dfs are those returned by the coord_clean_records function.


## Flag spatial outliers
SPATIAL.CLEAN = check_spatial_outliers(all_df       = COORD.CLEAN,
                                       land_shp     = LAND,
                                       site_df      = FALSE, 
                                       clean_path   = './data/ALA/Check_Plots/',
                                       plot_points  = FALSE,
                                       record_limit = 300000,
                                       spatial_mult = 10,
                                       prj          = CRS("+init=epsg:4326"))


# The next cleaning function takes a data frame of all species records, estimates the geographic 
# and environmental ranges for each species, and creates a table of all species ranges. 
# It uses the AOO.computing function in the ConR package: https://cran.r-project.org/web/packages/ConR/index.html
# It assumes that the input df is that returned by the check_spatial_outliers function.

## Estimate climate niches using species records
GLOB.NICHE = calc_enviro_niches(coord_df     = SPATIAL.CLEAN,
                                prj          = CRS("+init=epsg:4326"),
                                country_shp  = AUS,
                                world_shp    = LAND,
                                kop_shp      = Koppen_shp,
                                species_list = analysis_taxa,
                                env_vars     = env_variables,
                                cell_size    = 2,
                                save_data    = TRUE,
                                save_run     = "EUROPE_PLANTS",
                                data_path    = "./output/results/")


# We can also plot the environmental ranges of each species. This function 
# takes a data frame of all species records, and plots histograms and convex hulls for 
# each species in global environmental space. It assumes that the input df is that 
# prepared by the check_spatial_outliers function.


## Plot species ranges using histograms and convex hulls for rainfall and temperature distributions
plot_range_histograms(coord_df     = SPATIAL.CLEAN,
                      species_list = analysis_taxa,
                      range_path   = check_dir)




###############################################################################
################################# TBC #########################################
###############################################################################
