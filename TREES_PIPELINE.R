######################################################################################
###########################  ------ EURO TREE ANALYSIS ---- ##########################
######################################################################################



# \ 
# 
# This code prepares all the tree inventory data, runs all calculations and produces results/figures  \
#   
# \



## To-do ----

## Spatial and conceptual coupling of consequence and likelihood - rules come from different part 
## of likelihood uniqueness - .


## Re-format HEVAE Risk and Threatened species tables
## Check flow metrics
## Check all feature data 
## Check all decision points
## Integrate LUT with processing code : which variables will users change?


## Clear environment
rm(list = ls())
options(warn = 0)


## These flags control what is run
## some could be in a LUT (excel workbook).
## If the data prep and analyses is already done, the initial flags can be false
## These steps are the initial data prep.
read_enviro  <- FALSE  ## read in enviro spatial (feature) inputs from disk and pre-process?
calc_niche   <- FALSE  ## Calc niches?
calc_safety  <- FALSE
run_stats    <- TRUE

## The above flags are used inside* the scripts below
## add a switch to each one




# 1 :: LOAD DATA ----


## These scripts set the environments, reads in all the inventory and climate data and calculates carbon storage
source('./R/1_create_master_City_dataset.R')
source('./R/2 - calculating carbon storage.R')
source('./R/3 - extracting CHELSA climate data.R')




# 2 :: CALCULATE NICHES  ---- 


## These scripts calculate the climate niche position of all species within each city at different time points 
if(calc_niche) {
  
  source('./R/4 - Calculate tree species niches.R')

}



# 3 :: CALCULATE SAFETY MARGINS ---- 


if(calc_safety) {
  
  source('./R/5a- calculating species hydraulic safety margin or gap.R')
  source('./R/5b - calculating species thermal safety margin or gap.R')
  source('./R/6 - other calculations.R')
  
}


# 4 :: CREATE RESULTS FIGS AND TABLES ---- 


## These scripts calculate the consequence measures for each Decision Criteria
## They are split up by length - some DC's are too big to combine in one script...
if(figures) {
  
  ## 
  source('./R/7a - make figure 1 (koppen map).R') ## Create Figure 1). for the publication
  source('./R/7b - make figure 3 (climate graph).R') ## Create Figure 1). for the publication
  ## ETC 

}




# 5 :: RUN STAT MODEL ---- 

if(run_stats) {
  
source('./R/8 - statistical analysis (binomial GLM).R')

}



# 6 :: AGGREGATE RESULTS ---- 


## Combine Data and Results into a geo-package / data base, so that others can map it?
## Eg thematic feature layer maps of thermal margins, etc?





######################################################################################
###########################  ------ CHR RIGHTS ANALYSIS ---- #########################
######################################################################################
