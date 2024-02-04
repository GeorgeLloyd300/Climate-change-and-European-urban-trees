######################################################################################
###########################  ------ EURO TREE ANALYSIS ---- ##########################
######################################################################################



# \ 
# 
# This code prepares all the tree inventory data, runs all calculations and produces results/figures  \
#   
# \



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


## These scripts read in and tidy all the inventory data, extract climate data and calculates carbon storage
source('./R/1 - create master dataset.R')
source('./R/2 - calculating carbon storage.R')
source('./R/3 - extracting CHELSA climate data.R')




# 2 :: CALCULATE NICHES  ---- 


## This script calculates the climate niche position of all species within each city at different time points 
if(calc_niche) {
  
  source('./R/4 - Calculate tree species niches.R')

}



# 3 :: CALCULATE SAFETY MARGINS ---- 


## These scripts calculate a species safety margin or gap at different time points in each city and also 
## carries out other necessary calculations

if(calc_safety) {
  
  source('./R/5a- calculating species hydraulic safety margin or gap.R')
  source('./R/5b - calculating species thermal safety margin or gap.R')
  source('./R/6a - other calculations.R')
  source('./R/6b - other calculations continued.R')
  
}


# 4 :: CREATE RESULTS FIGS AND TABLES ---- 


## These scripts create all figures 
if(figures) {
  
  ## 
  source('./R/7a - make figure 1 (koppen map).R') ## Create Figure 1). for the publication
  source('./R/7b - make figure 2 (climate graph).R') ## Create Figure 1). for the publication
  source('./R/7c - make figure 3 (climate graph).R') ## Create Figure 1). for the publication
  source('./R/7d - make figure 4 (climate graph).R') ## Create Figure 1). for the publication
  
  

}




# 5 :: RUN STAT MODEL ---- 

## These scripts create a dataset and then run a GLMER on it

if(run_stats) {
  
source('./R/8 - statistical analysis (binomial GLM).R')

}



# 6 :: AGGREGATE RESULTS ---- 


## Combine Data and Results into a geo-package / data base, so that others can map it?
## Eg thematic feature layer maps of thermal margins, etc?


