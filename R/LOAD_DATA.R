###############################################################################
################################# LOAD DATA ###################################
###############################################################################



## Script to load in all baseline packages and data


## Function that installs packages if not already done
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





# STEP 1 :: Load city data ========================================


## 



# STEP 2 :: Load taxonomy data ========================================

# read in list of taxonomically correct sp made above
correct.sp <- read.csv("master.taxo.tidying.summary.csv")



# STEP 3 :: Load Raster data ========================================


## Could do that here..

# rastlist <- list.files(pattern='CHELSA', all.files=TRUE, full.names=FALSE)
# 
# # then convert to raster stack
# allrasters <-stack(rastlist)
# 
# ras <- lapply(rastlist,raster) 


