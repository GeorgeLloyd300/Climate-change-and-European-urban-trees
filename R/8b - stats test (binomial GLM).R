# Code to create a binomial GLM to model the current and 2070 % threatened carbon in each climate zone 
# date = 16th January 2024 
# Author = George Lloyd, University of Sheffield

# load necessary packages
library(tidyverse)
library(lme4)

# read in dataset with carbon data for each city (repeat with 2nd-98th and 5-59th percentile datasets)
CITY_CARBON<-read.csv("threatened carbon dataset_5_95.csv")

# make a new column with % carbon as integers
CITY_CARBON$carbon.round <- round(CITY_CARBON$carbon)

# add 'events' column with 100s as values
CITY_CARBON <- cbind(CITY_CARBON,
                       events=rep(c(100),times=66))

# change reference level (names of time.points) so they are in a better order
CITY_CARBON$time.point[CITY_CARBON$time.point=="current"]<-"A-current"
CITY_CARBON$time.point[CITY_CARBON$time.point=="2050"]<-"B-2050"
CITY_CARBON$time.point[CITY_CARBON$time.point=="2070"]<-"C-2070"

# Alter the name of a climate zone slightly
CITY_CARBON$koppen[CITY_CARBON$koppen=="Temperate oceanic"]<-"A-Temperate oceanic"

# GLM without interaction 
City_GLM1<- glmer(carbon.round / events ~ time.point + koppen + (1 | city), weights = events,
      family = binomial, data = CITY_CARBON)
summary(City_GLM1)

# plot residuals
res <- resid(City_GLM1)
plot(fitted(City_GLM1), res)
abline(0,0)

# ANOVA

# fit the "full" model (i.e. the model with all of the predictors, including the categorical predictor(koppen, call this g1) 
g1<- glmer(carbon.round / events ~ time.point + koppen + (1 | city), weights = events,
           family = binomial, data = CITY_CARBON)

# fit the model without the categorical predictor (koppen, call this g0)
g0<- glmer(carbon.round / events ~ time.point + (1 | city), weights = events,
           family = binomial, data = CITY_CARBON)

# fit anova
anova(g1, g0)