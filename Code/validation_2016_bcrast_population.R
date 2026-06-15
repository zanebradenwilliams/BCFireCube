##Technical Validation
##2016 Population Raster

#Accurate counts are taken from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page.cfm?Lang=E&Geo1=PR&Code1=59&Geo2=PR&Code2=01&SearchText=British%20Columbia&SearchType=Begins&SearchPR=01&B1=All&TABID=1&type=0

## Step 1: Call libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(viridis)
library(terra)
library(sf)
library(tidyterra)

## Step 2: Load data

#harmonized population data
pop <- rast("Products/2016_bcrast_population.nc")

## Step 3: Verify population count and density

#population should be 4,648,055
truepop <- 4648055

#true area should be 922,503.01 km^2
truearea <- 922503.01

#true density should be 5.038526/km^2
trueden <- truepop/truearea

#aggregate to see our estimate density
estden <- global(pop, fun = "mean", na.rm = TRUE)
#we get 5.211401

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.03431066

#aggregate to see our estimate population
estpop <- global(pop, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estpop <- estpop*0.0009
#we get 4,782,361

#calculate error
accpop <- (estpop - truepop)/truepop
#we get 0.03431066