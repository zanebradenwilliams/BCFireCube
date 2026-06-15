##Technical Validation
##2011 Population Raster

#Accurate counts are taken from https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/prof/details/page.cfm?Lang=E&Geo1=PR&Code1=59&Geo2=PR&Code2=01&Data=Count&SearchText=British%20Columbia&SearchType=Begins&SearchPR=01&B1=All&Custom=&TABID=1

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
pop <- rast("Products/2011_bcrast_population.nc")

## Step 3: Verify population count and density

#population should be 4,400,057
truepop <- 4400057

#true area should be 922,509.29 km^2
truearea <- 922509.29

#true density should be 4.769661/km^2
trueden <- truepop/truearea

#aggregate to see our estimate density
estden <- global(pop, fun = "mean", na.rm = TRUE)
#we get 4.941532

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.0360342

#aggregate to see our estimate population
estpop <- global(pop, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estpop <- estpop*0.0009
#we get 4535300

#calculate error
accpop <- (estpop - truepop)/truepop
#we get 0.03073656