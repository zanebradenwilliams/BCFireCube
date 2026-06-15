##Technical Validation
##2021 Population Raster

#Accurate counts are taken from https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&SearchText=British%20Columbia&DGUIDlist=2021A000259&GENDERlist=1,2,3&STATISTIClist=1,4&HEADERlist=0

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
pop <- rast("Products/2021_bcrast_population.nc")

## Step 3: Verify population count and density

#population should be 5,000,879
truepop <- 5000879

#true area should be 920,686.55 km^2
truearea <- 920686.55

#true density should be 5.431685/km^2
trueden <- truepop/truearea

#aggregate to see our estimate density
estden <- global(pop, fun = "mean", na.rm = TRUE)
#we get 5.617463

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.03420275

#aggregate to see our estimate population
estpop <- global(pop, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estpop <- estpop*0.0009
#we get 

#calculate error
accpop <- (estpop - truepop)/truepop
#we get 