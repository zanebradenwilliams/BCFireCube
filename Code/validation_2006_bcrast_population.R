##Technical Validation
##2006 Population Raster

#Accurate counts are taken from https://www12.statcan.gc.ca/census-recensement/2006/dp-pd/prof/92-591/details/Page.cfm?Lang=E&Geo1=PR&Code1=59&Geo2=PR&Code2=01&Data=Count&SearchText=British%20Columbia&SearchType=Begins&SearchPR=01&B1=All&GeoLevel=PR&GeoCode=59

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
pop <- rast("Products/2006_bcrast_population.nc")

## Step 3: Verify population count and density

#population should be 4,113,487
truepop <- 4113487

#true area should be 924,815.43 km^2
truearea <- 924815.43

#true density should be 4.4479/km^2
trueden <- truepop/truearea

#aggregate to see our estimate density
estden <- global(pop, fun = "mean", na.rm = TRUE)
#we get 4.626738

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.04020718

#aggregate to see our estimate population
estpop <- global(pop, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estpop <- estpop*0.0009
#we get 4243422

#calculate error
accpop <- (estpop - truepop)/truepop
#we get 0.03158767