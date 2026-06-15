##Technical Validation
##2016 Dwellings Raster

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

#harmonized dwellings data
dwl <- rast("Products/2016_bcrast_dwellings.nc")

## Step 3: Verify dwelling count and density

#dwellings should be 2,063,417
truedwl <- 2063417

#true area should be 922,503.01 km^2
truearea <- 922503.01

#true density should be 2.236759/km^2
trueden <- truedwl/truearea

#aggregate to see our estimate density
estden <- global(dwl, fun = "mean", na.rm = TRUE)
#we get 2.369803

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.05948074

#aggregate to see our estimate dwellings
estdwl <- global(dwl, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estdwl <- estdwl*0.0009
#we get 2174704

#calculate error
accdwl <- (estdwl - truedwl)/truedwl
#we get 0.05393329
