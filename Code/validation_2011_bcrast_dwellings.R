##Technical Validation
##2011 Dwellings Raster

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

#harmonized dwellings data
dwl <- rast("Products/2011_bcrast_dwellings.nc")

## Step 3: Verify dwelling count and density

#dwellings should be 1,945,365
truedwl <- 1945365

#true area should be 922,509.29 km^2
truearea <- 922509.29

#true density should be 2.108776/km^2
trueden <- truedwl/truearea

#aggregate to see our estimate density
estden <- global(dwl, fun = "mean", na.rm = TRUE)
#we get 2.237391

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.06099059

#aggregate to see our estimate dwellings
estdwl <- global(dwl, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estdwl <- estdwl*0.0009
#we get 2053453

#calculate error
accdwl <- (estdwl - truedwl)/truedwl
#we get 0.05556168
