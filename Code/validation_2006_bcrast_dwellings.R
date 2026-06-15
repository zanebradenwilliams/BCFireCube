##Technical Validation
##2006 Dwellings Raster

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

#harmonized dwellings data
dwl <- rast("Products/2006_bcrast_dwellings.nc")

## Step 3: Verify dwelling count and density

#dwellings should be 1,788,474
truedwl <- 1788474

#true area should be 924,815.43 km^2
truearea <- 924815.43

#true density should be 1.933871/km^2
trueden <- truedwl/truearea

#aggregate to see our estimate density
estden <- global(dwl, fun = "mean", na.rm = TRUE)
#we get 2.044451

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.0571807

#aggregate to see our estimate dwellings
estdwl <- global(dwl, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estdwl <- estdwl*0.0009
#we get 1875073

#calculate error
accdwl <- (estdwl - truedwl)/truedwl
#we get 0.04842055
