##Technical Validation
##2021 Dwellings Raster

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

#harmonized dwellings data
dwl <- rast("Products/2021_bcrast_dwellings.nc")

## Step 3: Verify dwelling count and density

#total dwellings should be 2,211,694
truedwl <- 2211694

#true area should be 920,686.55 km^2
truearea <- 920686.55

#true density should be 2.402223/km^2
trueden <- truedwl/truearea

#aggregate to see our estimate density
estden <- global(dwl, fun = "mean", na.rm = TRUE)
#we get 2.531822

#calculate error
accden <- (estden$mean - trueden)/trueden
#we get 0.05394982

#aggregate to see our estimate dwellings
estdwl <- global(dwl, fun = "sum", na.rm = TRUE)

#scale cell size to 1km
estdwl <- estdwl*0.0009
#we get 2,323,351

#calculate error
accdwl <- (estdwl - truedwl)/truedwl
#we get 0.05048469