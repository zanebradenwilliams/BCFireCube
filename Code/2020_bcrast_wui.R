##BC Wildfire WUI Human Interface Buffer

##Initialization: Download data from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-wui-human-interface-buffer

##Store unzipped data in /BCFireCube/Data/WUI

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R
#2014_bcrast_dem.R

## Step 1: Call libraries

library(terra)
library(tidyverse)
library(sf)

## Step 2: Read data

#pull DEM data
dem <- rast("Products/2014_bcrast_dem.nc")

#pull extent data
ext <- vect("Products/2020_bcvect_extent.gpkg")

#pull raw PSTA WUI data
pstawui <- st_read("Data/WUI/PROT_WUI_HMN_INTRFCE_BUFFR_SP/PROTWUIHMN_polygon.shp")

## Step 3: Recode, reproject, and rasterize data

#generate binary code for WUI coverage
pstawui$wui <- 1L

#convert to SpatVector
pstawui <- pstawui %>% vect()

#reproject to EPSG:3979
pstawui <- pstawui %>% project("EPSG:3979")

#rasterize to mirror dem format
pstawui <- pstawui %>% rasterize(dem, field = "wui", touches = TRUE)

## Step 4: Crop and mask to terrestrial extent

#crop to extent:
pstawui <- pstawui %>% crop(ext)

#set background values of wui raster to 0
pstawui[is.na(pstawui)] <- 0L

#mask to extent
pstawui <- pstawui %>% mask(ext, touches = TRUE)

## Step 5: Save as NetCDF
writeCDF(pstawui, "Products/2020_bcrast_wui.nc", varname="wui", longname="Wildland Urban Interface Extent", unit = "code")
